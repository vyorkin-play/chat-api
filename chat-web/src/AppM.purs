-- | A custom application monad that provides concrete implementations for our
-- | abstract capabilities.

module Chat.AppM where

import Prelude

import Chat.Capability.Logging (class Logging, consoleLogger, logDebug)
import Chat.Capability.Now (class Now)
import Chat.Capabiltiy.Hub (class Hub, receive, send)
import Chat.Capabiltiy.Navigation (class Navigation)
import Chat.Data.BaseURL as BaseURL
import Chat.Data.Log (Severity(..)) as Severity
import Chat.Data.Message (Message)
import Chat.Data.Message as Message
import Chat.Data.Route (print) as Route
import Chat.Data.User as User
import Chat.Env (Env)
import Chat.Env (LogLevel(..)) as LogLevel
import Chat.Env as Env
import Control.Coroutine (Producer)
import Control.Coroutine.Aff as Coroutine
import Control.Logger (cfilter, log) as Logger
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Control.MonadZero (class MonadZero, guard)
import Data.Array (elem, (:))
import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Now as Now
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (F, Foreign, unsafeToForeign)
import Foreign as Foreign
import Halogen (liftEffect)
import Halogen as H
import Prelude.Unicode ((∘))
import Routing.Hash (setHash) as Routing
import Text.Parsing.StringParser (ParseError(..))
import Type.Prelude (class TypeEquals, from)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)
import Web.Event.EventTarget as EventTarget
import Web.Socket.Event.EventTypes as EventTypes
import Web.Socket.Event.MessageEvent as MessageEvent
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WebSocket

-- Our application monad will be the base of a `ReaderT` transformer.
-- We gain the functionality of `ReaderT` in addition to any other instances we write.
-- Our instances will provide the implementations for our capabilities.

newtype AppM a = AppM (ReaderT Env Aff a)

-- We'll write the instances below, but first,
-- we'll write a function that will recover `Aff` from our custom type.
-- Halogen applications must run in `Aff`, so this is a necessary step.
-- See its use in `Main.purs` for more.

runAppM ∷ Env → AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

-- We can  derive all the instances we need to make
-- this new type a monad and allow the use of `Effect` and `Aff`.

derive instance newtypeAppM ∷ Newtype (AppM a) _

derive newtype instance functorAppM ∷ Functor AppM
derive newtype instance applyAppM ∷ Apply AppM
derive newtype instance applicativeAppM ∷ Applicative AppM
derive newtype instance bindAppM ∷ Bind AppM
derive newtype instance monadAppM ∷ Monad AppM
derive newtype instance monadEffectAppM ∷ MonadEffect AppM
derive newtype instance monadAffAppM ∷ MonadAff AppM

-- This instance has to be defined manually, as we are using a type synonym
-- rather than a newtype for our environment.

-- from ∷ e → Env
-- ask ∷ AppM e
-- asks ∷ MonadAsk e AppEnv ⇒ (e → Env) → AppM Env

instance monadAskAppM ∷ TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

-- Next, we'll define instances for each of our capabilities.

-- We'll rely on the Effect instances to fetch date times,
-- so this instance is trivial.
-- However, relying on this instance means we can easily swap in another time source.
-- Our tests will rely on static values, for example.

instance nowAppM ∷ Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

-- We'll log everything in development,
-- but we'll only log warnings and errors in production.

instance loggingAppM ∷ Logging AppM where
  logMessage msg = do
    level ← asks _.logLevel
    Logger.log (consoleLogger # filter level) msg
    where
      filter LogLevel.Dev  = identity
      filter LogLevel.Prod = Logger.cfilter isImportant
      isImportant e = elem e.severity [Severity.Warning, Severity.Error]

-- The root of our application is watching for hash changes,
-- so to route from location to location we just need to set the hash.
-- Logging out is more involved; we need to invalidate the session.

instance navigationAppM ∷ Navigation AppM where
  navigate = liftEffect ∘ Routing.setHash ∘ Route.print

instance hubAppM ∷ Hub AppM where
  connect me = do
    env ← ask
    let url = BaseURL.toString env.baseUrl
    logDebug $ "Creating socket connection with " <> url
    liftEffect do
      socket ← WebSocket.create url []
      listener ← EventTarget.eventListener \_ →
        launchAff_ $ runAppM env (send ("!hi!" <> User.toString me) *> listen)
      Ref.write (Just socket) env.connection
      Ref.write (Just me) env.user
      Ref.write true env.isLoading
      let target = WebSocket.toEventTarget socket
      EventTarget.addEventListener EventTypes.onOpen listener false target

  disconnect = do
    env ← ask
    liftEffect do
      socket ← Ref.read env.connection
      for_ socket WebSocket.close
      Env.reset env

  send text = do
    env ← ask
    liftEffect do
      user ← Ref.read env.user
      connection ← Ref.read env.connection
      for_ connection $ flip WebSocket.sendString text

  receive = case _ of
    Message.Accepted → do
      logDebug "Connection accepted, logged in"
      { isLoading } ← ask
      liftEffect $ Ref.write false isLoading
    Message.Rejected err → do
      logDebug $ "Connection rejected: " <> err
      -- env ← ask
      -- liftEffect do
      --   Ref.write (Just err) env.error
      --   socket ← Ref.read env.connection
        -- for_ socket WebSocket.close
        -- Env.reset env
    msg → do
      logDebug $ "Got message: " <> Message.print msg
      { messages } ← ask
      liftEffect $ Ref.modify_ ((:) msg) messages

listen
  ∷ ∀ m
  . MonadEffect m
  ⇒ MonadAsk Env m
  ⇒ Hub m
  ⇒ m Unit
listen = do
  env ← ask
  liftEffect do
    listener ← EventTarget.eventListener \event →
      for_ (MessageEvent.fromEvent event) \msgEvent →
        launchAff_ $ runAppM env do
          let
            msgData = MessageEvent.data_ msgEvent
            msgRaw  = readHelper Foreign.readString msgData
          for_ msgRaw \msg →
            case Message.parse msg of
              Left (ParseError err) →
                liftEffect $ Ref.write (Just err) env.error
              Right message →
                receive message
    socket ← Ref.read env.connection
    for_ socket (addListener listener)
  where
    addListener listener sock = EventTarget.addEventListener
      EventTypes.onMessage listener false (WebSocket.toEventTarget sock)

    readHelper ∷ ∀ a b. (Foreign → F a) → b → Maybe a
    readHelper read = either (const Nothing) Just ∘ runExcept ∘ read ∘ unsafeToForeign
