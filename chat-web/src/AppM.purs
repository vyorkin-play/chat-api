-- | A custom application monad that provides concrete implementations for our
-- | abstract capabilities.

module Chat.AppM where

import Prelude

import Chat.Capability.Logging (class Logging, consoleLogger, logDebug, logError)
import Chat.Capability.Now (class Now)
import Chat.Capabiltiy.Hub (class Hub, disconnect, send)
import Chat.Capabiltiy.Navigation (class Navigation)
import Chat.Data.BaseURL as BaseURL
import Chat.Data.Connection (_Accepted, _Pending)
import Chat.Data.Connection as Connection
import Chat.Data.Log (Severity(..)) as Severity
import Chat.Data.Message (Message)
import Chat.Data.Message as Message
import Chat.Data.Route (print) as Route
import Chat.Data.User as User
import Chat.Env (Env)
import Chat.Env (LogLevel(..)) as LogLevel
import Chat.Web.Socket.WebSocket (toCloseReason, onOpen, onClose, onMessage) as WebSocket
import Control.Logger (cfilter, log) as Logger
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Array (elem)
import Data.Either (Either, either)
import Data.Foldable (for_)
import Data.Lens ((^?))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Now as Now
import Effect.Ref as Ref
import Halogen (eventSource, liftEffect)
import Halogen.Query.EventSource (eventSource')
import Prelude.Unicode ((∘))
import Routing.Hash (setHash) as Routing
import Text.Parsing.StringParser (ParseError)
import Type.Prelude (class TypeEquals, from)
import Web.Event.Event (Event)
import Web.Socket.Event.CloseEvent (CloseEvent)
import Web.Socket.Event.CloseEvent as CloseEvent
import Web.Socket.WebSocket (close, create, sendString) as WebSocket

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

launchAppM_ ∷ ∀ a. Env → AppM a → Effect Unit
launchAppM_ env = launchAff_ ∘ runAppM env

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
  connect user handlers = do
    env ← ask
    let url = BaseURL.toString env.baseUrl
    logDebug $ "Creating socket connection with " <> url
    liftEffect do
      socket ← WebSocket.create url []
      WebSocket.onOpen socket (mkOpenHandler env)
      WebSocket.onClose socket (mkCloseHanlder env)
      WebSocket.onMessage socket (mkMessageHandler env)
      Ref.write (Connection.Pending { socket, user }) env.status
      pure { onClose: eventSource (WebSocket.onClose socket) handlers.onClose
           , onMessage: eventSource (WebSocket.onMessage socket) handlers.onMessage
           }
    where
      mkOpenHandler ∷ Env → Event → Effect Unit
      mkOpenHandler env event = launchAppM_ env do
        logDebug "Connection opened"
        send $ "!hi!" <> User.toString user

      mkCloseHanlder ∷ Env → CloseEvent → Effect Unit
      mkCloseHanlder env event = launchAppM_ env do
        logDebug "Connection closed"
        liftEffect $ Ref.write (Connection.Closed reason) env.status
        where
        reason =
          if CloseEvent.wasClean event
            then Nothing
            else Just (WebSocket.toCloseReason event)

      mkMessageHandler ∷ Env → Either ParseError Message → Effect Unit
      mkMessageHandler env = launchAppM_ env ∘ either (logError ∘ show) handle
        where
        handle = case _ of
          Message.Accepted → do
            logDebug "Connection accepted, logged in"
            liftEffect do
              status ← Ref.read env.status
              for_ (status ^? _Pending) \c →
                Ref.write (Connection.Accepted c) env.status
          Message.Rejected err → do
            logDebug $ "Connection rejected: " <> err
            disconnect
          other →
            logDebug $ "Got message: " <> Message.print other

  disconnect = do
    env ← ask
    liftEffect do
      status ← Ref.read env.status
      for_ (status ^? (_Pending <> _Accepted)) \c →
        WebSocket.close c.socket
      Ref.write (Connection.Closed Nothing) env.status

  send text = do
    env ← ask
    liftEffect do
      status ← Ref.read env.status
      traceM $ "status: " <> show status
      for_ (status ^? (_Pending <> _Accepted)) \c →
        WebSocket.sendString c.socket text
