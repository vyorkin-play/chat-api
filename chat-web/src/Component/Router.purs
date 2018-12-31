module Chat.Component.Router
  ( State
  , Query(..)
  , Input
  , Output
  , WithCaps
  , Component'
  , Component
  , component
  ) where

import Prelude

import Chat.Capability.Logging (class Logging)
import Chat.Capability.Now (class Now)
import Chat.Capabiltiy.Hub (class Hub, connect, send)
import Chat.Capabiltiy.Navigation (class Navigation, navigate)
import Chat.Component.HTML.Footer (footer)
import Chat.Component.HTML.Header (header)
import Chat.Component.HTML.Utils (css)
import Chat.Data.Connection (_Accepted)
import Chat.Data.Message (Message)
import Chat.Data.Message as Message
import Chat.Data.Route (Route(..)) as Route
import Chat.Data.Route (Route)
import Chat.Env (Env)
import Chat.Page.Contact as Contact
import Chat.Page.Room as Room
import Chat.Page.Welcome as Welcome
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array ((:))
import Data.Const (Const)
import Data.Either (Either)
import Data.Either.Nested (type (\/))
import Data.Foldable (for_)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Lens ((^?))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude.Unicode ((∘), (≢))
import Text.Parsing.StringParser (ParseError)
import Web.Socket.Event.CloseEvent (CloseEvent) as WebSocket

type State =
  { route ∷ Route
  , messages ∷ Array Message
  }

data Query a
  = Navigate Route a
  | HandleWelcome Welcome.Output a
  | HandleRoom Room.Output a
  | HandleClose WebSocket.CloseEvent (H.SubscribeStatus → a)
  | HandleMessage (Either ParseError Message) (H.SubscribeStatus → a)

type Input  = Unit
type Output = Void

-- | Query algebra for direct children of the router component,
-- | represented as a `Coproduct`.
type ChildQuery
  = Welcome.Query
  <\/> Room.Query
  <\/> Contact.Query
  <\/> Const Void

-- | Slot type for child components.
type ChildSlot
  = Welcome.Slot
  \/ Room.Slot
  \/ Contact.Slot
  \/ Void

-- A sort of continuation-passing style at the type level
type WithCaps c m
  = MonadAff m
  ⇒ MonadAsk Env m
  ⇒ Now m
  ⇒ Logging m
  ⇒ Navigation m
  ⇒ Hub m
  ⇒ c m

type Component' m = H.Component HH.HTML Query Input Output m
type Component  m = WithCaps Component' m

type DSL m  = H.ParentDSL State Query ChildQuery ChildSlot Output m
type HTML m = H.ParentHTML Query ChildQuery ChildSlot m

component ∷ ∀ m. Component m
component = H.parentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where
    initialState ∷ Input → State
    initialState = const
      { route: Route.Welcome
      , messages: []
      }

    eval ∷ Query ~> DSL m
    eval = case _ of
      Navigate dest a → a <$ do
        { route } ← H.get
        when (route ≢ dest) $
          H.modify_ _ { route = dest }
      HandleWelcome msg a → a <$ case msg of
        Welcome.Join user → do
          sources ← H.lift $ connect user
            { onClose: Just ∘ H.request ∘ HandleClose
            , onMessage: Just ∘ H.request ∘ HandleMessage
            }
          H.subscribe sources.onMessage
          H.subscribe sources.onClose
      HandleRoom msg a → a <$ case msg of
        Room.Send text → do
          env ← ask
          { messages } ← H.get
          status ← liftEffect $ Ref.read env.status
          for_ (status ^? _Accepted) \{ user } → do
            H.lift $ send text
            let message = Message.Said user text
            H.modify_ \s → s { messages = message : messages }
      HandleClose event reply → do
        pure $ reply H.Done
      HandleMessage event reply → do
        { messages } ← H.get
        for_ event $ case _ of
          Message.Accepted →
            navigate Route.Room
          Message.Rejected _ →
            navigate Route.Welcome
          msg →
            H.modify_ \s → s { messages = msg : messages }
        pure $ reply H.Listening

    render ∷ State → HTML m
    render state =
      HH.div
      [ css ["app"] ]
      [ HH.div
        [ css ["content"] ]
        [ header { user: Nothing, route: state.route }
        , renderPage state
        ]
      , footer
      ]

    renderPage ∷ State → HTML m
    renderPage state = case state.route of
      Route.Welcome →
        HH.slot' CP.cp1 Welcome.Slot Welcome.component unit (HE.input HandleWelcome)
      Route.Room →
        HH.slot' CP.cp2 Room.Slot Room.component { messages: state.messages } (HE.input HandleRoom)
      Route.Contact username →
        HH.slot' CP.cp3 Contact.Slot Contact.component unit absurd
