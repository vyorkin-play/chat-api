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

import Chat.Capability.Now (class Now)
import Chat.Data.Route (Route(..))
import Chat.Env (Env)
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Component.ChildPath as CP
import Data.Either.Nested (type (\/))
import Data.Functor.Coproduct.Nested (type (<\/>))
import Prelude.Unicode ((≢))
import Chat.Component.Welcome as Welcome
import Chat.Component.Room as Room
import Chat.Component.Contact as Contact

type State =
  { route ∷ Route
  }

data Query a
  = Navigate Route a

type Input  = Unit
type Output = Void

type WithCaps c m
  = MonadAff m
  ⇒ MonadAsk Env m
  ⇒ Now m
  ⇒ c m

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

type Component' m = H.Component HH.HTML Query Input Output m
type Component m  = WithCaps Component' m

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
    initialState = const { route: Welcome }

    eval ∷ Query ~> DSL m
    eval = case _ of
      Navigate dest a → a <$ do
        { route } ← H.get
        when (route ≢ dest) $
          H.modify_ _ { route = dest }

    render ∷ State → HTML m
    render { route } = case route of
      Welcome          → HH.slot' CP.cp1 Welcome.Slot Welcome.component unit absurd
      Room             → HH.slot' CP.cp2 Room.Slot Room.component unit absurd
      Contact username → HH.slot' CP.cp3 Contact.Slot Contact.component unit absurd
