module Chat.Page.Welcome
  ( Query(..)
  , Input
  , Slot(..)
  , component
  ) where

import Prelude

import Chat.Assets as Assets
import Chat.Component.HTML.Utils (css)
import Chat.Component.Raw (raw)
import Chat.Component.Raw as Raw
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

data Query a
  = Initialize a

type State = Unit

type Input  = Unit
type Output = Void

data Slot = Slot
derive instance eqSlot ∷ Eq Slot
derive instance ordSlot ∷ Ord Slot

type ChildQuery = Raw.Query
type ChildSlot  = Raw.Slot

type Component = H.Component HH.HTML Query Input Output

type DSL m  = H.ParentDSL State Query ChildQuery ChildSlot Output m
type HTML m = H.ParentHTML Query ChildQuery ChildSlot m

component ∷ ∀ m. MonadAff m ⇒ Component m
component = H.lifecycleParentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just $ H.action Initialize
  , finalizer: Nothing
  }
  where
    initialState ∷ Input → State
    initialState = const unit

    eval ∷ Query ~> DSL m
    eval (Initialize a) = pure a

    render ∷ State → HTML m
    render state =
      HH.div_
      [ raw [ css ["logo"] ] Assets.svg.logo ]
