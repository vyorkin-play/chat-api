module Chat.Page.Contact
  ( Query(..)
  , Input
  , Slot(..)
  , component
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

data Query a = Unit a
type State = Unit

type Input  = Unit
type Output = Void

data Slot = Slot
derive instance eqSlot ∷ Eq Slot
derive instance ordSlot ∷ Ord Slot

type Component = H.Component HH.HTML Query Input Output

type DSL  = H.ComponentDSL State Query Output
type HTML = H.ComponentHTML Query

component ∷ ∀ m. Component m
component = H.component
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where
    eval ∷ Query ~> DSL m
    eval (Unit a) = pure a

    render ∷ State → HTML
    render _ =
      HH.div
      []
      [ HH.text "contact" ]
