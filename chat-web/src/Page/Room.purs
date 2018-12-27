module Chat.Page.Room
  ( Query(..)
  , Input
  , Slot(..)
  , component
  ) where

import Prelude

import Chat.Capability.Logging (class Logging)
import Chat.Capabiltiy.Hub (class Hub)
import Chat.Component.HTML.Utils (css)
import Chat.Data.Message (Message)
import Chat.Page.Room.Message (message)
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.HTML as HH

data Query a = Unit a
type State = Unit

type Input  = Unit
type Output = Void

data Slot = Slot
derive instance eqSlot ∷ Eq Slot
derive instance ordSlot ∷ Ord Slot

type WithCaps c m r
  = MonadAff m
  ⇒ MonadAsk { messages ∷ Ref (Array Message) | r } m
  ⇒ Logging m
  ⇒ Hub m
  ⇒ c m

type Component' m = H.Component HH.HTML Query Input Output m
type Component  m r = WithCaps Component' m r

type DSL  = H.ComponentDSL State Query Output
type HTML = H.ComponentHTML Query

component ∷ ∀ m r. Component m r
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
      [ css ["page-room"] ]
      [ HH.h4
        [ css ["page-room-title"] ]
        [ HH.text "chat room" ]
      , HH.div
        [ css ["input-message"] ]
        [ HH.input
          [ css ["input form-field-input"]
          ]
        ]
      , HH.ul
        [ css ["message-list"]
        ]
        []
      ]
