module Chat.Page.Room
  ( Query(..)
  , Input
  , Output(..)
  , StateRep
  , Slot(..)
  , component
  ) where

import Prelude

import Chat.Capability.Logging (class Logging)
import Chat.Capabiltiy.Hub (class Hub)
import Chat.Component.HTML.Utils (css)
import Chat.Data.Message (Message)
import Chat.Page.Room.Message (message)
import Chat.Page.Room.MessageForm (MessageForm)
import Chat.Page.Room.MessageForm (render, validators, formProxy, Slot(..)) as MessageForm
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type StateRep r =
  { messages ∷ Array Message
  | r
  }

data Query a
  = Receive Input a
  | HandleForm (F.Message' MessageForm) a

type State = StateRep ()
type Input = StateRep ()

data Output = Send String

data Slot = Slot
derive instance eqSlot ∷ Eq Slot
derive instance ordSlot ∷ Ord Slot

type ChildQuery m = F.Query' MessageForm m
type ChildSlot = MessageForm.Slot

type WithCaps c m r
  = MonadAff m
  ⇒ Logging m
  ⇒ Hub m
  ⇒ c m

type Component' m = H.Component HH.HTML Query Input Output m
type Component  m r = WithCaps Component' m r

type DSL m  = H.ParentDSL State Query (ChildQuery m) ChildSlot Output m
type HTML m = H.ParentHTML Query (ChildQuery m) ChildSlot m

component ∷ ∀ m r. Component m r
component = H.parentComponent
  { initialState: identity
  , render
  , eval
  , receiver: HE.input Receive
  }
  where
    eval ∷ Query ~> DSL m
    eval = case _ of
      Receive { messages } a → do
        H.modify_ _ { messages = messages }
        pure a
      HandleForm msg a → case msg of
        F.Submitted outputs → do
          let
            fields = F.unwrapOutputFields outputs
          H.raise $ Send fields.text
          pure a
        _ →
          pure a

    render ∷ State → HTML m
    render state =
      HH.div
      [ css ["page-room"] ]
      [ HH.h4
        [ css ["page-room-title"] ]
        [ HH.text "chat room" ]
      , renderMessageForm
      , HH.ul
        [ css ["message-list"]
        ]
        (flip message [] <$> state.messages)
      ]

    renderMessageForm ∷ HTML m
    renderMessageForm = HH.slot
      MessageForm.Slot
      F.component
      { initialInputs: F.mkInputFields MessageForm.formProxy
      , validators: MessageForm.validators
      , render: MessageForm.render
      }
      (HE.input HandleForm)
