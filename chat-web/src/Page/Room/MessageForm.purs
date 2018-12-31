module Chat.Page.Room.MessageForm where

import Prelude

import Chat.Component.HTML.Utils (css)
import Chat.Form.Field (submit)
import Chat.Form.Field as Field
import Chat.Form.Validation (required) as Validation
import Chat.Form.Validation.Error (Error) as Validation
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

newtype MessageForm r f = MessageForm
  (r (text ∷ f Validation.Error String String))

derive instance newtypeMessageForm ∷ Newtype (MessageForm r f) _

data Slot = Slot
derive instance eqSlot ∷ Eq Slot
derive instance ordSlot ∷ Ord Slot

formProxy ∷ F.FormProxy MessageForm
formProxy = F.FormProxy

proxies ∷ F.SProxies MessageForm
proxies = F.mkSProxies formProxy

validators ∷ ∀ form m. Monad m ⇒ MessageForm Record (F.Validation form m)
validators = MessageForm { text: Validation.required }

render
  ∷ ∀ m
  . MonadAff m
  ⇒ F.State MessageForm m
  → F.HTML' MessageForm m
render state =
  HH.form
  [ css ["form-message"] ]
  [ HH.fieldset
    [ css ["form-message-fields"] ]
    [ message
    , submit
      [ css ["button", "button-send"] ]
      "send"
    ]
  ]
  where
    message = Field.input proxies.text state.form
      [ HP.placeholder "your message"
      , HP.type_ HP.InputText
      ]
