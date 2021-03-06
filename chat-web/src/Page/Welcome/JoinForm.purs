module Chat.Page.Welcome.JoinForm
  ( JoinForm(..)
  , Slot(..)
  , render
  , validators
  , formProxy
  ) where

import Prelude

import Chat.Component.HTML.Utils (css)
import Chat.Form.Field (submit)
import Chat.Form.Field as Field
import Chat.Form.Validation (required, minLength) as Validation
import Chat.Form.Validation.Error (Error) as Validation
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude.Unicode ((⋙))

newtype JoinForm r f = JoinForm
  (r (name ∷ f Validation.Error String String))

derive instance newtypeJoinForm ∷ Newtype (JoinForm r f) _

data Slot = Slot
derive instance eqSlot ∷ Eq Slot
derive instance ordSlot ∷ Ord Slot

formProxy ∷ F.FormProxy JoinForm
formProxy = F.FormProxy

proxies ∷ F.SProxies JoinForm
proxies = F.mkSProxies formProxy

validators ∷ ∀ form m. Monad m ⇒ JoinForm Record (F.Validation form m)
validators = JoinForm
  { name: Validation.required ⋙ Validation.minLength 3
  }

render
  ∷ ∀ m
  . MonadAff m
  ⇒ F.State JoinForm m
  → F.HTML' JoinForm m
render state =
  HH.form
  [ css ["form-join"] ]
  [ HH.fieldset
    [ css ["form-join-fields"] ]
    [ name
    , submit
      [ css ["button", "button-join"] ]
      "join"
    ]
  ]
  where
    name = Field.input proxies.name state.form
      [ HP.placeholder "what's your name?"
      , HP.type_ HP.InputText
      ]
