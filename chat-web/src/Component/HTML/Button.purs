module Chat.Component.HTML.Button
  ( Button(..)
  , button
  , buttonGroup
  , buttonGroup_
  ) where

import Prelude

import Chat.Component.HTML.Utils (css)
import DOM.HTML.Indexed (HTMLbutton, HTMLdiv)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH

data Button
  = Default
  | Primary
  | Secondary

toClassName ∷ Button → String
toClassName = case _ of
  Default   → "button-default"
  Primary   → "button-primary"
  Secondary → "button-secondary"

button ∷ ∀ p i. Button → Array (IProp HTMLbutton i) → Array (HTML p i) → HTML p i
button x attrs = HH.button $ [ css ["button", toClassName x] ] <> attrs

buttonGroup_ ∷ ∀ p i. Array (HTML p i) → HTML p i
buttonGroup_ = buttonGroup []

buttonGroup ∷ ∀ p i. Array (IProp HTMLdiv i) → Array (HTML p i) → HTML p i
buttonGroup attrs = HH.div $ [ css ["button-group"] ] <> attrs
