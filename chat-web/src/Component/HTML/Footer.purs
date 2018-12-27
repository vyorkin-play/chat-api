module Chat.Component.HTML.Footer
  ( footer
  ) where

import Prelude

import Chat.Component.HTML.Utils (css)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

footer ∷ ∀ i p. HH.HTML i p
footer =
  HH.footer
  [ css ["footer"] ]
  [ HH.text "something"
  ]
