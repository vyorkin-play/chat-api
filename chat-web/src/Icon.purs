module Chat.Icon
  ( Icon
  , logo
  , room
  , contact
  , msgLeft
  , msgRight
  ) where

import Chat.Assets (svg)
import Halogen.HTML (HTML, IProp)
import Svg.Parser.Halogen (icon)

type Icon = ∀ p r i. Array (IProp r i) → HTML p i

logo ∷ Icon
logo = icon svg.logo

room ∷ Icon
room = icon svg.room

contact ∷ Icon
contact = icon svg.contact

msgLeft ∷ Icon
msgLeft = icon svg.msgLeft

msgRight ∷ Icon
msgRight = icon svg.msgRight
