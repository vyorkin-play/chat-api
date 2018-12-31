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

ball ∷ Icon
ball = icon svg.ball

balls ∷ Icon
balls = icon svg.balls

confetti ∷ Icon
confetti = icon svg.confetti

fireworks ∷ Icon
fireworks = icon svg.fireworks

gift1 ∷ Icon
gift1 = icon svg.gift1

gift2 ∷ Icon
gift2 = icon svg.gift1

gift3 ∷ Icon
gift3 = icon svg.gift1

gift4 ∷ Icon
gift4 = icon svg.gift1

gift5 ∷ Icon
gift5 = icon svg.gift1

gift6 ∷ Icon
gift6 = icon svg.gift1

hat ∷ Icon
hat = icon svg.hat

santa1 ∷ Icon
santa1 = icon svg.santa1

santa2 ∷ Icon
santa2 = icon svg.santa2

sweet ∷ Icon
sweet = icon svg.sweet

tree1 ∷ Icon
tree1 = icon svg.tree1

tree2 ∷ Icon
tree2 = icon svg.tree2
