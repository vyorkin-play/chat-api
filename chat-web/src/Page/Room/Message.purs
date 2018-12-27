module Chat.Page.Room.Message
  ( message
  ) where

import Prelude
import Chat.Component.HTML.Utils (css)
import Chat.Data.Message (Message)
import Chat.Data.Message as Message
import DOM.HTML.Indexed (HTMLli)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH

messageClassName ∷ Message → String
messageClassName = case _ of
  Message.Said _ _ → "message-said"
  Message.Joined _ → "message-joined"
  Message.Left _   → "message-left"
  _                → ""

message ∷ ∀ p i. Message → Array (IProp HTMLli i) → HTML p i
message msg attrs =
  HH.li
  ([ css ["message", messageClassName msg] ] <> attrs)
  [ HH.text $ Message.print msg ]
