module Chat.Data.Connection
  ( Handler
  ) where

import Prelude
import Chat.Data.Message (Message)

type Handler m =
  { onAccepted ∷ m Unit
  , onRejected ∷ m Unit
  , onMessage ∷ Message → m Unit
  }
