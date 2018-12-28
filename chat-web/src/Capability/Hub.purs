module Chat.Capabiltiy.Hub
  ( class Hub
  , connect
  , disconnect
  , send
  ) where

import Prelude

import Chat.Data.Connection as Connection
import Chat.Data.User (User)

class Monad m <= Hub m where
  connect ∷ Connection.Handler m → User → m Unit
  disconnect ∷ m Unit
  send ∷ String → m Unit

-- instance hubHalogenM ∷ Hub m ⇒ Hub (HalogenM s f g p o m) where
--   connect handler = lift ∘ connect handler
--   disconnect = lift disconnect
--   send = lift ∘ send
