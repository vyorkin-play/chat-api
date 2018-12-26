module Chat.Capabiltiy.Hub
  ( class Hub
  , connect
  , disconnect
  , send
  , receive
  ) where

import Prelude

import Chat.Data.Message (Message)
import Chat.Data.User (User)
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import Prelude.Unicode ((∘))

class Monad m <= Hub m where
  connect ∷ User → m Unit
  disconnect ∷ m Unit
  send ∷ String → m Unit
  receive ∷ Message → m Unit

instance hubHalogenM ∷ Hub m ⇒ Hub (HalogenM s f g p o m) where
  connect = lift ∘ connect
  disconnect = lift disconnect
  send = lift ∘ send
  receive = lift ∘ receive
