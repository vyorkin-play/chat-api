module Chat.Data.Connection
  ( Connection
  , Status(..)
  , _Closed
  , _Pending
  , _Accepted
  , _Rejected
  ) where

import Prelude

import Chat.Data.User (User)
import Chat.Data.User as User
import Chat.Web.Socket.WebSocket (CloseReason(..))
import Data.Generic.Rep (class Generic)
import Data.Lens (Prism')
import Data.Maybe (Maybe(..))
import Generic.Optic (_Ctor')
import Type.Prelude (SProxy(..))
import Web.Socket.WebSocket (WebSocket)

type Connection =
  { socket ∷ WebSocket
  , user ∷ User
  }

data Status
  = Closed (Maybe CloseReason)
  | Pending Connection
  | Accepted Connection
  | Rejected String

derive instance genericStatus ∷ Generic Status _

instance showStatus ∷ Show Status where
  show = case _ of
    Closed Nothing →
      "Closed"
    Closed (Just (CloseReason code reason)) →
      "Closed: (" <> show code <> ") " <> reason
    Pending { user } →
      "Pending: " <> User.toString user
    Accepted { user } →
      "Accepted: " <> User.toString user
    Rejected error →
      "Rejected: " <> error


-- Prisms

_Closed ∷ Prism' Status (Maybe CloseReason)
_Closed = _Ctor' (SProxy ∷ SProxy "Closed")

_Pending ∷ Prism' Status Connection
_Pending = _Ctor' (SProxy ∷ SProxy "Pending")

_Accepted ∷ Prism' Status Connection
_Accepted = _Ctor' (SProxy ∷ SProxy "Accepted")

_Rejected ∷ Prism' Status String
_Rejected = _Ctor' (SProxy ∷ SProxy "Rejected")
