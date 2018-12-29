module Chat.Capabiltiy.Hub
  ( class Hub
  , EventSources
  , Handlers
  , MessageHandler
  , CloseHandler
  , connect
  , disconnect
  , send
  ) where

import Prelude

import Chat.Data.Message (Message)
import Chat.Data.User (User)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Halogen (EventSource, SubscribeStatus)
import Text.Parsing.StringParser (ParseError)
import Web.Socket.Event.CloseEvent (CloseEvent)

type CloseHandler f = CloseEvent → Maybe (f SubscribeStatus)
type MessageHandler f = Either ParseError Message → Maybe (f SubscribeStatus)

type Handlers f =
  { onClose   ∷ CloseHandler f
  , onMessage ∷ MessageHandler f
  }

type EventSources f m =
  { onClose   ∷ EventSource f m
  , onMessage ∷ EventSource f m
  }

class Monad m <= Hub m where
  connect ∷ ∀ f. User → Handlers f → m (EventSources f m)
  disconnect ∷ m Unit
  send ∷ String → m Unit
