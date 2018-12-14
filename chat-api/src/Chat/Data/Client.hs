module Chat.Data.Client
  ( ClientId(..)
  , Clients
  , clientName
  , emptyClients
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS

newtype ClientId = ClientId Text
  deriving (Show, Eq, Ord)

clientName :: ClientId -> Text
clientName (ClientId name) = name

type Clients = Map ClientId WS.Connection

emptyClients :: Clients
emptyClients = Map.empty
