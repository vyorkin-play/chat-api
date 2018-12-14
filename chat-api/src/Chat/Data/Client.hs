module Chat.Data.Client
  ( ClientId(..)
  , Clients
  , clientName
  , emptyClients
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype ClientId = ClientId Text
  deriving (Show, Eq, Ord)

clientName :: ClientId -> Text
clientName (ClientId name) = name

type Clients c = Map ClientId c

emptyClients :: Clients c
emptyClients = Map.empty
