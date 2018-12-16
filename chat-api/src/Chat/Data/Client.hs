module Chat.Data.Client
  ( ClientId(..)
  , ClientMap
  , clientName
  , emptyClients
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype ClientId = ClientId Text
  deriving (Show, Eq, Ord)

clientName :: ClientId -> Text
clientName (ClientId name) = name

type ClientMap c = Map ClientId c

emptyClients :: ClientMap c
emptyClients = Map.empty
