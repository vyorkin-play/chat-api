{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module Chat.Capabilities
  ( Hub
  , Connection
  , connectionCount
  , isConnected
  , connect
  , disconnect
  , broadcast
  ) where

import Chat.Data (ClientId)

class Monad m => Hub m where
  type Connection m :: *

  connectionCount :: m Int
  isConnected :: ClientId -> m Bool
  connect :: ClientId -> Connection m -> m ()
  disconnect :: ClientId -> m ()
  broadcast :: ClientId -> Text -> m ()
