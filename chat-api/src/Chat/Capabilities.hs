{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Chat.Capabilities
  ( Hub
  , connectionCount
  , isConnected
  , connect
  , disconnect
  , broadcast
  ) where

import Chat.Data (ClientId)

class Monad m => Hub m c | m -> c where
  connectionCount :: m Int
  isConnected :: ClientId -> m Bool
  connect :: ClientId -> c -> m ()
  disconnect :: ClientId -> m ()
  broadcast :: ClientId -> Text -> m ()
