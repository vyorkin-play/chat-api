{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Chat.Capabilities
  ( Hub
  , HubMsg
  , connectionCount
  , isConnected
  , connect
  , disconnect
  , send
  , receive
  ) where

import Chat.Data (ClientId)

type HubMsg = Text

class Monad m => Hub m c | m -> c where
  connectionCount :: m Int
  isConnected :: ClientId -> m Bool
  connect :: ClientId -> c -> m ()
  disconnect :: ClientId -> m ()
  send :: c -> HubMsg -> m ()
  receive :: c -> m HubMsg
