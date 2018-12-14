{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Chat.Capabilities
  ( Hub
  , connectionCount
  , isConnected
  , connect
  , disconnect
  , broadcast
  , accept
  , reject
  , start
  , join
  , leave
  ) where

import Chat.Data (AnnouncementError, ClientId)
import Prelude hiding (join)

class Monad m => Hub m c | m -> c where
  connectionCount :: m Int
  isConnected :: ClientId -> m Bool
  connect :: ClientId -> c -> m ()
  disconnect :: ClientId -> m ()
  broadcast :: ClientId -> Text -> m ()
  accept :: c -> ClientId -> m ()
  reject :: c -> AnnouncementError -> m ()
  start :: c -> ClientId -> m ()
  join :: c -> ClientId -> m ()
  leave :: ClientId -> m ()
