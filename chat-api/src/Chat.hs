{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
module Chat
  ( Env(..)
  , App(..)
  , runChat
  , chat
  , module Chat.Capabilities
  , module Chat.Data
  , module Chat.Env
  ) where

import Chat.Capabilities
import Chat.Data (ClientId, ClientMap, clientName)
import Chat.Data.Announcement as Announcement
import Chat.Env (Env (..))
import Colog (pattern D, pattern I, Message, WithLog, log)
import Control.Concurrent (modifyMVar_)
import Control.Exception (finally)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Network.WebSockets as WS
import Prelude hiding (join)

newtype App c a = App
  { runApp :: ReaderT (Env (App c) c) IO a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader (Env (App c) c)
    )

runChat :: Env (App c) c -> App c a -> IO a
runChat env = usingReaderT env . runApp

readClients :: (MonadReader (Env (App c) c) m, MonadIO m) => m (ClientMap c)
readClients = asks clients >>= readMVar

withClients :: (ClientMap c -> ClientMap c) -> App c ()
withClients f = do
  ref <- asks clients
  liftIO $ modifyMVar_ ref (return . f)

-- Synonym for constraints commonly
-- satisfied by monads used in stack.
type Chat m c =
  ( WithLog (Env (App c) c) Message m
  , MonadIO m
  , MonadUnliftIO m
  , Hub m c
  )

-- | Run chat server.
chat :: Chat m WS.Connection => m ()
chat = do
  portNum <- asks port
  log I $ "listening on port " <> show portNum <> "..."
  withRunInIO $ \io -> liftIO $
    WS.runServer "127.0.0.1" portNum (io . loop)

loop :: Chat m WS.Connection => WS.PendingConnection-> m ()
loop pending = do
  conn <- liftIO $ WS.acceptRequest pending
  liftIO $ WS.forkPingThread conn 30
  msg <- receive conn
  case Announcement.parse msg of
    Right cid -> do
      connected <- isConnected cid
      if connected
        then reject conn (Announcement.AlreadyConnected cid)
        else accept conn cid
    Left err ->
      reject conn err

instance Hub (App WS.Connection) WS.Connection where
  connectionCount = Map.size <$> readClients
  isConnected cid = Map.member cid <$> readClients

  connect cid = withClients . Map.insert cid
  disconnect  = withClients . Map.delete

  send c  = liftIO . WS.sendTextData c
  receive = liftIO . WS.receiveData

broadcast :: Chat m c => ClientId -> HubMsg -> m ()
broadcast cid msg = do
  log D msg
  clients' <- readClients
  let others = Map.withoutKeys clients' (one cid)
  forM_ (Map.elems others) (`send` msg)

start :: Chat m c => c -> ClientId -> m ()
start conn cid = do
  log D $ "Starting a new chat session for " <> show cid
  withRunInIO $ \io -> liftIO $ finally
    (io $ join conn cid)
    (io $ leave cid)

accept :: Chat m c => c -> ClientId -> m ()
accept conn cid = do
  send conn $ Announcement.responseText Accept
  start conn cid

reject :: Chat m c => c -> Announcement.Error -> m ()
reject conn err = do
  log D $ "Rejecting connection because: " <> Announcement.errorText err
  send conn $ Announcement.responseText (Announcement.Reject err)

join :: Chat m c => c -> ClientId -> m ()
join conn cid = do
  connect cid conn
  broadcast cid $ "-> " <> name <> " joined"
  log D $ show cid <> " joined"
  forever $ do
    msg <- receive conn
    when (not $ Text.isPrefixOf "!" msg) $
      broadcast cid $ name <> ": " <> msg
  where
    name = clientName cid

leave :: Chat m c => ClientId -> m ()
leave cid = do
  broadcast cid $ "<- " <> clientName cid <> " left"
  disconnect cid
  log D $ show cid <> " left"
