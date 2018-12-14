{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeFamilies               #-}
module Chat
  ( Env(..)
  , App(..)
  , runChat
  , chat
  ) where

import Chat.Capabilities
import Chat.Data (Clients, clientName)
import Chat.Data.Announcement as Announcement
import Chat.Env (Env (..))
import Colog (pattern I, Message, WithLog, log)
import Control.Concurrent (modifyMVar_)
import Control.Exception (finally)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import qualified Data.Map.Strict as Map
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
  raw <- liftIO $ WS.receiveData conn
  case Announcement.parse raw of
    Right cid -> accept conn cid
    Left err  -> reject conn err

instance Hub (App WS.Connection) WS.Connection where
  connectionCount = Map.size <$> readClients
  isConnected cid = Map.member cid <$> readClients

  connect cid = withClients . Map.insert cid
  disconnect  = withClients . Map.delete

  broadcast cid msg = do
    clients' <- readClients
    let others = Map.withoutKeys clients' (one cid)
    liftIO $ forM_ (Map.elems others) (`WS.sendTextData` msg)

  start conn cid =
    withRunInIO $ \io -> liftIO $ finally
      (io $ join conn cid)
      (io $ leave cid)

  accept conn cid = do
    connected <- isConnected cid
    if connected
      then liftIO sendAlreadyConnected
      else start conn cid
    where
      sendAlreadyConnected = WS.sendTextData conn
        ("client " <> show cid <> " is already connected" :: Text)

  reject conn err =
    let reason = Announcement.errorText err
    in liftIO $ WS.sendTextData conn reason

  join conn cid = do
    connect cid conn
    broadcast cid $ "-> " <> name <> " joined"
    forever $ do
      msg <- liftIO $ WS.receiveData conn
      broadcast cid $ name <> ": " <> msg
    where name = clientName cid

  leave cid = do
    broadcast cid $ "<- " <> clientName cid <> " left"
    disconnect cid

readClients :: App c (Clients c)
readClients = asks clients >>= readMVar

withClients :: (Clients c -> Clients c) -> App c ()
withClients f = do
  ref <- asks clients
  liftIO $ modifyMVar_ ref (return . f)
