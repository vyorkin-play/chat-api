{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Main (main) where

import Chat.Capabilities
import Chat.Data (ClientId, ClientMap, emptyClients)
import Colog (HasLog, LogAction, Message, getLogAction, setLogAction)
import Control.Concurrent (modifyMVar_)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype MockHub a = MockHub
  { runMockHub :: ReaderT MockHubEnv IO a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader MockHubEnv
    )

data MockHubEnv = MockHubEnv
  { clients :: !(MVar Clients)
  }

type Clients = ClientMap MockConnection

data MockConnection = MockConnection
  { clientId :: ClientId
  , incoming :: [HubMsg]
  , messages :: [HubMsg]
  }

newMockConnection :: ClientId -> IO MockConnection
newMockConnection cid = do
  return $ MockConnection
    { clientId = cid
    , incoming = []
    , messages = []
    }

instance Applicative m => HasLog MockHubEnv Message m where
  getLogAction :: MockHubEnv -> LogAction m Message
  getLogAction = mempty

  setLogAction :: LogAction m Message -> MockHubEnv -> MockHubEnv
  setLogAction = const id

newMockHubEnv :: IO MockHubEnv
newMockHubEnv = do
  initialClients <- newMVar emptyClients
  return $ MockHubEnv
    { clients = initialClients
    }

readClients :: MockHub Clients
readClients = asks clients >>= readMVar

withClients :: (Clients -> Clients) -> MockHub ()
withClients f = do
  ref <- asks clients
  liftIO $ modifyMVar_ ref (return . f)

instance Hub MockHub MockConnection where
  connectionCount :: MockHub Int
  connectionCount = Map.size <$> readClients

  isConnected :: ClientId -> MockHub Bool
  isConnected cid = Map.member cid <$> readClients

  connect :: ClientId -> MockConnection -> MockHub ()
  connect cid = withClients . Map.insert cid

  disconnect :: ClientId -> MockHub ()
  disconnect = withClients . Map.delete

  send = undefined
  receive = undefined

  -- send :: MockConnection -> HubMsg -> MockHub ()
  -- send _ _ = return ()

  -- receive :: MockConnection -> MockHub HubMsg
  -- receive (MockConnection cid _) = do

main :: IO ()
main = return ()
