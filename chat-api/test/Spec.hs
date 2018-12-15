{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Main (main) where

import Chat.Capabilities
import Chat.Data (ClientId, Clients)
import Control.Concurrent (modifyMVar_)
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
  { pending :: !(MVar HubMsg)
  , clients :: !(MVar MockHubClients)
  }

readClients :: MockHub MockHubClients
readClients = asks clients >>= readMVar

withClients :: (MockHubClients -> MockHubClients) -> MockHub ()
withClients f = do
  ref <- asks clients
  liftIO $ modifyMVar_ ref (return . f)

readPending :: MockHub HubMsg
readPending = asks pending >>= readMVar

putPending :: HubMsg -> MockHub ()
putPending msg = asks pending >>= flip putMVar msg

type MockHubClients = Clients [HubMsg]

instance Hub MockHub [HubMsg] where
  connectionCount :: MockHub Int
  connectionCount = Map.size <$> readClients

  isConnected :: ClientId -> MockHub Bool
  isConnected cid = Map.member cid <$> readClients

  connect :: ClientId -> [HubMsg] -> MockHub ()
  connect cid = withClients . Map.insert cid

  disconnect :: ClientId -> MockHub ()
  disconnect  = withClients . Map.delete

  broadcast :: ClientId -> HubMsg -> MockHub ()
  broadcast cid msg = do
    clients' <- readClients
    let others = Map.withoutKeys clients' (one cid)
    forM_ (Map.elems others) (return . (:) msg)

  send :: [HubMsg] -> HubMsg -> MockHub ()
  send _ _ = return ()

  receive :: [HubMsg] -> MockHub HubMsg
  receive _ = readPending

main :: IO ()
main = return ()
