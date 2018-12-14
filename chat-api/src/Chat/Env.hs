{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
module Chat.Env
  ( Env
  , port
  , logger
  , clients
  , newEnv
  ) where

import Chat.Data (Clients, emptyClients)
import Colog (HasLog, LogAction, Message, getLogAction, setLogAction)
import Control.Concurrent (MVar)

data Env m = Env
  { port    :: Int
  , logger  :: LogAction m Message
  , clients :: !(MVar Clients)
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = logger

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction logAction env = env { logger = logAction }

newEnv :: Int -> LogAction m Message -> IO (Env m)
newEnv portNumber logAction = do
  initialClients <- newMVar emptyClients
  return $ Env { port = portNumber
               , logger = logAction
               , clients = initialClients
               }
