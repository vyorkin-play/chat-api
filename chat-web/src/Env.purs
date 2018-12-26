module Chat.Env
  ( LogLevel(..)
  , Env
  , mkEnv
  , reset
  ) where

import Prelude

import Chat.Data.Message (Message)
import Chat.Data.User (User)
import Chat.Data.BaseURL (BaseURL(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Web.Socket.WebSocket (WebSocket)

-- | An application global log level.
data LogLevel = Dev | Prod

derive instance eqLogLevel ∷ Eq LogLevel
derive instance ordLogLevel ∷ Ord LogLevel

-- Our global environment will store
-- read-only information and mutable refs to global state
-- available to any function with the right MonadAsk constraint.

type Env =
  { baseUrl ∷ BaseURL
  , logLevel ∷ LogLevel
  , user ∷ Ref (Maybe User)
  , messages ∷ Ref (Array Message)
  , connection ∷ Ref (Maybe WebSocket)
  , isLoading ∷ Ref Boolean
  , error ∷ Ref (Maybe String)
  }

mkEnv ∷ BaseURL → LogLevel → Effect Env
mkEnv baseUrl logLevel = do
  user ← liftEffect $ Ref.new Nothing
  messages ← liftEffect $ Ref.new []
  connection ← liftEffect $ Ref.new Nothing
  isLoading ← liftEffect $ Ref.new false
  error ← liftEffect $ Ref.new Nothing
  pure { logLevel: Dev
       , baseUrl: BaseURL "ws://localhost:9160"
       , user
       , messages
       , connection
       , isLoading
       , error
       }

reset ∷ Env → Effect Unit
reset env = do
  Ref.write Nothing env.connection
  Ref.write Nothing env.user
  Ref.write false env.isLoading
  Ref.write Nothing env.error
