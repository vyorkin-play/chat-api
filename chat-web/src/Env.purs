module Chat.Env
  ( LogLevel(..)
  , Env
  , mkEnv
  ) where

import Prelude

import Chat.Data.BaseURL (BaseURL(..))
import Chat.Data.Connection as Connection
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (liftEffect)

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
  , status ∷ Ref Connection.Status
  }

mkEnv ∷ BaseURL → LogLevel → Effect Env
mkEnv baseUrl logLevel = do
  liftEffect do
    status ← Ref.new $ Connection.Closed Nothing
    pure { logLevel: Dev
         , baseUrl: BaseURL "ws://localhost:9160"
         , status
         }
