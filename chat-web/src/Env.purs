module Chat.Env where

import Prelude

import Affjax (URL)
import Data.Newtype (class Newtype)

-- | A BaseURL newtype to have a meaningful type to pass around in parameters
-- | a more powerful type that does validation could be an improvement.
newtype BaseURL = BaseURL URL

derive instance newtypeBaseURL ∷ Newtype BaseURL _

-- | An application global log level.
data LogLevel = Dev | Prod

derive instance eqLogLevel ∷ Eq LogLevel
derive instance ordLogLevel ∷ Ord LogLevel

-- Our global environment will store read-only information available to any function
-- with the right MonadAsk constraint.

type Env =
  { baseUrl ∷ BaseURL
  , logLevel ∷ LogLevel
  }
