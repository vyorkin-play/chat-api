module Chat.Data.BaseURL
  ( BaseURL(..)
  , toString
  ) where

import Affjax (URL)
import Data.Newtype (class Newtype, unwrap)

-- | A BaseURL newtype to have a meaningful type to pass around in parameters
-- | a more powerful type that does validation could be an improvement.
newtype BaseURL = BaseURL URL

derive instance newtypeBaseURL ∷ Newtype BaseURL _

toString ∷ BaseURL → String
toString = unwrap
