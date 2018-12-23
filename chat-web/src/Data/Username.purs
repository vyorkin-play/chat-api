module Chat.Data.Username
  ( Username(..)
  , parse
  , toString
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)

newtype Username = Username String

derive instance newtypeBaseURL ∷ Newtype Username _
derive instance genericUsername ∷ Generic Username _
derive newtype instance eqUsername ∷ Eq Username
derive newtype instance ordUsername ∷ Ord Username

instance showUsername ∷ Show Username where
  show = genericShow

-- We'll use a smart constructor pattern to guarantee that usernames in the
-- system are never empty. We may want to provide even more validation later.

parse ∷ String → Maybe Username
parse "" = Nothing
parse s  = Just (Username s)

toString ∷ Username → String
toString = unwrap
