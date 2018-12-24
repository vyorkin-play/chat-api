module Chat.Data.User
  ( User(..)
  , parse
  , toString
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)

newtype User = User String

derive instance newtypeBaseURL ∷ Newtype User _
derive instance genericUser ∷ Generic User _
derive newtype instance eqUser ∷ Eq User
derive newtype instance ordUser ∷ Ord User

instance showUser ∷ Show User where
  show = genericShow

-- We'll use a smart constructor pattern to guarantee that users in the
-- system always have name. We may want to provide even more validation later.

parse ∷ String → Maybe User
parse "" = Nothing
parse s  = Just (User s)

toString ∷ User → String
toString = unwrap
