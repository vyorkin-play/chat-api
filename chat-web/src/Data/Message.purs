module Chat.Data.Message
  ( Message(..)
  , parse
  , print
  ) where

import Prelude

import Chat.Data.User (User(..))
import Chat.Data.User as User
import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String.CodeUnits (singleton)
import Prelude.Unicode ((⊙))
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (alphaNum, string)
import Text.Parsing.StringParser.Combinators (many, (<?>))

data Message
  = Said User String
  | Joined User
  | Left User

derive instance genericMessage ∷ Generic Message _
derive instance eqMessage ∷ Eq Message
derive instance ordMessage ∷ Ord Message

instance showMessage ∷ Show Message where
  show = genericShow

print ∷ Message → String
print = case _ of
  Said user text → User.toString user <> ": " <> text
  Joined user    → "-> " <> User.toString user <> " joined"
  Left user      → "<- " <> User.toString user <> " left"

parse ∷ String → Either ParseError Message
parse = runParser parser

parser ∷ Parser Message
parser = announcement "-> " Joined "joined"
     <|> announcement "<- " Left "left"
     <|> message
     <?> "Error parsing message"

announcement ∷ String → (User → Message) → String → Parser Message
announcement prefix ctor action = do
  name ← string prefix *> token *> string action
  pure $ ctor (User name)

message ∷ Parser Message
message = do
  name ← token *> string ":"
  text ← token
  pure $ Said (User name) text

token ∷ Parser String
token = foldMap singleton ⊙ many alphaNum
