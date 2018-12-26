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
import Text.Parsing.StringParser (ParseError, Parser, runParser)
import Text.Parsing.StringParser.CodePoints (alphaNum, anyChar, string, whiteSpace)
import Text.Parsing.StringParser.Combinators (many, optional, (<?>))

data Message
  = Accepted
  | Rejected String
  | Said User String
  | Joined User
  | Left User

derive instance genericMessage ∷ Generic Message _
derive instance eqMessage ∷ Eq Message
derive instance ordMessage ∷ Ord Message

instance showMessage ∷ Show Message where
  show = genericShow

print ∷ Message → String
print = case _ of
  Accepted       → "!accepted!"
  Rejected error → "!rejected!" <> error
  Said user text → User.toString user <> ": " <> text
  Joined user    → "-> " <> User.toString user <> " joined"
  Left user      → "<- " <> User.toString user <> " left"

parse ∷ String → Either ParseError Message
parse = runParser parser

parser ∷ Parser Message
parser = accepted
     <|> rejected
     <|> announcement "-> " Joined "joined"
     <|> announcement "<- " Left "left"
     <|> message
     <?> "Error parsing message"

accepted ∷ Parser Message
accepted = do
  void $ string "!accepted!"
  pure Accepted

rejected ∷ Parser Message
rejected = do
  error ← string "!rejected!" *> anyString
  pure $ Rejected error

announcement ∷ String → (User → Message) → String → Parser Message
announcement prefix ctor action = do
  void $ string prefix
  optional whiteSpace
  name ← token
  optional whiteSpace
  void $ string action
  pure $ ctor (User name)

message ∷ Parser Message
message = do
  name ← token
  optional whiteSpace
  void $ string ":"
  optional whiteSpace
  text ← anyString
  pure $ Said (User name) text

token ∷ Parser String
token = many' alphaNum

anyString ∷ Parser String
anyString = many' anyChar

many' ∷ Parser Char → Parser String
many' p = foldMap singleton <$> many p
