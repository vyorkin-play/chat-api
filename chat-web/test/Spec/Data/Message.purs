module Test.Spec.Data.Message (spec) where

import Prelude

import Chat.Data.Message as Message
import Chat.Data.User (User(..))
import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (ParseError(..))

spec ∷ Spec Unit
spec = describe "Chat.Data.Message" do
  it "parses accepted/rejected responses" do
    let error = "User already exists"
    Message.parse ("!rejected!" <> error) `shouldEqual` (Right $ Message.Rejected error)
    Message.parse "!accepted!" `shouldEqual` (Right Message.Accepted)
    Message.parse "!whatever!" `shouldEqual` (Left $ ParseError "Error parsing message")

  it "parses announcements" do
    Message.parse "-> foo joined" `shouldEqual` (Right $ Message.Joined (User "foo"))
    Message.parse "<- foo left" `shouldEqual` (Right $ Message.Left (User "foo"))

  it "parses messages" do
    Message.parse "foo: Hello" `shouldEqual` (Right $ Message.Said (User "foo") "Hello")
    Message.parse "bar: Gute Nacht" `shouldEqual` (Right $ Message.Said (User "bar") "Gute Nacht")
