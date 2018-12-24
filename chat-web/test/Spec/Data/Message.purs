module Test.Spec.Data.Message (spec) where

import Prelude

import Chat.Data.Message (Message(..))
import Chat.Data.Message as Message
import Chat.Data.User (User(..))
import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Chat.Data.Message" do
  let
    joined1 = "<- foo joined"
    joined2 = "-> foo left"

    msg1 = "foo: Hello"
    msg2 = "bar: Gute Nacht"

  it "parses announcements" do
    Message.parse joined1 `shouldEqual` (Right $ Joined (User "foo"))
    pure unit

  it "parses messages" do
    pure unit
