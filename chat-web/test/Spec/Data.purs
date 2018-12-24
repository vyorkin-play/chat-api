module Test.Spec.Data (spec) where

import Prelude

import Test.Spec (Spec)
import Test.Spec.Data.Message as Message

spec ∷ Spec Unit
spec = Message.spec
