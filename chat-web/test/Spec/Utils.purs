module Test.Spec.Utils
  ( shouldParse
  , shouldNotParse
  , shouldParseAs
  ) where

import Prelude

import Data.Either (Either(..), isLeft, isRight)
import Effect.Aff (Aff)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (Parser, runParser)

shouldParse ∷ ∀ a. Parser a → String → Aff Unit
shouldParse p i = isRight (runParser p i) `shouldEqual` true

shouldNotParse ∷ ∀ a. Parser a → String → Aff Unit
shouldNotParse p i = isLeft (runParser p i) `shouldEqual` true

shouldParseAs ∷ ∀ a. Show a ⇒ Eq a ⇒ a → Parser a → String → Aff Unit
shouldParseAs r p i = runParser p i `shouldEqual` Right r
