module Chat.Form.Validation.Utils
  ( cond
  , toOptional
  ) where

import Prelude

import Chat.Form.Validation.Error (Error)
import Chat.Utils.Conditional ((?))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Formless (Validation(..), runValidation)
import Prelude.Unicode ((∘), (≡))

cond ∷ ∀ a. (a → Boolean) → Error → a → Either Error a
cond f err a = f a ? pure a $ Left err

toOptional
  ∷ ∀ form m a b
  . Monoid a
  ⇒ Eq a
  ⇒ Monad m
  ⇒ Validation form m Error a b
  → Validation form m Error a (Maybe b)
toOptional v = Validation \form val →
  case val ≡ mempty of
    true → pure (pure Nothing)
    _ → (map ∘ map) Just (runValidation v form val)
