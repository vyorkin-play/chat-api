module Chat.Form.Validation
  ( required
  , minLength
  ) where

import Prelude

import Chat.Form.Validation.Error (Error(..))
import Chat.Form.Validation.Utils (cond)
import Data.String as String
import Formless (Validation, hoistFnE_)
import Prelude.Unicode ((≢))

required ∷ ∀ form m a. Eq a ⇒ Monoid a ⇒ Monad m ⇒ Validation form m Error a a
required = hoistFnE_ $ cond (_ ≢ mempty) Required

minLength ∷ ∀ form m. Monad m ⇒ Int → Validation form m Error String String
minLength n = hoistFnE_ $ cond (\str → String.length str > n) TooShort
