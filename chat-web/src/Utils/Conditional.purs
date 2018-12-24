module Chat.Utils.Conditional
  ( (?)
  , ifelse
  ) where

ifelse ∷ ∀ a. Boolean → a → a → a
ifelse p a b = if p then a else b

infixl 1 ifelse as ?
