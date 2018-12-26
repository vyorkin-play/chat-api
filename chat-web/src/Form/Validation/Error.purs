module Chat.Form.Validation.Error
  ( Error(..)
  , toString
  ) where

data Error
  = Required
  | TooShort

toString ∷ Error → String
toString = case _ of
  Required → "This field is required"
  TooShort → "Not enought characters entered"
