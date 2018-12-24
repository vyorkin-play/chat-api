module Chat.Assets
  ( SvgAssets
  , svg
  ) where

type SvgAssets =
  { logo ∷ String
  , room ∷ String
  , contact ∷ String
  , msgLeft ∷ String
  , msgRight ∷ String
  }

foreign import svg ∷ SvgAssets
