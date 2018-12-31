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
  , ball ∷ String
  , balls ∷ String
  , confetti ∷ String
  , fireworks ∷ String
  , gift1 ∷ String
  , gift2 ∷ String
  , gift3 ∷ String
  , gift4 ∷ String
  , gift5 ∷ String
  , gift6 ∷ String
  , hat ∷ String
  , santa1 ∷ String
  , santa2 ∷ String
  , sweet ∷ String
  , tree1 ∷ String
  , tree2 ∷ String
  }

foreign import svg ∷ SvgAssets
