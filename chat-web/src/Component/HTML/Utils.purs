module Chat.Component.HTML.Utils
  ( css
  ) where

import Prelude

import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes)
import Prelude.Unicode ((∘))

type WithClass r i = HH.IProp ("class" ∷ String | r) i

css ∷ ∀ r i. Array String → WithClass r i
css = classes ∘ map ClassName
