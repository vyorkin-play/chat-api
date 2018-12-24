module Chat.Component.HTML.Utils
  ( css
  , className
  , safeHref
  , whenElem
  , maybeElem
  , emptyElem
  ) where

import Prelude

import Chat.Data.Route (Route)
import Chat.Data.Route (print) as Route
import Chat.Utils.Conditional ((?))
import Data.Maybe (Maybe(..))
import Halogen.HTML (AttrName(..), ClassName(..), IProp, attr)
import Halogen.HTML as HH
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Prelude.Unicode ((∘))

type WithClass r i = IProp ("class" ∷ String | r) i

css ∷ ∀ r i. Array String → WithClass r i
css = classes ∘ map ClassName

className ∷ ∀ p i. String → IProp p i
className = attr $ AttrName "class"

-- We can ensure that we only create safe hashes by relying on our `Route` data type
safeHref ∷ ∀ r i. Route → IProp (href ∷ String | r) i
safeHref = HP.href ∘ append "#" ∘ Route.print

whenElem ∷ ∀ p i. Boolean → (Unit → HH.HTML p i) → HH.HTML p i
whenElem cond f = cond ? f unit $ emptyElem

maybeElem ∷ ∀ p i a. Maybe a → (a → HH.HTML p i) → HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = emptyElem

emptyElem ∷ ∀ p i. HH.HTML p i
emptyElem = HH.text ""
