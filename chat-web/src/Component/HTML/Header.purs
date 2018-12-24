module Chat.Component.HTML.Header
  ( Input
  , Query(..)
  , State
  , Slot(..)
  , header
  ) where

import Prelude

import Chat.Assets as Assets
import Chat.Component.HTML.Utils (css, safeHref)
import Chat.Component.Raw (raw_)
import Chat.Component.Raw as Raw
import Chat.Data.Route (Route(..))
import Chat.Data.User (User)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

data Query a = Unit a

type State = Unit

type Input =
  { user  ∷ Maybe User
  , route ∷ Route
  , title ∷ String
  }

type Output = Void

data Slot = Slot
derive instance eqSlot ∷ Eq Slot
derive instance ordSlot ∷ Ord Slot

type ChildQuery = Raw.Query
type ChildSlot  = Raw.Slot

type Component = H.Component HH.HTML Query Input Output

type DSL m  = H.ParentDSL State Query ChildQuery ChildSlot Output m
type HTML m = H.ParentHTML Query ChildQuery ChildSlot m

header ∷ ∀ m. MonadAff m ⇒ Component m
header = H.parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
 where
    eval ∷ Query ~> DSL m
    eval (Unit a) = pure a

    render ∷ State → HTML m
    render _ =
      HH.header
      [ css ["header"] ]
      [ HH.a
        [ css ["logo"], safeHref Welcome ]
        [ raw_ Assets.svg.logo ]
      ]
