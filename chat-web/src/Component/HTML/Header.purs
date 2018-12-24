module Chat.Component.HTML.Header
  ( Props
  , header
  ) where

import Chat.Component.HTML.Utils (className, css, safeHref)
import Chat.Data.Route (Route(..))
import Chat.Data.User (User)
import Chat.Icon as Icon
import Data.Maybe (Maybe)
import Halogen.HTML as HH

type Props =
  { user  ∷ Maybe User
  , route ∷ Route
  }

header ∷ ∀ p i. Props → HH.HTML p i
header { user, route } =
  HH.header
  [ css ["header"] ]
  [ HH.a
    [ css ["logo"], safeHref Welcome ]
    [ Icon.logo [ className "logo-icon" ] ]
  ]
