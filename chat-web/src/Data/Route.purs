module Chat.Data.Route
  ( Route(..)
  , codec
  ) where

import Prelude hiding ((/))

import Chat.Data.Username (Username)
import Chat.Data.Username as Username
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Prelude.Unicode ((∘))
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

-- We'll represent routes in our application with a simple sum type. As the
-- application grows, you might want to swap this out with an extensible sum
-- type with `Variant` and have several sub-sections. For our small MVP this
-- type will work just fine and will prevent us from trying to send users to
-- non-existent routes.

data Route
  = Welcome
  | Room
  | Contact Username

derive instance genericRoute ∷ Generic Route _

instance showRoute ∷ Show Route where
  show = genericShow

derive instance eqRoute ∷ Eq Route
derive instance ordRoute ∷ Ord Route

-- Next, we'll define a bidirectional codec for our route parsing. Our single
-- codec will handle both parsing browser locations and serializing our data type
-- to a browser location. We'll skip the boilerplate of separate encoding and
-- decoding functions, and we'll ensure our parsing and printing is always in sync.

-- Our codec will cause a compile-time error if
-- we fail to handle any of our route cases.

codec ∷ RouteDuplex' Route
codec = root $ sum
  { "Welcome": noArgs
  , "Room": "room" / noArgs
  , "Contact": "contact" / username segment
  }

username ∷ RouteDuplex' String → RouteDuplex' Username
username = as Username.toString (note "Bad username" ∘ Username.parse)
