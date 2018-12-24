module Chat.Data.Route
  ( Route(..)
  , codec
  , parse
  , print
  ) where

import Prelude hiding ((/))

import Chat.Data.User (User)
import Chat.Data.User as User
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Prelude.Unicode ((∘))
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex (parse, print) as Route
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)

-- We'll represent routes in our application with a simple sum type. As the
-- application grows, you might want to swap this out with an extensible sum
-- type with `Variant` and have several sub-sections. For our small MVP this
-- type will work just fine and will prevent us from trying to send users to
-- non-existent routes.

data Route
  = Welcome
  | Room
  | Contact User

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

parse ∷ String → Either RouteError Route
parse = Route.parse codec

print ∷ Route → String
print = Route.print codec

username ∷ RouteDuplex' String → RouteDuplex' User
username = as User.toString (note "Bad username" ∘ User.parse)
