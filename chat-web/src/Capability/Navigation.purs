-- | This module describes the capability to navigate from place to place in the
-- | application. Currently implemented with hashes, but could easily be swapped
-- | to another method (like `pushState`).

module Chat.Capabiltiy.Navigation
  ( class Navigation
  , navigate
  ) where

import Prelude

import Chat.Data.Route (Route)
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import Prelude.Unicode ((∘))

-- | The ability to navigate around the application.
class Monad m <= Navigation m where
  navigate ∷ Route → m Unit

-- Includes an instance for `HalogenM` to make this class
-- convenient to use in Halogen components.
-- A  special case is given for logging out,
-- as there is no route in our spec meant to handle this case.

instance navigationHalogenM ∷ Navigation m ⇒ Navigation (HalogenM s f g p o m) where
  navigate = lift ∘ navigate
