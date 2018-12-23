module Main where

import Prelude

import Chat.AppM (runAppM)
import Chat.Component.Router as Router
import Chat.Data.Route (codec) as Route
import Chat.Env (BaseURL(..), Env, LogLevel(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI) as VDom
import Prelude.Unicode ((≢))
import Routing.Duplex (parse) as Route
import Routing.Hash (matchesWith) as Route

main :: Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody

  let env ∷ Env
      env =
        { logLevel: Dev
        , baseUrl: BaseURL "ws://localhost:9160"
        }
      router ∷ Router.Component' Aff
      router = H.hoist (runAppM env) Router.component

  driver ← VDom.runUI router unit body
  void $ liftEffect $ Route.matchesWith (Route.parse Route.codec) \old new →
    when (old ≢ Just new) (launchAff_ $ driver.query $ Router.Navigate new unit)
