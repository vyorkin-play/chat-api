module Main where

import Prelude

import Chat.AppM (runAppM)
import Chat.Component.Router as Router
import Chat.Data.BaseURL (BaseURL(..))
import Chat.Data.Route (codec) as Route
import Chat.Env (LogLevel(..), mkEnv)
import Control.Coroutine (Producer)
import Control.Coroutine.Aff as Coroutine
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Foreign (F, Foreign, readString, unsafeToForeign)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI) as VDom
import Prelude.Unicode ((∘), (≢))
import Routing.Duplex (parse) as Route
import Routing.Hash (matchesWith) as Route
import Web.Event.EventTarget as EventTarget
import Web.Socket.Event.EventTypes as EventTypes
import Web.Socket.Event.MessageEvent as MessageEvent
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WebSocket

main :: Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody

  -- env ← liftEffect $ mkEnv (BaseURL "ws://echo.websocket.org") Dev
  env ← liftEffect $ mkEnv (BaseURL "ws://localhost:9160") Dev

  let router ∷ Router.Component' Aff
      router = H.hoist (runAppM env) Router.component

  driver ← VDom.runUI router unit body
  void $ liftEffect $ Route.matchesWith (Route.parse Route.codec) \old new →
    when (old ≢ Just new) (launchAff_ $ driver.query $ Router.Navigate new unit)
