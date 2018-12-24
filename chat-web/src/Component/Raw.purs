module Chat.Component.Raw
  ( Query(..)
  , Input
  , Slot(..)
  , component
  , raw_
  , raw
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentSlot, RefLabel)
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude.Unicode ((◇))
import Web.HTML (HTMLElement)

foreign import unsafeSetInnerHTML
  ∷ HTMLElement
  → String
  → Effect Unit

type State =
  { ref  ∷ RefLabel
  , html ∷ String
  }

type Input =
  { html ∷ String
  }

type Output = Void

data Query a
  = SetInnerHTML a
  | Receive Input a

data Slot = Slot
derive instance eqSlot ∷ Eq Slot
derive instance ordSlot ∷ Ord Slot

type Component = H.Component HH.HTML Query Input Output

type DSL  = H.ComponentDSL State Query Output
type HTML = H.ComponentHTML Query

component
  ∷ ∀ m. MonadAff m
  ⇒ Array (IProp HTMLdiv (Query Unit))
  → Component m
component props = H.lifecycleComponent
  { initialState
  , render
  , eval
  , receiver: HE.input Receive
  , initializer: Just $ H.action SetInnerHTML
  , finalizer: Nothing
  }
  where
    initialState ∷ Input → State
    initialState { html } = { ref: H.RefLabel "raw", html }

    render ∷ State → HTML
    render state = HH.div ([ HP.ref state.ref ] ◇ props) []

    eval ∷ Query ~> DSL m
    eval = case _ of
      SetInnerHTML a → a <$ do
        ref  ← H.gets _.ref
        elem ← H.getHTMLElementRef ref
        for_ elem \el → do
          html ← H.gets _.html
          H.liftEffect $ unsafeSetInnerHTML el html

      Receive { html } a → do
        H.modify_ _ { html = html }
        eval $ SetInnerHTML a

type Raw m q = HH.HTML (ComponentSlot HH.HTML Query m Slot (q Unit)) (q Unit)

raw_ ∷ ∀ m q. MonadAff m ⇒ String → Raw m q
raw_ = raw mempty

raw ∷ ∀ m q. MonadAff m ⇒ Array (IProp HTMLdiv (Query Unit)) → String → Raw m q
raw props html = HH.slot Slot (component props) { html } absurd
