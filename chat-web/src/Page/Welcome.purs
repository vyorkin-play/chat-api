module Chat.Page.Welcome
  ( Query(..)
  , Input
  , Output
  , Slot(..)
  , WithCaps
  , Component'
  , Component
  , component
  ) where

import Prelude

import Chat.Capabiltiy.Hub (class Hub, connect)
import Chat.Capabiltiy.Navigation (class Navigation, navigate)
import Chat.Component.HTML.Utils (css)
import Chat.Data.Route (Route(..)) as Route
import Chat.Data.User (User(..))
import Chat.Data.User as User
import Chat.Form.Field as Field
import Chat.Form.Validation (required, minLength) as Validation
import Chat.Form.Validation.Error (Error) as Validation
import Chat.Page.Welcome.JoinForm (JoinForm)
import Chat.Page.Welcome.JoinForm (render, validators, formProxy, Slot(..)) as JoinForm
import Control.Monad.Reader (class MonadAsk)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude.Unicode ((⋙))
import Web.Socket.WebSocket (WebSocket)

data Query a
  = Initialize a
  | HandleForm (F.Message' JoinForm) a

type State = Unit

type Input  = Unit
type Output = Void

data Slot = Slot
derive instance eqSlot ∷ Eq Slot
derive instance ordSlot ∷ Ord Slot

type ChildQuery m = F.Query' JoinForm m
type ChildSlot  = JoinForm.Slot

type WithCaps c m r
  = MonadAff m
  ⇒ MonadAsk { user ∷ Ref (Maybe User) | r } m
  ⇒ Navigation m
  ⇒ Hub m
  ⇒ c m

type Component' m = H.Component HH.HTML Query Input Output m
type Component  m r = WithCaps Component' m r

type DSL m  = H.ParentDSL State Query (ChildQuery m) ChildSlot Output m
type HTML m = H.ParentHTML Query (ChildQuery m) ChildSlot m

component ∷ ∀ m r. Component m r
component = H.lifecycleParentComponent
  { initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just $ H.action Initialize
  , finalizer: Nothing
  }
  where
    initialState ∷ Input → State
    initialState = const unit

    eval ∷ Query ~> DSL m
    eval = case _ of
      Initialize a →
        pure a
      HandleForm msg a → case msg of
        F.Submitted outputs → do
          let
            fields = F.unwrapOutputFields outputs
            user = User.parse fields.name
            -- traverse connect user
            -- traverse_ (\_ → navigate Route.Room)
          pure a
        _ →
          pure a

    render ∷ State → HTML m
    render state =
      HH.div
      [ css ["container"] ]
      [ renderJoinForm
      ]

    renderJoinForm ∷ HTML m
    renderJoinForm =
      HH.slot
        JoinForm.Slot
        F.component
        { initialInputs: F.mkInputFields JoinForm.formProxy
        , validators: JoinForm.validators
        , render: JoinForm.render
        }
        (HE.input HandleForm)
