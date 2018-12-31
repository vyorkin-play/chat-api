module Chat.Page.Welcome
  ( Query(..)
  , Input
  , Output(..)
  , Slot(..)
  , WithCaps
  , Component'
  , Component
  , component
  ) where

import Prelude

import Chat.Capability.Logging (class Logging)
import Chat.Capabiltiy.Navigation (class Navigation)
import Chat.Component.HTML.Utils (css)
import Chat.Data.User (User)
import Chat.Data.User as User
import Chat.Page.Welcome.JoinForm (JoinForm)
import Chat.Page.Welcome.JoinForm (render, validators, formProxy, Slot(..)) as JoinForm
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude.Unicode ((∘))

data Query a
  = Initialize a
  | HandleForm (F.Message' JoinForm) a

type State = Unit

type Input = Unit
data Output = Join User

data Slot = Slot
derive instance eqSlot ∷ Eq Slot
derive instance ordSlot ∷ Ord Slot

type ChildQuery m = F.Query' JoinForm m
type ChildSlot = JoinForm.Slot

type WithCaps c m r
  = MonadAff m
  ⇒ Logging m
  ⇒ Navigation m
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
          for_ user $ H.raise ∘ Join
          pure a
        _ →
          pure a

    render ∷ State → HTML m
    render state =
      HH.div
      [ css ["page-welcome"] ]
      [ renderJoinForm
      ]

    renderJoinForm ∷ HTML m
    renderJoinForm = HH.slot
      JoinForm.Slot
      F.component
      { initialInputs: F.mkInputFields JoinForm.formProxy
      , validators: JoinForm.validators
      , render: JoinForm.render
      }
      (HE.input HandleForm)
