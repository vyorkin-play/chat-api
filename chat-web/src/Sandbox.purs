module Chat.Sandbox where

import Prelude

import Web.Event.EventTarget as EventTarget
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Const (Const(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (liftEffect)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)

class Foo m where
  foo ∷ Unit → m Int

newtype App a = App (ReaderT Unit Aff a)

derive instance newtypeApp ∷ Newtype (App a) _

runApp ∷ App ~> Aff
runApp (App m) = runReaderT m unit

derive newtype instance functorApp ∷ Functor App
derive newtype instance applyApp ∷ Apply App
derive newtype instance applicativeApp ∷ Applicative App
derive newtype instance bindApp ∷ Bind App
derive newtype instance monadApp ∷ Monad App
derive newtype instance monadEffectApp ∷ MonadEffect App
derive newtype instance monadAffApp ∷ MonadAff App

instance fooApp ∷ Foo App where
  foo _ = pure 42

example
  ∷ ∀ m
  . Foo m
  ⇒ MonadEffect m
  ⇒ m Unit
example =
  void $ eventListener' \ev → do
    _ ← foo unit
    pure unit

eventListener' ∷ ∀ a m. MonadEffect m ⇒ (Event → m a) → m EventListener
eventListener' = unsafeCoerce EventTarget.eventListener
