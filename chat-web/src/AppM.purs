-- | A custom application monad that provides concrete implementations for our
-- | abstract capabilities.

module Chat.AppM where

import Prelude

import Chat.Capability.Now (class Now)
import Chat.Env (Env)
import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as Now
import Type.Prelude (class TypeEquals, from)

-- Our application monad will be the base of a `ReaderT` transformer.
-- We gain the functionality of `ReaderT` in addition to any other instances we write.
-- Our instances will provide the implementations for our capabilities.

newtype AppM a = AppM (ReaderT Env Aff a)

-- We'll write the instances below, but first,
-- we'll write a function that will recover `Aff` from our custom type.
-- Halogen applications must run in `Aff`, so this is a necessary step.
-- See its use in `Main.purs` for more.

runAppM ∷ Env → AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

-- We can  derive all the instances we need to make
-- this new type a monad and allow the use of `Effect` and `Aff`.

derive instance newtypeAppM ∷ Newtype (AppM a) _

derive newtype instance functorAppM ∷ Functor AppM
derive newtype instance applyAppM ∷ Apply AppM
derive newtype instance applicativeAppM ∷ Applicative AppM
derive newtype instance bindAppM ∷ Bind AppM
derive newtype instance monadAppM ∷ Monad AppM
derive newtype instance monadEffectAppM ∷ MonadEffect AppM
derive newtype instance monadAffAppM ∷ MonadAff AppM

-- This instance has to be defined manually, as we are using a type synonym
-- rather than a newtype for our environment.

-- from ∷ e → Env
-- ask ∷ AppM e
-- asks ∷ MonadAsk e AppEnv ⇒ (e → Env) → AppM Env

instance monadAskAppM ∷ TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

-- Next, we'll define instances for each of our capabilities.

-- We'll rely on the Effect instances to fetch date times,
-- so this instance is trivial.
-- However, relying on this instance means we can easily swap in another time source.
-- Our tests will rely on static values, for example.

instance nowAppM ∷ Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime
