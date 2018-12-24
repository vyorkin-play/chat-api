-- | This module describes the capability to log messages to some output, whether
-- | the console, an external service like Rollbar, or to files for golden testing.
-- | See AppM for the implementation.

module Chat.Capability.Logging
  ( log
  , logDebug
  , logInfo
  , logWarning
  , logError
  , class Logging
  , logMessage
  , consoleLogger
  ) where

import Prelude

import Chat.Capability.Now (class Now)
import Chat.Data.Log (Message, Severity(..), mkMessage)
import Control.Logger (Logger)
import Control.Logger.Console (console) as Logger
import Control.Monad.Trans.Class (lift)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM)
import Prelude.Unicode ((∘))

-- | Logs the message with given `Severity`.
log ∷ ∀ m. Logging m ⇒ Severity → String → m Unit
log t = logMessage <=< mkMessage t

-- | Logs the message with `Debug` severity.
logDebug ∷ ∀ m. Logging m ⇒ String → m Unit
logDebug = log Debug

-- | Logs the message with `Info` severity.
logInfo ∷ ∀ m. Logging m ⇒ String → m Unit
logInfo = log Info

-- | Logs the message with `Warning` severity.
logWarning ∷ ∀ m. Logging m ⇒ String → m Unit
logWarning = log Warning

-- | Logs the message with `Error` severity.
logError ∷ ∀ m. Logging m ⇒ String → m Unit
logError = log Error

-- We will require the `Logging` type class member to use the `Message` newtype,
-- which is not exported from this module and can only be constructed using our
-- pure implementation below. This restricts all instances to a predictable output.

-- In addition, we'll superclass another capability that is required for this one:
-- the ability to get the current time.

class Now m <= Logging m where
  logMessage ∷ Message → m Unit

instance loggingHalogenM ∷ Logging m ⇒ Logging (HalogenM s f g p o m) where
  logMessage = lift ∘ logMessage

-- We will ultimately log messages to a logging service,
-- but for now, we'll only log to the console.
consoleLogger ∷ ∀ m. MonadEffect m ⇒ Logger m Message
consoleLogger = Logger.console _.text
