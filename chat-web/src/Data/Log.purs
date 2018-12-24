-- | This module provides a data type that can be used to construct log messages
-- | that conform to a particular format we require for our logging service to
-- | parse for information.

module Chat.Data.Log
  ( Severity(..)
  , Message
  , mkMessage
  ) where

import Prelude

import Chat.Capability.Now (class Now, nowDateTime)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.String as String
import Data.Formatter.DateTime (formatDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- | Severity for the log messages.
-- |
-- | - `Debug` - Information useful for debug purposes.
-- | - `Info` - Normal operation information.
-- | - `Warning` - General warnings, non-critical failures.
-- | - `Error` - General/severe errors.
data Severity
  = Debug
  | Info
  | Warning
  | Error

derive instance genericSeverity ∷ Generic Severity _
derive instance eqSeverity ∷ Eq Severity
derive instance ordSeverity ∷ Ord Severity

instance showSeverity ∷ Show Severity where
  show = genericShow

-- | Consist of the message `Severity` level,
-- | timestamp and the message itself.
type Message =
  { timestamp ∷ DateTime
  , severity ∷ Severity
  , text ∷ String
  }

-- We want the core implementation of our logging function to be pure. When we
-- write our instance later on we'll re-use this function, but it can be tested
-- independently in other contexts like the `Writer` monad or by writing to a
-- golden test suite.

mkMessage ∷ ∀ m. Now m ⇒ Severity → String → m Message
mkMessage severity msg = do
  timestamp ← nowDateTime
  let
    -- Will format "2018-10-25 11:25:29 AM"
    formattedTimestamp = formatDateTime "YYYY-DD-MM hh:mm:ss a" timestamp
    -- It's possible that we had a typo in our format string, so we'll need
    -- to handle that case. We could use `unsafePartial <<< fromLeft` but I'd
    -- like to be a little more careful by logging a failure message. We can
    -- always test this in a pure context, like the Writer monad, to verify!
    timestampStr = either (const "(Failed to assign time)") identity formattedTimestamp
    text = "[" <> String.toUpper (show severity) <> ": " <> timestampStr <> "]\n" <> msg
  pure { timestamp, severity, text }
