module Chat.Web.Socket.WebSocket
  ( CloseReason(..)
  , toCloseReason
  , onOpen
  , onClose
  , onMessage
  ) where

import Prelude

import Chat.Data.Message (Message)
import Chat.Data.Message as Message
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.String as String
import Effect (Effect)
import Foreign (Foreign, MultipleErrors)
import Foreign as Foreign
import Prelude.Unicode ((∘))
import Text.Parsing.StringParser (ParseError(..))
import Web.Event.Event (Event)
import Web.Event.EventTarget as EventTarget
import Web.Socket.Event.CloseEvent (CloseEvent)
import Web.Socket.Event.CloseEvent as CloseEvent
import Web.Socket.Event.EventTypes as EventTypes
import Web.Socket.Event.MessageEvent (MessageEvent)
import Web.Socket.Event.MessageEvent as MessageEvent
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WebSocket

data CloseReason = CloseReason Int String

toCloseReason ∷ CloseEvent → CloseReason
toCloseReason event = CloseReason
  (CloseEvent.code event)
  (CloseEvent.reason event)

onOpen
  ∷ WebSocket
  → (Event → Effect Unit)
  → Effect Unit
onOpen socket fn = do
  let target = WebSocket.toEventTarget socket
  listener ← EventTarget.eventListener fn
  EventTarget.addEventListener EventTypes.onOpen listener false target

onClose
  ∷ WebSocket
  → (CloseEvent → Effect Unit)
  → Effect Unit
onClose socket fn = do
  let target = WebSocket.toEventTarget socket
  listener ← EventTarget.eventListener (traverse_ fn ∘ CloseEvent.fromEvent)
  EventTarget.addEventListener EventTypes.onClose listener false target

onMessage
  ∷ WebSocket
  → (Either ParseError Message → Effect Unit)
  → Effect Unit
onMessage socket fn = do
  let target = WebSocket.toEventTarget socket
      handler = traverse_ (fn ∘ parseEvent) ∘ MessageEvent.fromEvent
  listener ← EventTarget.eventListener handler
  EventTarget.addEventListener EventTypes.onMessage listener false target
  where
    parseEvent ∷ MessageEvent → Either ParseError Message
    parseEvent event = either Left Message.parse msg where
      raw = MessageEvent.data_ event
      msg = readMessage raw

    readMessage ∷ Foreign → Either ParseError String
    readMessage = lmap toParseError ∘ runExcept ∘ Foreign.readString

    toParseError ∷ MultipleErrors → ParseError
    toParseError errors = ParseError
      $ "Unable to parse message data: \n"
      <> String.joinWith "\n" (renderErrors errors)

    renderErrors ∷ MultipleErrors → Array String
    renderErrors = map Foreign.renderForeignError ∘ Array.fromFoldable
