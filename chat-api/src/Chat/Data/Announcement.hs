module Chat.Data.Announcement
  ( AnnouncementError
  , parse
  , errorText
  ) where

import Chat.Data.Client (ClientId (..))
import qualified Data.Char as Char
import qualified Data.Text as Text

data AnnouncementError
  = WrongAnnouncement
  | InvalidUserName

errorText :: AnnouncementError -> Text
errorText WrongAnnouncement = "Wrong announcement"
errorText InvalidUserName   = "Invalid user name"

parse :: Text -> Either AnnouncementError ClientId
parse s | isWrongAnnouncement s = Left WrongAnnouncement
parse s | isInvalidUserName s   = Left InvalidUserName
parse s = Right $ mkClientId s

isWrongAnnouncement :: Text -> Bool
isWrongAnnouncement s = not (prefix `Text.isPrefixOf` s)

isInvalidUserName :: Text -> Bool
isInvalidUserName s = any ($ name)
  [ Text.null
  , Text.any Char.isPunctuation
  , Text.any Char.isSpace
  ]
  where name = mkClientName s

mkClientId :: Text -> ClientId
mkClientId = ClientId . mkClientName

mkClientName :: Text -> Text
mkClientName = Text.drop (Text.length prefix)

prefix :: Text
prefix = "!hi!"
