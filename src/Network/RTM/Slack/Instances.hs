{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.RTM.Slack.Instances where

import ClassyPrelude
import Data.Binary
import qualified Web.Slack as S
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Aeson

instance Binary S.ChannelId
instance Binary S.UserId
instance Binary S.IMId
instance Binary S.FileId
instance Binary S.CommentId
instance Binary S.BotId
instance Binary S.TeamId

deriving instance Generic S.BotIcons
instance Binary S.BotIcons

deriving instance Generic S.TeamIcons
instance Binary S.TeamIcons

deriving instance Generic S.Submitter
instance Binary S.Submitter

deriving instance Generic S.Mode
instance Binary S.Mode

deriving instance Generic S.FileUrl
instance Binary S.FileUrl

deriving instance Generic S.Thumbnail
instance Binary S.Thumbnail

deriving instance Generic S.Preview
instance Binary S.Preview

deriving instance Generic S.Comment
instance Binary S.Comment

deriving instance Generic S.File
instance Binary S.File

deriving instance Generic S.Subtype
instance Binary S.Subtype

deriving instance Generic S.Edited
instance Binary S.Edited

deriving instance Generic S.MessageUpdate
instance Binary S.MessageUpdate

instance Binary DiffTime where
    put = put . diffTimeToPicoseconds
    get = picosecondsToDiffTime <$> get
deriving instance Generic Day
instance Binary Day

deriving instance Generic UTCTime
instance Binary UTCTime

instance Binary POSIXTime where
    put = put . posixSecondsToUTCTime
    get = utcTimeToPOSIXSeconds <$> get

deriving instance Generic S.Time
instance Binary S.Time

deriving instance Generic S.SlackTimeStamp
instance Binary S.SlackTimeStamp

deriving instance Generic S.Topic
instance Binary S.Topic

deriving instance Generic S.Channel
instance Binary S.Channel

deriving instance Generic S.Profile
instance Binary S.Profile

deriving instance Generic S.Permissions
instance Binary S.Permissions

deriving instance Generic S.EmbeddedItem
instance Binary S.EmbeddedItem

deriving instance Generic S.Timezone
instance Binary S.Timezone

deriving instance Generic S.Item
instance Binary S.Item

deriving instance Generic S.SlackError
instance Binary S.SlackError

deriving instance Generic S.Bot
instance Binary S.Bot

deriving instance Generic S.User
instance Binary S.User

deriving instance Generic S.Presence
instance Binary S.Presence

deriving instance Generic S.FileReference
instance Binary S.FileReference

deriving instance Generic S.IM
instance Binary S.IM

deriving instance Generic S.ChannelRenameInfo
instance Binary S.ChannelRenameInfo

deriving instance Generic S.ChannelOpt
instance Binary S.ChannelOpt

deriving instance Generic S.Team
instance Binary S.Team

deriving instance Generic S.TeamPreferences
instance Binary S.TeamPreferences

deriving instance Generic S.Preferences
instance Binary S.Preferences

deriving instance Generic S.Self
instance Binary S.Self

deriving instance Generic S.Event
instance Binary S.Event

deriving instance Generic Value
instance Binary Value

instance Binary (Vector Value) where
    put = put . toList
    get = fromList <$> get

instance Binary (HashMap Text Value) where
    put = put . mapToList
    get = mapFromList <$> get
