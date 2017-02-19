{-# LANGUAGE Rank2Types #-}

module Network.RTM.Slack.State.Update
    ( update
    ) where

import ClassyPrelude
import Network.RTM.Slack.State
import qualified Web.Slack as S
import Control.Lens ( Getter, (^.), (.~), (&), (%~), ix, at )

update :: S.Event -> SlackState -> SlackState
update (S.ChannelCreated ch) s  = s & slackChannels %~ setInMap S.channelId ch
update (S.ChannelJoined ch) s   = s & slackChannels . ix (ch ^. S.channelId) %~ (S.channelMembers .~ ch ^. S.channelMembers)
                                                                                . (S.channelIsMember .~ True)
update (S.ChannelLeft chId) s   = s & slackChannels . ix chId . S.channelIsMember .~ False
update (S.ChannelDeleted chId) s = s & slackChannels . at chId .~ Nothing
update (S.ChannelRename (S.ChannelRenameInfo chId n _)) s
                                = s & slackChannels . ix chId . S.channelName .~ n
update (S.ChannelArchive chId _) s = s & slackChannels . ix chId . S.channelIsArchived .~ Just True
update (S.ChannelUnarchive chId _) s = s & slackChannels . ix chId . S.channelIsArchived .~ Just False
update (S.ImCreated _ im) s     = s & slackIMs %~ setInMap S.imId im
update (S.GroupJoined g) s      = s & slackGroups %~ setInMap S.channelId g
update (S.GroupLeft g) s        = s & slackGroups . at (g ^. S.channelId) .~ Nothing
update (S.GroupArchive gId) s   = s & slackGroups . ix gId . S.channelIsArchived .~ Just True
update (S.GroupUnarchive gId) s = s & slackGroups . ix gId . S.channelIsArchived .~ Just False
update (S.GroupRename (S.ChannelRenameInfo gId n _)) s
                                = s & slackGroups . ix gId . S.channelName .~ n
update (S.PresenceChange _ _) s = s         -- | Just as a reminder that we could have stored that info
update (S.ManualPresenceChange p) s = s & slackSelf . S.selfManualPresence .~ p
update (S.PrefChange _) s       = s         -- | Can be implemented with some TH due to the amount of fields
update (S.UserChange u) s       = s & slackUsers . at (u ^. S.userId) .~ Just u
update (S.TeamJoin u) s         = s & slackUsers . at (u ^. S.userId) .~ Just u
update (S.TeamPrefChange _) s   = s         -- | Can be implemented with some TH due to the amount of fields
update (S.TeamRenameEvent n) s  = s & slackTeam . S.teamName .~ n
update (S.TeamDomainChange _ d) s = s & slackTeam . S.teamDomain .~ d
update (S.EmailDomainChange d _) s = s & slackTeam . S.teamEmailDomain .~ d
update (S.BotChanged b) s       = s & slackBots %~ setInMap S.botId b
update (S.BotAdded b) s         = s & slackBots %~ setInMap S.botId b
update _ s                      = s

-- | Sets an element into a map
setInMap :: Ord k => Getter a k -> a -> Map k a -> Map k a
setInMap l e = insertMap (e ^. l) e
