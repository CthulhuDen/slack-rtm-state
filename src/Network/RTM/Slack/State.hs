{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}

module Network.RTM.Slack.State
    ( SlackState (..)
    , slackSelf
    , slackTeam
    , slackUsers
    , slackChannels
    , slackGroups
    , slackIMs
    , slackBots
    , stateFromSession
    ) where

import ClassyPrelude
import qualified Web.Slack as S
import Control.Lens ( (^.), Getter )
import Control.Lens.TH ( makeLenses )
import Network.RTM.Slack.Instances ()
import Data.Binary

data SlackState = SlackState { _slackSelf :: S.Self
                             , _slackTeam :: S.Team
                             , _slackUsers :: Map S.UserId S.User
                             , _slackChannels :: Map S.ChannelId S.Channel
                             , _slackGroups :: Map S.ChannelId S.Channel
                             , _slackIMs :: Map S.IMId S.IM
                             , _slackBots :: Map S.BotId S.Bot
                             } deriving (Generic, Typeable)

instance Binary SlackState

stateFromSession :: S.SlackSession -> SlackState
stateFromSession s = SlackState { _slackSelf = s ^. S.slackSelf
                                , _slackTeam = s ^. S.slackTeam
                                , _slackUsers = mfl S.userId $ s ^. S.slackUsers
                                , _slackChannels = mfl S.channelId $ s ^. S.slackChannels
                                , _slackGroups = mfl S.channelId $ s ^.  S.slackGroups
                                , _slackIMs = mfl S.imId $ s ^. S.slackIms
                                , _slackBots = mfl S.botId $ s ^. S.slackBots
                                }
  where
    -- | Map from list
    mfl :: Ord k => Getter a k -> [a] -> Map k a
    mfl l xs = mapFromList [(x ^. l, x) | x <- xs]

makeLenses ''SlackState
