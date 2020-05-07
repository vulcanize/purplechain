{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Purplechain.Module.Message where

import Control.Arrow                (left)
import Control.Error.Util           (note)
import Control.Lens
import Control.Monad                ((<=<))
import Data.Text                    (Text)
import GHC.Generics                 (Generic)

import Maker
import Proto3.Suite                 (Named, Message, fromByteString, toLazyByteString)
import Tendermint.SDK.Types.Message (ValidateMessage(..), HasMessageType(..), coerceProto3Error, formatMessageParseError)
import Tendermint.SDK.Codec         (HasCodec(..))

import Tendermint

data PerformMsg' act actor = PerformMsg
  { _performMsg_act :: act
  , _performMsg_actor :: actor
  } deriving (Eq, Ord, Read, Show, Generic)

type PerformMsg = PerformMsg' Act Actor
type PerformMsgProto = PerformMsg' Text Text

instance Message PerformMsgProto
instance Named PerformMsgProto

instance HasMessageType (PerformMsg' act actor) where
  messageType _ = "PerformMsg"

instance HasCodec PerformMsg where
  encode (PerformMsg act actor) =
    view strict . toLazyByteString $ PerformMsg (tshow act) (tshow actor)

  decode = f <=< left (formatMessageParseError . coerceProto3Error) . fromByteString
    where f (PerformMsg act actor) = pure PerformMsg
            <*> (tread act & note ("Cannot parse 'Act' from: " <> act))
            <*> (tread actor & note ("Cannot parse 'Actor' from: " <> actor))

instance ValidateMessage PerformMsg where
  validateMessage _ = pure ()
