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

data PerformMsg' act = PerformMsg
  { _performMsg_act :: act
  } deriving Generic

type PerformMsg = PerformMsg' Act
type PerformMsgProto = PerformMsg' Text

instance Message PerformMsgProto
instance Named PerformMsgProto

instance HasMessageType (PerformMsg' a) where
  messageType _ = "PerformMsg"

instance HasCodec PerformMsg where
  encode (PerformMsg act) =
    view strict . toLazyByteString . PerformMsg $ tshow act

  decode = f <=< left (formatMessageParseError . coerceProto3Error) . fromByteString
    where f (PerformMsg txt) = tread txt
            & note ("Cannot parse 'Act' from: " <> txt)
            & fmap PerformMsg

instance ValidateMessage PerformMsg where
  validateMessage _ = pure ()
