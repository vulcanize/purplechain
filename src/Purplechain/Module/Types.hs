{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Purplechain.Module.Types where

import Control.Error.Util (note)
import Control.Lens
import qualified Data.Aeson as A
import Data.Fixed                                       (HasResolution)
import Data.TreeDiff.Class
import Data.Text                                        (Text)
import Data.Word (Word64)
import GHC.Generics                                     (Generic)

import Maker
import Maker.Decimal
import Tendermint.SDK.Codec (HasCodec(..))
import Tendermint.SDK.BaseApp.Store
import qualified Tendermint.SDK.BaseApp as BA
import qualified Tendermint.SDK.BaseApp.Store.Map as M
import qualified Tendermint.SDK.BaseApp.Store.Var as V

{- Orphans -}
instance ToExpr Actor
instance ToExpr Maker.Address
instance HasResolution a => ToExpr (Decimal a)
instance ToExpr (Id a)
instance ToExpr Ilk
instance ToExpr Mode
instance ToExpr Ray
instance ToExpr Sec
instance ToExpr System
instance ToExpr Tag
instance ToExpr Token
instance ToExpr Urn
instance ToExpr Vox
instance ToExpr Wad

type ModuleName = "purplechain"

data PurplechainError = PurplechainError Text

instance BA.IsAppError PurplechainError where
  makeAppError (PurplechainError msg) = BA.AppError
      { BA.appErrorCode = 1
      , BA.appErrorCodespace = "purplechain"
      , BA.appErrorMessage = msg
      }

data PurplechainEvent = PurplechainEvent Text
  deriving (Eq, Ord, Read, Show, Generic)

instance BA.ToEvent PurplechainEvent


newtype BlockHeight = BlockHeight Word64
  deriving newtype (Enum, Num, BA.QueryData)

instance RawKey BlockHeight where
  rawKey = rawKey . coerced

data PurplechainNamespace

store :: Store PurplechainNamespace
store = makeStore $ KeyRoot "purplechain"

data SystemKey = SystemKey
data SystemMapKey = SystemMapKey

instance RawKey SystemKey where
  rawKey = iso (const "system") (const SystemKey)

instance RawKey SystemMapKey where
  rawKey = iso (const "blocks") (const SystemMapKey)

instance IsKey SystemKey PurplechainNamespace where
  type Value SystemKey PurplechainNamespace = V.Var System

instance IsKey SystemMapKey PurplechainNamespace where
  type Value SystemMapKey PurplechainNamespace = M.Map BlockHeight System

systemVar :: V.Var System
systemVar = V.makeVar SystemKey store

systemsMap :: M.Map BlockHeight System
systemsMap = M.makeMap SystemMapKey store

instance HasCodec System where
  encode = view strict . A.encode
  decode = note "Cannot decode 'System'" . A.decode . view lazy
