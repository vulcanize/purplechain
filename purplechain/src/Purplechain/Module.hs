{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Purplechain.Module where

import Data.Proxy

import Tendermint.SDK.Application  (Module (..), ModuleEffs)
import Tendermint.SDK.BaseApp      (DefaultCheckTx (..))
import Tendermint.SDK.Modules.Bank (Bank)
import Polysemy                    (Members)

import Purplechain.Module.Keeper   (PurplechainEffs, eval)
import Purplechain.Module.Query    (QueryApi, querier)
import Purplechain.Module.Router   (MessageApi, messageHandlers)
import Purplechain.Module.Types    (ModuleName)

type Purplechain =
  Module ModuleName MessageApi MessageApi QueryApi PurplechainEffs '[Bank]

purplechainModule
  :: Members (ModuleEffs Purplechain) r
  => Purplechain r
purplechainModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy @MessageApi) Proxy
  , moduleQuerier = querier
  , moduleEval = eval
  }
