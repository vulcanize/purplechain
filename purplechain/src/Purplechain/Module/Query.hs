{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Purplechain.Module.Query where

import Maker
import Polysemy (Members)
import Tendermint.SDK.BaseApp (QueryEffs, StoreLeaf, RouteQ, storeQueryHandler)
import qualified Tendermint.SDK.BaseApp.Store.Var as V
import Servant.API ((:>))

import Purplechain.Module.Types

type QueryApi = "system" :> StoreLeaf (V.Var System)

querier :: Members QueryEffs r => RouteQ QueryApi r
querier = storeQueryHandler systemVar
