{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Purplechain.Module.Router where

import           Polysemy                                 (Members, Sem)
import           Tendermint.SDK.BaseApp                   ((:~>), Return, RouteTx, RoutingTx (..), TypedMessage)
import           Tendermint.SDK.Types.Message             (Msg (..))
import           Tendermint.SDK.Types.Transaction         (Tx (..))

import           Purplechain.Module.Message
import           Purplechain.Module.Keeper

type MessageApi =
  TypedMessage PerformMsg :~> Return ()

messageHandlers
  :: Members PurplechainEffs r
  => RouteTx MessageApi r
messageHandlers = performActH

performActH
  :: Members PurplechainEffs r
  => RoutingTx PerformMsg
  -> Sem r ()
performActH (RoutingTx Tx {txMsg=Msg{msgData}}) = do
  performAct msgData
