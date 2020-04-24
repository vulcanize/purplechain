{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Purplechain.Module.Keeper where

import Control.Arrow        (left)
import Data.TreeDiff.Class  (ediff)
import Data.TreeDiff.Pretty (ansiWlEditExprCompact)

import Maker                (exec, initialSystem, perform)
import Polysemy             (Sem, Member, Members, interpret, makeSem)
import Polysemy.Error       (Error, fromEither, mapError)
import Tendermint
import qualified Tendermint.SDK.BaseApp as BA
import qualified Tendermint.SDK.BaseApp.Store.Var as V

import Purplechain.Module.Message
import Purplechain.Module.Types


type PurplechainEffs = '[PurplechainKeeper, Error PurplechainError]

data PurplechainKeeper m a where
  PerformAct :: PerformMsg -> PurplechainKeeper m ()

makeSem ''PurplechainKeeper

eval
  :: Members BA.TxEffs r
  => Members BA.BaseEffs r
  => Sem (PurplechainKeeper ': Error PurplechainError ': r) a
  -> Sem r a
eval = mapError BA.makeAppError . evalPurplechain
  where
    evalPurplechain
      :: Members BA.TxEffs r
      => Members BA.BaseEffs r
      => Member (Error PurplechainError) r
      => Sem (PurplechainKeeper ': r) a -> Sem r a
    evalPurplechain = interpret $ \case
      PerformAct (PerformMsg act) -> do
        currentSystem <- V.takeVar systemVar >>= \case
          Nothing -> pure $ initialSystem 0 --TODO: is there a genesis hook?
          Just s -> pure s
        newSystem <- fromEither $ left (PurplechainError . tshow) $ exec currentSystem (perform act)
        V.putVar newSystem systemVar
        BA.emit $ PurplechainEvent $ tshow $ ansiWlEditExprCompact $ ediff currentSystem newSystem
