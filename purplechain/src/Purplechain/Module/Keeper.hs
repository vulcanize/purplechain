{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Purplechain.Module.Keeper where

import Control.Arrow        (left)
import Control.Lens         (ifor_, (^.))
import Data.Foldable        (for_)
import qualified Data.Map   as Map
import Data.Maybe           (fromMaybe)
import Data.Semialign       (align)
import qualified Data.Text   as T
import Data.These           (These(..))

import Maker                hiding (Error)
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
      PerformAct (PerformMsg act actor) -> do
        let action = being actor $ perform act
        currentSystem <- fromMaybe genesis <$> V.takeVar systemVar
        newSystem <- fromEither $ left (PurplechainError . tshow) $ exec currentSystem action
        V.putVar newSystem systemVar

        let balanceDiffs = minimizeDiff $ align (currentSystem ^. balances) (newSystem ^. balances)
            minimizeDiff = Map.filter $ \case
              These a b -> a /= b
              _ -> True
            showDiff label subject (before, after) = T.unwords
              [ label
              , subject <> ":"
              , before
              , "->"
              , after
              ]
            showStage sys urnId = case getStage sys (Id urnId) of
              Left err -> "Error (" <> tshow err <> ")"
              Right s -> tshow s

        BA.log BA.Info $ "Actor: " <> tshow actor
        BA.log BA.Info $ "Act: " <> tshow act

        for_ (Map.keys $ newSystem ^. urns) $ \(Id urnId) ->
          BA.log BA.Info $ showDiff "Stage of" (T.pack urnId) (showStage currentSystem urnId, showStage newSystem urnId)

        ifor_ balanceDiffs $ \(owner, token) -> BA.log BA.Info . showDiff "Balance of" (tshow (owner,token)) . \case
          This old -> (tshow old, "N/A")
          That new -> ("N/A", tshow new)
          These old new -> (tshow old, tshow new)

--TODO: is there a genesis hook?
genesis :: System
genesis = initialSystem 1.0
