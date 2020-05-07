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
import Control.Lens         (at, ifor_, (^.), (&), (?~))
import Data.Fixed           (Fixed(..))
import Data.Foldable        (for_)
import qualified Data.Map   as Map
import Data.Semialign       (align)
import Data.String          (fromString)
import qualified Data.Text   as T
import Data.These           (These(..))

import Maker                hiding (Error)
import Maker.Decimal        (Decimal(..))
import Polysemy             (Sem, Member, Members, interpret, makeSem)
import Polysemy.Error       (Error, fromEither, mapError)
import Tendermint
import qualified Tendermint.SDK.BaseApp as BA
import qualified Tendermint.SDK.BaseApp.Store.Var as V
import qualified Tendermint.SDK.Modules.Bank as Bank
import qualified Tendermint.SDK.Types.Address as SDK

import Purplechain.Module.Message
import Purplechain.Module.Types


type PurplechainEffs = '[PurplechainKeeper, Error PurplechainError]

data PurplechainKeeper m a where
  PerformAct :: PerformMsg -> PurplechainKeeper m ()

makeSem ''PurplechainKeeper

eval
  :: Members BA.TxEffs r
  => Members BA.BaseEffs r
  => Member Bank.BankKeeper r
  => Sem (PurplechainKeeper ': Error PurplechainError ': r) a
  -> Sem r a
eval = mapError BA.makeAppError . evalPurplechain
  where
    evalPurplechain
      :: Members BA.TxEffs r
      => Members BA.BaseEffs r
      => Member Bank.BankKeeper r
      => Member (Error PurplechainError) r
      => Sem (PurplechainKeeper ': r) a -> Sem r a
    evalPurplechain = interpret $ \case
      PerformAct (PerformMsg act actor) -> do
        let action = being actor $ perform act
        oldSystem <- V.takeVar systemVar >>= \case
          Nothing -> genesis
          Just s -> pure s
        newSystem <- fromEither $ left (PurplechainError . tshow) $ exec oldSystem action
        V.putVar newSystem systemVar

        let balanceDiffs = minimizeDiff $ align (oldSystem ^. balances) (newSystem ^. balances)
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
              Left _ -> "N/A"
              Right s -> tshow s

        BA.log BA.Info $ "Act: " <> tshow act
        BA.log BA.Info $ "Actor: " <> tshow actor

        for_ (Map.keys $ newSystem ^. urns) $ \(Id urnId) ->
          BA.log BA.Info $ showDiff "Stage of" (T.pack urnId) (showStage oldSystem urnId, showStage newSystem urnId)

        ifor_ balanceDiffs $ \(owner, token) diff -> do
          let
            addr = mkKeplerAddress owner
            mkCoin = mkKeplerCoin token

          (old, new) <- case diff of
            This old -> do
              Bank.burn addr (mkCoin old)
              pure (tshow old, "N/A")
            That new -> do
              Bank.mint addr (mkCoin new)
              pure ("N/A", tshow new)
            These old new -> let change = new - old in do
              if change > 0
                then Bank.mint addr (mkCoin change)
                else Bank.burn addr (mkCoin $ negate change)
              pure (tshow old, tshow new)

          BA.log BA.Info $ showDiff "Balance for" (tshow (owner, token)) (old, new)

--TODO: is there a genesis hook?
genesis :: Member Bank.BankKeeper r => Sem r System
genesis = do
  for_ [acc1, acc2] $ \owner ->
    Bank.mint (mkKeplerAddress owner) (mkKeplerCoin (Gem collateralTag) genesisWad)
  pure genesisSystem

genesisWad :: Wad
genesisWad = 10

genesisSystem :: System
genesisSystem = initialSystem 1.0
    & balances . at (acc1, Gem collateralTag) ?~ genesisWad
    & balances . at (acc2, Gem collateralTag) ?~ genesisWad

mkKeplerAddress :: Actor -> SDK.Address
mkKeplerAddress = SDK.addressFromBytes . fromString . show

mkKeplerCoinId :: Token -> Bank.CoinId
mkKeplerCoinId = Bank.CoinId . tshow

mkKeplerCoin :: Token -> Wad -> Bank.Coin
mkKeplerCoin token wad = Bank.Coin (mkKeplerCoinId token) (mkKeplerAmount wad)

-- WARNING: Overflows for
-- 1e20 > 2 ^ 64 > 1e18
mkKeplerAmount :: Wad -> Bank.Amount
mkKeplerAmount (Wad (D (MkFixed wad))) = Bank.Amount (fromInteger wad)

makerAddrToKeplerAddr :: Address -> SDK.Address
makerAddrToKeplerAddr (Address dst) = SDK.addressFromBytes $ fromString dst

acc1,acc2 :: Actor
acc1 = Account addr1
acc2 = Account addr2

addr1, addr2 :: Address
addr1 = Address "Address 1"
addr2 = Address "Address 2"

urn1, urn2 :: Id Urn
urn1 = Id "urn1"
urn2 = Id "urn2"

collateralTag :: Id Tag
collateralTag = Id "collateral"

collateralIlk :: Id Ilk
collateralIlk = Id "collateral"
