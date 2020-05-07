{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Purplechain.Network where

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.State
import Data.List (intersperse)
import qualified Data.Text as T

import Tendermint
import Tendermint.Utils.TxClient.Types (TxClientResponse(..))

import Purplechain.Client hiding (flip)
import Purplechain.Node hiding (flip)
import Purplechain.Module.Keeper hiding (performAct)

seconds :: Int -> Int
seconds = (* 1e6)

wait :: MonadIO m => Int -> m ()
wait = liftIO . threadDelay . seconds


testNetwork :: IO ()
testNetwork = do
  initProcess
  withThrowawayNetwork (purplechainNetwork 1) $ \_ nodes -> do
    wait 7
    testScenario nodes

testScenario :: [PurplechainNode] -> IO ()
testScenario nodes = do
  case nodes of
    [n] -> testScenario' (n, n, n, n)
    (n0 : n1 : n2 : n3 : _) -> testScenario' (n0, n1, n2, n3)
    _ -> pure ()

testScenario' :: (PurplechainNode, PurplechainNode, PurplechainNode, PurplechainNode) -> IO ()
testScenario' (n0, n1, n2, n3) = flip evalStateT genesisSystem $ do
      let
        printDiff act old new = liftIO $ do
          let
            separator = putStrLn "=========================================================="

          separator
          print act
          putStrLn ""

          if old == new
            then putStrLn "= SYSTEM UNCHANGED"
            else print $ diff old new

          putStrLn ""
          print act
          separator

        printErrors = \case
          Left err -> liftIO $ putStrLn $ T.unpack err
          Right r -> pure r

        performAct n actor act = printErrors <=< runExceptT $ do
            old <- get

            preview <- case exec old (being actor $ perform act) of
              Right r -> pure r
              Left err -> throwError $ "Preview failed: " <> tshow err

            _ <- tx n performActTx (PerformMsg act actor) >>= \case
              Response sr -> pure sr
              err -> throwError $ "RPC failed: " <> tshow err

            wait 1
            respQ <- query n0 $ getSystem $ QueryArgs False () 0

            case unQuery respQ of
              Nothing -> throwError $ "Query failed: " <> tshow respQ
              Just new -> do
                put new
                when (new /= preview) $ do
                  liftIO $ putStrLn "PREVIEW MISMATCH"
                  printDiff act old new

      sequence_ $ intersperse (wait 2)
        [
          -- market parameters
          performAct n0 God $ Frob 1.000000000000000001
        , performAct n0 God $ Tell 1.01
        , performAct n0 God $ Form collateralIlk collateralTag
        , performAct n0 God $ Cork collateralIlk 100
        , performAct n0 God $ Mark collateralTag (Wad 1) (Sec 1)

          -- issuance
        , performAct n1 acc1 $ Open urn1 collateralIlk
        , performAct n1 acc1 $ Lock urn1 5
        , performAct n1 acc1 $ Free urn1 1
        , performAct n1 acc1 $ Draw urn1 2
        , performAct n1 acc1 $ Wipe urn1 1

          -- urn lifecycle
        , performAct n2 acc2 $ Open urn2 collateralIlk
        , performAct n2 acc2 $ Give urn2 addr1
        , performAct n2 acc1 $ Shut urn2

          -- liquidation
        , performAct n3 God $ Warp (Sec 100)
        , performAct n3 God $ Bite urn1
        , performAct n3 God $ Grab urn1
        , performAct n3 God $ Plop urn1 4
        ]

purplechainNetwork :: Word -> AppNetwork PurplechainNode
purplechainNetwork size = AppNetwork
  { _appNetwork_toAppNode = mkPurplechainNode
  , _appNetwork_fromAppNode = _purplechainNode_tendermint
  , _appNetwork_withNode = withPurplechainNode
  , _appNetwork_size = size
  }
