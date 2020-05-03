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
import Control.Error.Util (hoistEither)
import Control.Monad.Except
import Control.Monad.State
import Tendermint
import Tendermint.Utils.TxClient.Types (TxClientResponse(..))
import Purplechain.Client hiding (flip)
import Purplechain.Node hiding (flip)

test :: IO ()
test = do
  let seconds = (* 1e6)
      wait = liftIO . threadDelay . seconds
      waiting s x = void $ wait s *> x
  initProcess
  withThrowawayNetwork (purplechainNetwork 1) $ \_root -> \case
    (n0 : _) -> flip evalStateT (initialSystem 0) $ do
      let
        printDiff old new = liftIO $ do
          putStrLn "========================== DIFF =========================="
          if old == new
            then putStrLn "= SYSTEM UNCHANGED"
            else print $ diff old new
          putStrLn "========================== DIFF =========================="

        printErrors = \case
          Left err -> liftIO $ print err
          Right r -> pure r

        performAct n act = printErrors <=< runExceptT $ do
            old <- get

            preview <- hoistEither $ case exec old (perform act) of
              Right r -> pure r
              Left err -> throwError $ "Preview failed: " <> tshow err

            respTx <- tx n performActTx (PerformMsg act) >>= \case
              Response sr -> pure sr
              err -> throwError $ "RPC failed: " <> tshow err

            liftIO $ print $ tshow respTx

            wait 1
            respQ <- query n0 $ getSystem $ QueryArgs False () 0
            printDiff old preview

            case unQuery respQ of
              Nothing -> throwError $ "Query failed: " <> tshow respQ
              Just new -> do
                put new
                when (new /= preview) $ do
                  liftIO $ putStrLn "PREVIEW MISMATCH"
                  printDiff old new

      waiting 7 $ liftIO $ print =<< query n0 health
      waiting 1 $ performAct n0 $ Mint DAI 12 God
      waiting 1 $ performAct n0 Prod

    [] -> pure ()

purplechainNetwork :: Word -> AppNetwork PurplechainNode
purplechainNetwork size = AppNetwork
  { _appNetwork_toAppNode = mkPurplechainNode
  , _appNetwork_fromAppNode = _purplechainNode_tendermint
  , _appNetwork_withNode = withPurplechainNode
  , _appNetwork_size = size
  }
