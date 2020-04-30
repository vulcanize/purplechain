{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Purplechain.Network where

import Control.Concurrent
import Control.Monad.State
import Tendermint
import Purplechain.Client hiding (flip)
import Purplechain.Node hiding (flip)

test :: IO ()
test = do
  let seconds = (* 1e6)
      wait = liftIO . threadDelay . seconds
      waiting s x = void $ wait s *> x
  initProcess
  withThrowawayNetwork (purplechainNetwork 1) $ \_root -> \case
    [n0] -> flip evalStateT (initialSystem 0) $ do
      let reloadSystem = do
            old <- get
            resp <- query n0 $ getSystem $ QueryArgs False () 0
            case unQuery resp of
              Nothing -> liftIO $ do
                putStrLn "QUERY ERROR"
                print resp
              Just new -> do
                liftIO $ do
                  putStrLn ""
                  putStrLn "========================== DIFF =========================="
                  print $ diff old new
                  putStrLn "========================== DIFF =========================="
                  putStrLn ""
                put new

      waiting 7 $ liftIO $ print =<< query n0 health
      waiting 1 $ tx n0 performActTx $ PerformMsg $ Mint DAI 12 God
      waiting 1 reloadSystem

    _ -> pure ()

purplechainNetwork :: Word -> AppNetwork PurplechainNode
purplechainNetwork size = AppNetwork
  { _appNetwork_toAppNode = mkPurplechainNode
  , _appNetwork_fromAppNode = _purplechainNode_tendermint
  , _appNetwork_withNode = withPurplechainNode
  , _appNetwork_size = size
  }
