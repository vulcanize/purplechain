{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Purplechain.Network where

import Tendermint
import Purplechain.Node

test :: IO ()
test = do
  initProcess
  withThrowawayNetwork (purplechainNetwork 3) $ \_root -> \case
    _ -> pure ()

purplechainNetwork :: Word -> AppNetwork PurplechainNode
purplechainNetwork size = AppNetwork
  { _appNetwork_toAppNode = mkPurplechainNode
  , _appNetwork_fromAppNode = _purplechainNode_tendermint
  , _appNetwork_withNode = withPurplechainNode
  , _appNetwork_size = size
  }
