{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Purplechain where

import Control.Lens                                     ((^.))
import Data.Conduit.Network                             (serverSettings)
import Data.Default.Class                               (def)
import Data.String                                      (IsString(..))
import qualified Data.Text as T

import Network.ABCI.Server                              (serveAppWith)
import Network.ABCI.Server.App                          (App(..), Request(..), transformApp)
import Tendermint
import Tendermint.Config

main :: IO ()
main = test

type PurplechainNode = TendermintNode

purplechainNetwork :: Word -> AppNetwork PurplechainNode
purplechainNetwork size = AppNetwork
  { _appNetwork_toAppNode = id
  , _appNetwork_fromAppNode = id
  , _appNetwork_withNode = runABCI
  , _appNetwork_size = size
  }

test :: IO ()
test = do
  initProcess
  withThrowawayNetwork (purplechainNetwork 3) $ \_root -> \case
    [_n0,_n1,_n2] -> pure ()
    _ -> error "impossible"

runABCI :: PurplechainNode -> IO ()
runABCI n = do
  let
    cfg = n ^. tendermintNode_config
    (host, port) = unsafeHostPortFromURI $ cfg ^. config_proxyApp

  serveAppWith (serverSettings (fromEnum port) (fromString . T.unpack $ host)) mempty $
    transformApp id app

app :: Applicative m => App m
app = App $ \case
  RequestEcho _ -> pure def
  RequestFlush _ -> pure def
  RequestInfo _ -> pure def
  RequestSetOption _ -> pure def
  RequestInitChain _ -> pure def
  RequestQuery _ -> pure def
  RequestBeginBlock _ -> pure def
  RequestCheckTx _ -> pure def
  RequestDeliverTx _ -> pure def
  RequestEndBlock _ -> pure def
  RequestCommit _ -> pure def
