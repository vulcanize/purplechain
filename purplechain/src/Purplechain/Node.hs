{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Purplechain.Node
  ( module Purplechain.Node
  , module Purplechain.Module.Message
  , module Maker
  , QueryArgs(..)
  ) where

import Control.Concurrent.Async
import Control.Exception
import Control.Lens                                     (makeLenses, (^.), (&))

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Conduit.Network                             (serverSettings)
import Data.Foldable                                    (fold)
import Data.String                                      (IsString(..))
import Data.Text                                        (Text)
import GHC.Generics
import qualified Data.Text as T
import Shelly                                           (shelly, sleep)
import System.Environment                               (getArgs)
import System.Process                                   (createProcess, getPid, shell, spawnProcess, terminateProcess, waitForProcess)
import System.Which                                     (staticWhich)

import qualified Katip                                  as K
import qualified Tendermint.SDK.BaseApp.Logger.Katip    as KL
import Maker
import Network.ABCI.Server                              (serveAppWith)
import Tendermint
import Tendermint.Config
import Tendermint.SDK.BaseApp                           (contextLogConfig)
import Tendermint.SDK.BaseApp.Query.Types               (QueryArgs(..))

import Purplechain.Application                          (mkContext, makeIOApp)
import Purplechain.Module.Message

data PurplechainNode = PurplechainNode
  { _purplechainNode_tendermint :: TendermintNode
  , _purplechainNode_iavlPorts :: IAVLPorts
  } deriving (Eq, Ord, Show, Generic)

data IAVLPorts = IAVLPorts
  { _iavlPorts_grpc :: Word
  , _iavlPorts_gateway :: Word
  } deriving (Eq, Ord, Show, Generic)

makeLenses ''PurplechainNode
makeLenses ''IAVLPorts

mkPurplechainNode :: NodeEnvironment -> TendermintNode -> PurplechainNode
mkPurplechainNode env tn = PurplechainNode tn iavlPorts
  where
    startingPort = case env of
      NodeEnvironment_Container -> 8090
      NodeEnvironment_Thread -> succ $ snd $ unsafeHostPortFromURI $ tn ^. tendermintNode_config . config_proxyApp
    iavlPorts = IAVLPorts (startingPort + 0) (startingPort + 1)

withPurplechainNode :: MonadIO m => PurplechainNode -> m ()
withPurplechainNode pn = liftIO $ do
  let home = pn ^. purplechainNode_tendermint . tendermintNode_home
  shelly $ sleep 3
  withAsync (runIAVL home $ pn ^. purplechainNode_iavlPorts) $ \_ -> do
    shelly $ sleep 3
    runABCI pn

iavlServerPath :: IsString s => s
iavlServerPath = fromString $(staticWhich "iavlserver")

runIAVL :: Text -> IAVLPorts -> IO ()
runIAVL root (IAVLPorts grpcPort gatewayPort) = do
  let
    iavl = do
      spawnProcess iavlServerPath $ fold
        [ ["-db-name", "test"]
        , ["-datadir", T.unpack root <> "/test.db"]
        , ["-grpc-endpoint", "0.0.0.0" <> ":" <> show grpcPort]
        , ["-gateway-endpoint", "0.0.0.0" <> ":" <> show gatewayPort ]
        ]

    terminateProcess' p = do
      terminateProcess p
      getPid p >>= \case
        Nothing -> pure ()
        Just pid -> void $ createProcess $ shell $ "kill -KILL " <> show pid

  void $ bracket iavl terminateProcess' waitForProcess
    & handle (\(e :: SomeException) -> do
                 putStrLn "ERROR IN RUNIAVL"
                 putStrLn ("::: " <> show e <> " :::")
                 error ""
             )

runABCI :: PurplechainNode -> IO ()
runABCI pn = do
  let (host, port) = unsafeHostPortFromURI $ pn ^. purplechainNode_tendermint . tendermintNode_config . config_proxyApp
  bracket
    (mkContext (pn ^. purplechainNode_iavlPorts . iavlPorts_grpc))
    (\cfg -> K.closeScribes (cfg ^. contextLogConfig . KL.logEnv))
    (serveAppWith (serverSettings (fromEnum port) (fromString . T.unpack $ host)) mempty . makeIOApp)

main :: IO ()
main = do
  getArgs >>= \case
    [home] -> runNodeDir (mkPurplechainNode NodeEnvironment_Container) _purplechainNode_tendermint withPurplechainNode (T.pack home)
    _ -> putStrLn "Usage: purplechain <tendermint-node-directory>"
