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
import Shelly                                           (shelly, sleep, run)
import qualified Shelly                                 as Sh
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

mkPurplechainNode :: TendermintNode -> PurplechainNode
mkPurplechainNode tn = PurplechainNode tn iavlPorts
  where
    iavlPorts = IAVLPorts (proxyAppPort + 1) (proxyAppPort + 2)
    (_, proxyAppPort) = unsafeHostPortFromURI $ tn ^. tendermintNode_config . config_proxyApp

withPurplechainNode :: MonadIO m => PurplechainNode -> m ()
withPurplechainNode pn = liftIO $ do
  let home = pn ^. purplechainNode_tendermint . tendermintNode_home
  shelly $ sleep 3
  withAsync (runIAVL home $ pn ^. purplechainNode_iavlPorts) $ \_ -> do
    shelly $ sleep 3
    runABCI pn

iavlServerPath :: Sh.FilePath
iavlServerPath = Sh.fromText $ T.pack $(staticWhich "iavlserver")

runIAVL :: Text -> IAVLPorts -> IO ()
runIAVL root (IAVLPorts grpcPort gatewayPort) = do
  let iavl = print <=< shelly $ run iavlServerPath $ fold
        [ ["-db-name", "test"]
        , ["-datadir", root <> "/test.db"]
        , ["-grpc-endpoint", "0.0.0.0" <> ":" <> tshow grpcPort]
        , ["-gateway-endpoint", "0.0.0.0" <> ":" <> tshow gatewayPort ]
        ]
  iavl
    & handle (\(e :: SomeException) -> do
                 putStrLn "ERROR IN RUNIAVL"
                 putStrLn ("::: " <> show e <> " :::")
                 error ""
             )
  pure ()

runABCI :: PurplechainNode -> IO ()
runABCI pn = do
  let (host, port) = unsafeHostPortFromURI $ pn ^. purplechainNode_tendermint . tendermintNode_config . config_proxyApp
  bracket
    (mkContext (pn ^. purplechainNode_iavlPorts . iavlPorts_grpc))
    (\cfg -> K.closeScribes (cfg ^. contextLogConfig . KL.logEnv))
    (serveAppWith (serverSettings (fromEnum port) (fromString . T.unpack $ host)) mempty . makeIOApp)
