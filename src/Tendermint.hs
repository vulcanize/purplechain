{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Tendermint where

import Control.Concurrent.Async                         (forConcurrently_, withAsync)
import Control.Exception                                (IOException, handle)
import Control.Lens                                     (_Right, _Just, imap, makeLenses, (&), (^.), (.~), (%~), (^?))
import Control.Monad.IO.Class                           (MonadIO(..))
import Data.Bool                                        (bool)
import Data.Functor                                     (void)
import Data.Maybe                                       (fromMaybe)
import Data.Text                                        (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable                                 (for)
import GHC.Generics                                     (Generic)
import Shelly                                           (Sh, cp, pwd, run, shelly, silently, toTextIgnore, withTmpDir, (</>))
import qualified Shelly as Sh
import System.IO                                        (BufferMode (..), hSetBuffering, stderr, stdout)
import System.Which                                     (staticWhich)
import Text.Read                                        (readMaybe)
import Text.URI                                         (Authority(Authority), RText, RTextLabel(Host), URI, mkHost, unRText)
import Text.URI.Lens                                    (authHost, authPort, uriAuthority)

import Tendermint.Config                                hiding (uri)


{- Utils -}

tshow :: Show a => a -> Text
tshow = T.pack . show

logger :: MonadIO m => Text -> m ()
logger = liftIO . T.putStrLn . ("[PURPLE] \t" <>)

initProcess :: MonadIO m => m ()
initProcess = liftIO $ do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

localhost :: Text
localhost = "127.0.0.1"

localhostRText :: RText 'Host
localhostRText = fromMaybe (error "localhostRText: invalid IP") $ mkHost localhost

unsafeHostPortFromURI :: URI -> (Text, Word)
unsafeHostPortFromURI uri = (h,p)
  where
    err component = error $ "unsafeHostPortFromURI: URI did not contain " <> component
    Authority _ host port = fromMaybe (err "Authority") $ uri ^. uriAuthority ^? _Right
    p = fromMaybe (err "Port") port
    h = unRText host

cleave :: Text -> Text -> (Text, Text)
cleave sep str =
  let (a,b) = T.breakOn sep str
  in (a, T.drop (T.length sep) b)



{- CLI -}

data AppNetwork a = AppNetwork
  { _appNetwork_toAppNode :: TendermintNode -> a
  , _appNetwork_fromAppNode :: a -> TendermintNode
  , _appNetwork_withNode :: a -> IO ()
  , _appNetwork_size :: Word
  }

data TendermintNode = TendermintNode
  { _tendermintNode_home        :: Text
  , _tendermintNode_config      :: Config
  , _tendermintNode_id          :: Text
  } deriving (Eq, Ord, Show, Generic)

newtype GlobalFlags = GlobalFlags
  { _globalFlags_home :: Text
  } deriving (Eq, Ord, Read, Show, Generic)

data NetworkFlags = NetworkFlags
  { _networkFlags_validators    :: Word
  , _networkFlags_output        :: Text
  , _networkFlags_populatePeers :: Bool
  } deriving (Eq, Ord, Read, Show, Generic)

concat <$> traverse makeLenses
  [ ''TendermintNode
  , ''GlobalFlags
  , ''NetworkFlags
  ]

mkNetworkFlags :: Text -> Word -> NetworkFlags
mkNetworkFlags networkRoot size = NetworkFlags
  { _networkFlags_validators    = size
  , _networkFlags_output        = networkRoot
  , _networkFlags_populatePeers = True
  }

loadTendermintNode :: MonadIO m => Text -> m TendermintNode
loadTendermintNode home = pure (TendermintNode home)
  <*> loadConfigFile (T.unpack $ home <> "/config" <> "/config.toml")
  <*> shelly (silently $ fmap T.strip $ tendermint (GlobalFlags home) "show_node_id" [])

storeConfig :: MonadIO m => Text -> Config -> m ()
storeConfig home = storeConfigFile (T.unpack $ home <> "/config" <> "/config.toml")

genesisFile :: TendermintNode -> Sh.FilePath
genesisFile n = configDir n </> ("genesis.json" :: Text)

configDir :: TendermintNode -> Sh.FilePath
configDir n = _tendermintNode_home n </> ("config" :: Text)

tendermintPath :: Sh.FilePath
tendermintPath = Sh.fromText $ T.pack $(staticWhich "tendermint")

tendermint :: GlobalFlags -> Text -> [Text] -> Sh Text
tendermint gf tmCmd cmdArgs = run tendermintPath $ tmArgs <> [tmCmd] <> cmdArgs
  where
    tmArgs = ["--home", _globalFlags_home gf]

tendermintNetwork :: NetworkFlags -> Sh Text
tendermintNetwork nf = run tendermintPath $ ("testnet" :) $
  [ "--v", tshow $ _networkFlags_validators nf
  , "--o", _networkFlags_output nf
  , "--starting-ip-address", localhost
  ] <> bool [] ["--populate-persistent-peers"] (_networkFlags_populatePeers nf)

tendermintNode :: GlobalFlags -> Sh Text
tendermintNode gf = tendermint gf "node" []

data NodePorts = NodePorts
  { _nodePorts_p2p :: Word
  , _nodePorts_rpc :: Word
  , _nodePorts_abci :: Word
  } deriving (Eq, Ord, Read, Show, Generic)

nodePortsOffset :: Word
nodePortsOffset = 10

nthNodePorts :: Word -> NodePorts
nthNodePorts index =
  let offset = 26656 + nodePortsOffset * index
  in NodePorts offset (offset + 1) (offset + 2)

extraNodePorts :: NodePorts
extraNodePorts = nthNodePorts (negate 1)

addTendermintNode :: MonadIO m => Text -> Text -> NodePorts -> TendermintNode -> m TendermintNode
addTendermintNode home moniker ports preExistingNode = shelly $ do
  void $ tendermint (GlobalFlags home) "init" []
  n <- loadTendermintNode home

  cp (genesisFile preExistingNode) (configDir n)
  let
    newCfg = _tendermintNode_config preExistingNode
      & config_moniker .~ moniker
      & updatePorts ports
  storeConfig home newCfg

  pure $ n &
    tendermintNode_config .~ newCfg


runTendermintNodeDir :: MonadIO m  => (TendermintNode -> IO ()) -> Text -> m ()
runTendermintNodeDir = runNodeDir id id

runNodeDir
  :: MonadIO m
  => (TendermintNode -> appNode)
  -> (appNode -> TendermintNode)
  -> (appNode -> IO ())
  -> Text
  -> m ()
runNodeDir toAppNode fromAppNode withNode dir = loadTendermintNode dir >>= runNode fromAppNode withNode . toAppNode

runTendermintNode :: MonadIO m => (TendermintNode -> IO ()) -> TendermintNode -> m ()
runTendermintNode = runNode id

runNode :: MonadIO m => (node -> TendermintNode) -> (node -> IO ()) -> node -> m ()
runNode fromAppNode withNode n = void $ do
  initProcess

  liftIO $ withAsync (withNode n) $ \_ -> do
    logger ("Launching " <> fromAppNode n ^. tendermintNode_config . config_moniker)
    shelly $ tendermintNode $ GlobalFlags $ _tendermintNode_home $ fromAppNode n

loadNetwork :: MonadIO m => Text -> Word -> m (Either IOException [TendermintNode])
loadNetwork root size = liftIO $ handle onFailure $ do
  fmap Right $ for [0..size-1] $ \i -> do
    loadTendermintNode $ root <> "/node" <> tshow i
  where
    onFailure (e :: IOException) = pure $ Left e


initNetwork :: MonadIO m => Text -> Word -> m [TendermintNode]
initNetwork root size = shelly $ do
  void $ tendermintNetwork $ mkNetworkFlags root size
  for [0..size-1] $ \i -> do
    let
      home = root <> "/node" <> tshow i
    n <- loadTendermintNode home
    let
      imapWord f = imap $ \x y -> f (toEnum x) y
      oldCfg = n ^. tendermintNode_config
      ports = nthNodePorts i

      peers = T.splitOn "," $ oldCfg ^. config_p2p . configP2P_persistentPeers
      peers' = T.intercalate "," $ flip imapWord peers $ \j peer ->
        let (nid, rest) = cleave "@" peer
            (_host, port) = cleave ":" rest
            port' = case readMaybe (T.unpack port) of
              Nothing -> error "parsing error"
              Just p -> p + j * nodePortsOffset
        in nid <> "@" <> localhost <> ":" <> tshow port'

    let cfg = oldCfg
          & config_moniker .~ "node" <> tshow i
          & updatePorts ports
          & config_p2p . configP2P_persistentPeers .~ peers'
          & config_p2p . configP2P_privatePeerIds .~ peers'
          & config_p2p . configP2P_addrBookStrict .~ False
          & config_p2p . configP2P_allowDuplicateIp .~ True
          & config_consensus . configConsensus_createEmptyBlocksInterval .~ "60s"

    storeConfig home cfg
    pure $ n & tendermintNode_config .~ cfg

updatePorts :: NodePorts -> Config -> Config
updatePorts (NodePorts p2p rpc abci) cfg = cfg
  & config_p2p . configP2P_laddr %~ f p2p
  & config_rpc . configRPC_laddr %~ f rpc
  & config_proxyApp %~ f abci
  where
    f p h = h
      & uriAuthority . _Right . authPort . _Just .~ p
      & uriAuthority . _Right . authHost .~ localhostRText

withTempDir :: MonadIO m => (Text -> IO a) -> m a
withTempDir f = shelly . withTmpDir $ liftIO . f . toTextIgnore

withCurrentDir :: MonadIO m => (Text -> IO a) -> m a
withCurrentDir f = shelly $ pwd >>= liftIO . f . toTextIgnore

withNetwork :: Text -> AppNetwork node -> (Text -> [node] -> IO ()) -> IO ()
withNetwork root net f = do
  let size = _appNetwork_size net
      report outcome = logger (outcome <> " network of size " <> tshow size <> " at " <> root)
      toAppNodes = fmap (_appNetwork_toAppNode net)
      launchNodes ns = forConcurrently_ ns $
        runNode (_appNetwork_fromAppNode net) (_appNetwork_withNode net)

  loadNetwork root size >>= \case
    Left _ -> do
      genesisNodes <- toAppNodes <$> initNetwork root size
      report "Initialized"
      liftIO $ withAsync (f root genesisNodes) $ \_ ->
        launchNodes genesisNodes
    Right ns -> do
      report "Restored"
      launchNodes $ toAppNodes ns

withThrowawayNetwork :: AppNetwork node -> (Text -> [node] -> IO ()) -> IO ()
withThrowawayNetwork net f = withTempDir $ \x -> withNetwork x net f

withLocalNetwork :: AppNetwork node -> (Text -> [node] -> IO ()) -> IO ()
withLocalNetwork net f = withCurrentDir $ \x -> withNetwork x net f
