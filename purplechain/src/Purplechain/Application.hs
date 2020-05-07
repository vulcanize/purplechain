{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Purplechain.Application where

import Control.Exception
import Control.Lens ((&), (.~), (^.))
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.IO (stdout)

import Database.IAVL.RPC.Types (GrpcConfig (..))
import qualified Katip as K
import Network.ABCI.Server.App (App)
import Polysemy (Sem)
import Tendermint
import Tendermint.SDK.BaseApp (CoreEffs, Context, contextLogConfig, defaultCompileToCore, makeContext, runCoreEffs)
import Tendermint.SDK.BaseApp.Logger.Katip (InitialLogNamespace(..), logEnv)
import Tendermint.SDK.BaseApp.Store.IAVLStore (initIAVLVersions)
import Tendermint.SDK.Modules.Auth (Auth, authModule)
import Tendermint.SDK.Application (ModuleList(..), HandlersContext(..), baseAppAnteHandler, makeApp, createIOApp)
import Tendermint.SDK.Crypto (Secp256k1)
import Tendermint.SDK.Modules.Bank (Bank, bankModule)

import Purplechain.Module (Purplechain, purplechainModule)

type PurplechainModules =
   '[ Purplechain
    , Bank
    , Auth
    ]

handlersContext :: HandlersContext Secp256k1 PurplechainModules CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy
  , modules = purplechainModules
  , compileToCore  = defaultCompileToCore
  , anteHandler = baseAppAnteHandler
  }
  where
  purplechainModules =
       purplechainModule
    :+ bankModule
    :+ authModule
    :+ NilModules

app :: App (Sem CoreEffs)
app = makeApp handlersContext

makeIOApp :: Context -> App IO
makeIOApp ctx = createIOApp (runCoreEffs ctx) app

mkContext :: Word -> IO Context
mkContext port = do
  vs <- initIAVLVersions
  ctx <- makeContext ns Nothing vs (GrpcConfig (T.unpack localhost) (fromIntegral port))
    & handle (\(e :: SomeException) -> do
                 putStrLn "ERROR IN MKCONTEXT"
                 putStrLn ("::: " <> show e <> " :::")
                 error ""
             )
  (severity, verbosity) <- lookupEnvVars
  scribesLogEnv <- makeKatipScribe (fromMaybe K.InfoS severity) (fromMaybe K.V0 verbosity) $ ctx ^. contextLogConfig . logEnv
  pure $ ctx & contextLogConfig . logEnv .~ scribesLogEnv

  where
    ns = InitialLogNamespace
      { _initialLogEnvironment = "dev"
      , _initialLogProcessName = "purplechain"
      }

makeKatipScribe
  :: K.Severity
  -> K.Verbosity
  -> K.LogEnv
  -> IO K.LogEnv
makeKatipScribe severity verbosity le = do
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem severity) verbosity
  K.registerScribe "stdout" handleScribe K.defaultScribeSettings le

lookupEnvVars :: IO (Maybe K.Severity, Maybe K.Verbosity)
lookupEnvVars = do
  sev <- lookupEnv "LOG_SEVERITY"
  verb <- lookupEnv "LOG_VERBOSITY"
  pure ( parseSeverity =<< sev
       , parseVerbosity =<< verb
       )
  where
    parseSeverity = K.textToSeverity . T.pack
    parseVerbosity = \case
      "0" -> Just K.V0
      "1" -> Just K.V1
      "2" -> Just K.V2
      "3" -> Just K.V3
      _ -> Nothing
