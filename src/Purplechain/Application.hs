{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Purplechain.Application where

import Control.Exception
import Control.Lens ((&))
import Data.Proxy
import qualified Data.Text as T

import Database.IAVL.RPC.Types (GrpcConfig (..))
import Network.ABCI.Server.App (App)
import Polysemy (Sem)
import Tendermint
import Tendermint.SDK.BaseApp (CoreEffs, Context, defaultCompileToCore, makeContext, runCoreEffs)
import Tendermint.SDK.BaseApp.Logger.Katip (InitialLogNamespace(..))
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
  makeContext ns Nothing vs (GrpcConfig (T.unpack localhost) (fromIntegral port))
    & handle (\(e :: SomeException) -> do
                 putStrLn "ERROR IN MKCONTEXT"
                 putStrLn ("::: " <> show e <> " :::")
                 error ""
             )
  where
    ns = InitialLogNamespace
      { _initialLogEnvironment = "dev"
      , _initialLogProcessName = "purplechain"
      }
