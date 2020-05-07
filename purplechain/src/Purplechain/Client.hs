{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Purplechain.Client
  ( module Maker
  , module Purplechain.Client
  , module Purplechain.Module.Message
  , module Network.Tendermint.Client
  , module Tendermint
  , module Tendermint.SDK.BaseApp.Query
  ) where

import qualified Data.Aeson as Aeson
import           Control.Exception                  (SomeException, handle)
import           Control.Lens                       (strict, (^.), (&))
import           Control.Monad.IO.Class             (MonadIO(..))
import           Control.Monad.Reader               (ReaderT(..))
import qualified Data.ByteArray.Base64String        as Base64
import qualified Data.ByteString.Lazy.Internal      as BS
import           Data.Default.Class                 (def)
import           Data.Proxy                         (Proxy(..))
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as T
import           Data.TreeDiff.Class                (ToExpr, ediff)
import           Data.TreeDiff.Pretty               (ansiWlEditExprCompact)
import           Servant.API                        ((:<|>) (..))
import           Text.PrettyPrint.ANSI.Leijen.Internal (Doc)

import           Maker
import           Network.Tendermint.Client          (health, abciInfo)
import qualified Network.Tendermint.Client          as RPC

import           Tendermint
import           Tendermint.Config
import           Tendermint.SDK.Application.Module  (ApplicationC, ApplicationD, ApplicationQ)
import qualified Tendermint.SDK.BaseApp             as BA
import           Tendermint.SDK.BaseApp.Query       (QueryArgs (..), QueryResult (..))
import qualified Tendermint.SDK.Modules.Auth        as Auth
import qualified Tendermint.SDK.Modules.Bank        as B
import qualified Tendermint.SDK.Types.Address       as Addr
import           Tendermint.Utils.Client            ( ClientConfig (..), EmptyTxClient (..), HasQueryClient (..), HasTxClient (..)
                                                    , Signer, QueryClientResponse(..), TxClientResponse, TxOpts(..), defaultClientTxOpts)
import           Tendermint.Utils.User              (User, makeSignerFromUser, makeUser)

import           Purplechain.Application
import           Purplechain.Module.Message
import           Purplechain.Node

type TxClientM = ReaderT ClientConfig IO

runTxClientM :: RPC.Config -> TxClientM a -> IO a
runTxClientM cfg m = runReaderT m (txClientConfig cfg)

mkRPCConfig :: PurplechainNode -> RPC.Config
mkRPCConfig pn =
  let (host, port) = unsafeHostPortFromURI $ pn ^. purplechainNode_tendermint . tendermintNode_config . config_rpc . configRPC_laddr
  in RPC.defaultConfig (T.encodeUtf8 host) (fromEnum port) True

tx :: MonadIO m => PurplechainNode -> (TxOpts -> msg -> TxClientM a) -> msg -> m a
tx pn mkTx msg = liftIO $ runTxClientM (mkRPCConfig pn) $ mkTx txOpts msg

query :: MonadIO m => PurplechainNode -> RPC.TendermintM a -> m a
query pn q = liftIO $ RPC.runTendermintM (mkRPCConfig pn) q

{- Queries -}
getSystem
  :: QueryArgs ()
  -> RPC.TendermintM (QueryClientResponse System)

getBalance
  :: QueryArgs Addr.Address
  -> Auth.CoinId
  -> RPC.TendermintM (QueryClientResponse Auth.Coin)

getAccount
  :: QueryArgs Addr.Address
  -> RPC.TendermintM (QueryClientResponse Auth.Account)

getSystem :<|> getBalance :<|> getAccount = genClientQ
  Proxy
  (Proxy @(ApplicationQ PurplechainModules))
  def

{- Transactions -}
performActTx
  :: TxOpts -> PerformMsg -> TxClientM (TxClientResponse () ())

transferTx
  :: TxOpts -> B.TransferMsg -> TxClientM (TxClientResponse () ())

burnTx
  :: TxOpts -> B.BurnMsg -> TxClientM (TxClientResponse () ())

performActTx :<|> (burnTx :<|> transferTx) :<|> EmptyTxClient = genClientT
  Proxy
  (Proxy @(ApplicationC PurplechainModules))
  (Proxy @(ApplicationD PurplechainModules))
  defaultClientTxOpts

{- Test utils -}
user1 :: User
user1 = makeUser "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

signer1 :: Signer
signer1 = makeSignerFromUser user1

txOpts :: TxOpts
txOpts = TxOpts
  { txOptsSigner = signer1
  , txOptsGas = 0
  }

txClientConfig :: RPC.Config -> ClientConfig
txClientConfig cfg =
  let getNonce addr = do
        resp <- RPC.runTendermintM cfg $ getAccount $
          QueryArgs
            { queryArgsHeight = -1
            , queryArgsProve = False
            , queryArgsData = addr
            }
        -- @NOTE: TxNonce should be +1 of accountNonce
        case resp of
          QueryError e ->
            if BA.appErrorCode e == 2
              then pure 1
              else error $ "Unknown nonce error: " <> show (BA.appErrorMessage e)
          QueryResponse qr ->
            pure $ 1 + Auth.accountNonce (queryResultData qr)

  in ClientConfig
       { clientGetNonce = getNonce
       , clientRPC = cfg
       }


diff :: ToExpr a => a -> a -> Doc
diff old new = ansiWlEditExprCompact $ ediff old new

unQuery :: QueryClientResponse System -> Maybe System
unQuery = \case
  QueryError _ -> Nothing
  QueryResponse r -> Just $ queryResultData r

{- Tendermint layer transactions -}
broadcastAct :: MonadIO m => Text -> Word -> Act -> m ()
broadcastAct host port = broadcastTransaction host port . Aeson.encode

broadcastTransaction :: MonadIO m => Text -> Word -> BS.ByteString -> m ()
broadcastTransaction host port bs = liftIO $ do
  let
    cfg = RPC.defaultConfig (T.encodeUtf8 host) (fromEnum port) True
    broadcast = RPC.runTendermintM cfg $ RPC.broadcastTxCommit $ RPC.RequestBroadcastTxCommit $ Base64.fromBytes $ bs ^. strict

  r <- fmap Right broadcast
    & handle @SomeException (pure . Left)
  case r of
    Left err -> print err
    Right _ -> pure ()
