{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.UI.Request.DS.Core where

import Control.Monad.Logger
import Data.Conduit
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T
--import Text.Parsec
import Data.Aeson
import Control.Monad.Except

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.UI.Request.DM.Type
import PMS.UI.Request.DS.Utility

-- |
--
app :: AppContext ()
app = do
  $logDebugS DM._LOGTAG "app called."
  runConduit pipeline
  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| work .| sink

---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () B.ByteString AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext B.ByteString
    go = do
      hdl <- view inputHandleAppData <$> ask
      bs <- readLineL hdl
      $logDebugS DM._LOGTAG $ T.pack $ "src: " ++ DM.lbs2str bs
      return bs

---------------------------------------------------------------------------------
-- |
--
work :: ConduitT B.ByteString DM.JsonRpcRequest AppContext ()
work = await >>= \case
  Just reqBS -> flip catchError errHdl $ do
    lift (go reqBS) >>= yield >> work
  Nothing  -> do
    $logWarnS DM._LOGTAG "work: await returns nothing. skip."
    work
    
  where
    errHdl :: String -> ConduitT B.ByteString DM.JsonRpcRequest AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "work: exception. skip. " ++ msg
      work

    go :: B.ByteString -> AppContext DM.JsonRpcRequest
    go = liftEither . eitherDecode

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT DM.JsonRpcRequest Void AppContext ()
sink = await >>= \case
  Just req -> flip catchError errHdl $ do
    lift (go req) >> sink
  Nothing  -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    errHdl :: String -> ConduitT DM.JsonRpcRequest Void AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "sink: exception skip. " ++ msg
      sink

    go :: DM.JsonRpcRequest -> AppContext ()
    go req = do
      let method  = req^.DM.methodJsonRpcRequest
          rawJson = req^.DM.paramsJsonRpcRequest
      reqDat <- decodeReq method rawJson req
      enq reqDat

-- |
--
enq :: DM.McpRequest -> AppContext ()
enq req = do
  queue <- view DM.requestQueueDomainData <$> lift ask
  liftIO $ STM.atomically $ STM.writeTQueue queue req

-- |
--
-- {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{"roots":{"listChanged":true}},"clientInfo":{"name":"Visual Studio Code","version":"1.100.2"}}} @(pms-ui-request-0.1.0.0-d3d6f94ec8cad8f11e6c82557d1e8927ebf7b47db964c68a0c1101f4c8759996:PMS.UI.Request.DS.Core src/PTY/UI/Request/Core.hs:47:8)
-- {"method":"notifications/initialized","jsonrpc":"2.0"} @(pms-ui-request-0.1.0.0-d3d6f94ec8cad8f11e6c82557d1e8927ebf7b47db964c68a0c1101f4c8759996:PMS.UI.Request.DS.Core src/PTY/UI/Request/Core.hs:47:8)
-- {"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}} @(pms-ui-request-0.1.0.0-d3d6f94ec8cad8f11e6c82557d1e8927ebf7b47db964c68a0c1101f4c8759996:PMS.UI.Request.DS.Core src/PTY/UI/Request/Core.hs:47:8)
-- {"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"df","arguments":{"arg1":""},"_meta":{"progressToken":"df68feeb-d5b0-4807-abac-ad2eb5d628bd"}}} @(pms-ui-request-0.1.0.0-d3d6f94ec8cad8f11e6c82557d1e8927ebf7b47db964c68a0c1101f4c8759996:PMS.UI.Request.DS.Core src/PTY/UI/Request/Core.hs:47:8)
-- {"jsonrpc":"2.0","id":14,"method":"completion/complete","params":{"ref":{"type":"ref/prompt","name":"summarize_text"},"argument":{"name":"param2","value":"1200"},"context":{"arguments":{"param1":"abc"}}}}
-- {"method":"notifications/cancelled","params":{"requestId":14},"jsonrpc":"2.0"}
-- {"jsonrpc":"2.0","id":15,"method":"prompts/get","params":{"name":"summarize_text","arguments":{"param1":"abc","param2":"1200"}}}
--
decodeReq :: String -> Maybe DM.RawJsonByteString -> DM.JsonRpcRequest -> AppContext DM.McpRequest
decodeReq "initialize" (Just (DM.RawJsonByteString rawJson)) req = DM.McpInitializeRequest . DM.McpInitializeRequestData req <$> (liftEither (eitherDecode rawJson))
decodeReq "tools/list" _ req = pure . DM.McpToolsListRequest . DM.McpToolsListRequestData $ req
decodeReq "tools/call" (Just (DM.RawJsonByteString rawJson)) req = DM.McpToolsCallRequest . DM.McpToolsCallRequestData req <$> (liftEither (eitherDecode rawJson))
decodeReq "prompts/list" _ req = pure . DM.McpPromptsListRequest . DM.McpPromptsListRequestData $ req
decodeReq "prompts/get" (Just (DM.RawJsonByteString rawJson)) req = DM.McpPromptsGetRequest . DM.McpPromptsGetRequestData req <$> (liftEither (eitherDecode rawJson))
decodeReq "resources/templates/list" _ req = pure . DM.McpResourcesTemplatesListRequest . DM.McpResourcesTemplatesListRequestData $ req
decodeReq "resources/list" _ req = pure . DM.McpResourcesListRequest . DM.McpResourcesListRequestData $ req
decodeReq "resources/read" (Just (DM.RawJsonByteString rawJson)) req = DM.McpResourcesReadRequest . DM.McpResourcesReadRequestData req <$> (liftEither (eitherDecode rawJson))
decodeReq "notifications/initialized" Nothing req = pure . DM.McpInitializedNotification . DM.McpInitializedNotificationData $ req
decodeReq "notifications/cancelled" (Just (DM.RawJsonByteString rawJson)) req = DM.McpCancelledNotification . DM.McpCancelledNotificationData req <$> (liftEither (eitherDecode rawJson))
decodeReq "completion/complete" (Just (DM.RawJsonByteString rawJson)) req = DM.McpCompletionCompleteRequest . DM.McpCompletionCompleteRequestData req <$> (liftEither (eitherDecode rawJson))
decodeReq _ _ req = throwError $ "unsupported method: " ++ show req

