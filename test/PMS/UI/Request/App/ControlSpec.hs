{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE MultilineStrings #-}

module PMS.UI.Request.App.ControlSpec (spec) where

import Test.Hspec
import Data.Default
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import System.IO
import System.Posix.IO
import Control.Lens

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.UI.Request.App.Control as SUT
import qualified PMS.UI.Request.DM.Type as SUT

-- |
--
data SpecContext = SpecContext {
                   _handlePairSpecContext :: (Handle, Handle) 
                 , _domainDataSpecContext :: DM.DomainData
                 , _appDataSpecContext :: SUT.AppData
                 }

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  domDat <- DM.defaultDomainData
  let appDat = def
  return SpecContext {
           _handlePairSpecContext = (stdin, stdout) 
         , _domainDataSpecContext = domDat
         , _appDataSpecContext    = appDat
         }

-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Spec."
  beforeAll setUpOnce $ 
    afterAll tearDownOnce . 
      beforeWith setUp . 
        after tearDown $ run

-- |
--
setUpOnce :: IO SpecContext
setUpOnce = do
  putStrLn "[INFO] EXECUTED ONLY ONCE BEFORE ALL TESTS START."
  defaultSpecContext

-- |
--
tearDownOnce :: SpecContext -> IO ()
tearDownOnce _ = do
  putStrLn "[INFO] EXECUTED ONLY ONCE AFTER ALL TESTS FINISH."

-- |
--
setUp :: SpecContext -> IO SpecContext
setUp ctx = do
  putStrLn "[INFO] EXECUTED BEFORE EACH TEST STARTS."

  (readFd, writeFd) <- createPipe
  readH  <- fdToHandle readFd
  writeH <- fdToHandle writeFd
  hSetBuffering readH NoBuffering
  hSetBuffering writeH NoBuffering

  domDat <- DM.defaultDomainData
  let appDat = ctx^.appDataSpecContext
  return ctx {
                _handlePairSpecContext = (readH, writeH)
              , _domainDataSpecContext = domDat
              , _appDataSpecContext    = appDat {SUT._inputHandleAppData = readH}
              }

-- |
--
tearDown :: SpecContext -> IO ()
tearDown ctx = do
  putStrLn "[INFO] EXECUTED AFTER EACH TEST FINISHES."
  hClose $ fst $ ctx^.handlePairSpecContext
  hClose $ snd $ ctx^.handlePairSpecContext

-- |
--
run :: SpecWith SpecContext
run = do
  describe "runWithAppData" $ do
    context "when initialize" $ do
      it "should be McpInitializeRequest" $ \ctx -> do 
        putStrLn "[INFO] EXECUTING THE FIRST TEST."

        let writeH = snd $ ctx^.handlePairSpecContext
            domDat = ctx^.domainDataSpecContext
            appDat = ctx^.appDataSpecContext
            reqQ   = domDat^.DM.requestQueueDomainData
            input  = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"capabilities\":{\"roots\":{\"listChanged\":true}},\"clientInfo\":{\"name\":\"Visual Studio Code\",\"version\":\"1.99.2\"},\"protocolVersion\":\"2024-11-05\"}}"
            expect = "2.0"            
            
        thId <- async $ SUT.runWithAppData appDat domDat

        hPutStr writeH input
        hPutStr writeH "\n"
        hFlush  writeH

        (DM.McpInitializeRequest dat) <- STM.atomically $ STM.readTQueue reqQ
        let actual = dat^.DM.jsonrpcMcpInitializeRequestData^.DM.jsonrpcJsonRpcRequest
        actual `shouldBe` expect

        cancel thId

      