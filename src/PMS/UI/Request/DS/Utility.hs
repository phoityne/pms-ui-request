{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module PMS.UI.Request.DS.Utility where

import System.Log.FastLogger
import qualified Control.Exception.Safe as E
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.ByteString.Lazy as BSL
import qualified System.IO as S

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM
import PMS.UI.Request.DM.Type

-- |
--
runApp :: DM.DomainData -> AppData -> TimedFastLogger -> AppContext a -> IO (Either DM.ErrorData a)
runApp domDat appDat logger ctx =
  DM.runFastLoggerT domDat logger
    $ runExceptT
    $ flip runReaderT domDat
    $ runReaderT ctx appDat


-- |
--
liftIOE :: IO a -> AppContext a
liftIOE f = liftIO (go f) >>= liftEither
  where
    go :: IO b -> IO (Either String b)
    go x = E.catchAny (Right <$> x) errHdl

    errHdl :: E.SomeException -> IO (Either String a)
    errHdl = return . Left . show

-- |
--
readLine :: S.Handle -> AppContext String
readLine hdl =   isOpenHdl hdl
             >>= isReadableHdl
             >>= isNotEofHdl
             >>= go

  where
    go h = liftIOE $ S.hGetLine h

-- |
--
readLineL :: S.Handle -> AppContext BSL.ByteString
readLineL hdl = readLine hdl >>= return . DM.str2lbs


-- |
--
readChar :: S.Handle -> AppContext String
readChar hdl = isOpenHdl hdl
           >>= isReadableHdl
           >>= isNotEofHdl
           >>= go
           >>= toString
           >>= isNotEmpty

  where
    go h = liftIOE $ S.hGetChar h

    toString c = return [c]


-- |
--
readCharL :: S.Handle -> AppContext BSL.ByteString
readCharL hdl = readCharsL hdl 1


-- |
--
readCharsL :: S.Handle -> Int -> AppContext BSL.ByteString
readCharsL hdl c = isOpenHdl hdl
               >>= isReadableHdl
               >>= isNotEofHdl
               >>= go
               >>= isNotEmptyL

  where
    go h = liftIOE $ BSL.hGet h c


-- |
--
isOpenHdl :: S.Handle -> AppContext S.Handle
isOpenHdl rHdl = liftIOE (S.hIsOpen rHdl) >>= \case
  True  -> return rHdl
  False -> throwError "invalid HANDLE. not opened."


-- |
--
isReadableHdl :: S.Handle -> AppContext S.Handle
isReadableHdl rHdl = liftIOE (S.hIsReadable rHdl) >>= \case
  True  -> return rHdl
  False -> throwError "invalid HANDLE. not readable."


-- |
--
isNotEofHdl :: S.Handle -> AppContext S.Handle
isNotEofHdl rHdl = liftIOE (S.hIsEOF rHdl) >>= \case
  False -> return rHdl
  True  -> throwError "invalid HANDLE. eof."


-- |
--
isNotEmpty :: String -> AppContext String
isNotEmpty b
  | null b = throwError "empty input."
  | otherwise = return b


-- |
--
isNotEmptyL :: BSL.ByteString -> AppContext BSL.ByteString
isNotEmptyL b
  | b == BSL.empty = throwError "empty input."
  | otherwise = return b
