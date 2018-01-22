{-
    The LANGUAGE pragma and Generic instance let us write empty FromJSON and
    ToJSON instances for which the compiler will generate sensible default
    implementations.
-}
--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoKeeper.JSON where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Conduit (ConduitM, (.|), runConduitRes)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Binary (sourceFile)
import Data.CryptoKeeper.IO (debugLog)

sinkFromJSON :: (MonadThrow m, FromJSON a) => ConduitM BS.ByteString o m a
sinkFromJSON = do
  value <- sinkParser json
  case fromJSON value of
    Error e -> throwM $ userError e
    Success x -> return x

readJSONFile :: (MonadIO m, FromJSON a) => FilePath -> m a
readJSONFile fp = liftIO $ runConduitRes $ sourceFile fp .| sinkFromJSON

readJSONFileStrict :: (MonadIO m, FromJSON a) => FilePath -> m a
readJSONFileStrict fp =
  liftIO $ do
    putStrLn $ "Attempting to read from " ++ show fp
    bs <- BS.readFile fp >>= debugLog "readFile fp" :: IO BS.ByteString
    case eitherDecodeStrict' bs of
      Left e -> throwM $ userError e
      Right x -> return x
