{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoKeeper.Network.Api where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Currency
import Data.Maybe (fromMaybe)
import Network.HTTP.Conduit (simpleHttp)
import Text.Read (readMaybe)

data Change = Change
  { priceChange :: String
  , priceChangePercent :: String
  , lastPrice :: String
  , highPrice :: String
  , lowPrice :: String
  } deriving (Show)

instance FromJSON Change where
  parseJSON =
    withObject "Change" $ \v ->
      Change
        <$> v .: "priceChange"
        <*> v .: "priceChangePercent"
        <*> v .: "lastPrice"
        <*> v .: "highPrice"
        <*> v .: "lowPrice"

fetchLastPc :: CurrencyPair -> IO Float
fetchLastPc symbol = do
  lstPrice <- fetchChangeFloatValue symbol priceChangePercent
  return $ fromMaybe 0 lstPrice

fetchLastPrice :: CurrencyPair -> IO Float
fetchLastPrice symbol = do
  lstPrice <- fetchChangeFloatValue symbol lastPrice
  return $ fromMaybe 0 lstPrice

fetchChangeFloatValue :: CurrencyPair -> (Change -> String) -> IO (Maybe Float)
fetchChangeFloatValue symbol f = do
  chStr <- changeInLastDay $ uncurry (++) symbol :: IO (Maybe Change)
  let change = fmap f chStr >>= readMaybe
  return change

changeInLastDay :: MonadIO m => String -> m (Maybe Change)
changeInLastDay sym =
  fmap decode . fetchFromBinance $ "api/v1/ticker/24hr?symbol=" ++ sym

fetchFromBinance :: MonadIO m => String -> m ByteString
fetchFromBinance url = simpleHttp $ "https://api.binance.com/" ++ url
