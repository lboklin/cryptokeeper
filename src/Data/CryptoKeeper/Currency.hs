{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoKeeper.Currency where

import Data.Aeson
import Data.Monoid ((<>))
import Data.Text as Text

type CurrencyPair = (String, String)

data Money =
  Money Float
        Currency
  deriving (Read)

instance Show Money where
  show (Money amount currency) = show amount ++ " " ++ show currency

data Currency
  = EUR
  | USD
  | XBT
  | XMR
  | ETH
  | AION
  | ENG
  | ADA
  deriving (Read, Show, Eq, Ord)

instance ToJSON Money where
  toJSON (Money amount currency) =
    object ["amount" .= amount, "currency" .= currency]
  toEncoding (Money amount currency) =
    pairs ("amount" .= amount <> "currency" .= currency)

instance FromJSON Money where
  parseJSON =
    withObject "Money" $ \v -> Money <$> (v .: "amount") <*> (v .: "currency")

instance ToJSON Currency where
  toJSON currency = object ["currency" .= Text.pack (show currency)]
  toEncoding currency = pairs ("currency" .= Text.pack (show currency))

instance FromJSON Currency where
  parseJSON =
    withObject "Currency" $ \v -> read . Text.unpack <$> (v .: "currency")

toCurrency :: String -> Maybe Currency
toCurrency str =
  case str of
    "EUR" -> Just EUR
    "eur" -> Just EUR
    "USD" -> Just USD
    "usd" -> Just USD
    "XBT" -> Just XBT
    "xbt" -> Just XBT
    "BTC" -> Just XBT
    "btc" -> Just XBT
    "XMR" -> Just XMR
    "xmr" -> Just XMR
    "ETH" -> Just ETH
    "eth" -> Just ETH
    "AION" -> Just AION
    "aion" -> Just AION
    "ENG" -> Just ENG
    "eng" -> Just ENG
    "ADA" -> Just ADA
    "ada" -> Just ADA
    _ -> Nothing

fromCurrency :: Currency -> Text
fromCurrency c = pack $ show c

recognizedCurrencies :: String
recognizedCurrencies = "EUR, USD, XBT, XMR, ETH, AION, ENG, ADA"
