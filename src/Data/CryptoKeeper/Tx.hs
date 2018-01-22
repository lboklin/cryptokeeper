{-
    The LANGUAGE pragma and Generic instance let us write empty FromJSON and
    ToJSON instances for which the compiler will generate sensible default
    implementations.
-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoKeeper.Tx where

import Data.Aeson
import Data.CryptoKeeper.Currency

data Tx = Tx
  { paid :: Money
  , recieved :: Money
  , txDescription :: String
  } deriving (Read)

instance ToJSON Tx where
  toJSON (Tx pd rcv desc) =
    object ["paid" .= pd, "recieved" .= rcv, "txDescription" .= desc]

instance FromJSON Tx where
  parseJSON =
    withObject "Tx" $ \v ->
      Tx <$> v .: "paid" <*> v .: "recieved" <*> v .: "txDescription"

instance Show Tx where
  show (Tx pd rcv desc) = conv ++ txf ++ dsc
    where
      frm = show pd
      to = show rcv
      conv = frm ++ " -> " ++ to
      txf = " (Value diff: " ++ "N/A" ++ ")"
      dsc =
        if desc == ""
          then ""
          else " -- " ++ desc
