{- Jumpstarted with https://swizec.com/blog/writing-a-rest-client-in-haskell/swizec/6152 -}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Data.Aeson
import Data.Either.Compat (fromRight)
import Data.Maybe (fromMaybe)

import Data.CryptoKeeper.IO
import Data.CryptoKeeper.IO.Prompts
import Data.CryptoKeeper.Tx (Tx)
import Data.CryptoKeeper.Util (eitherToMaybe)

dataFile :: [Tx] -> DataFile
dataFile txs =
  DataFile
    {appName = "cryptokeeper", fileName = "transactions.json", contents = txs}

main :: IO ()
main = do
  ans <- putQuestion "Would you like to input a new transaction?" No
  print ans
  if ans == Yes
    then do
      hist <- fromMaybe [] . eitherToMaybe <$> readDataFile (dataFile [])
      inputNewTransactions hist
    else printHistory

printHistory :: IO ()
printHistory = do
  hist <- readDataFile (dataFile []) :: IO (Either String [Tx])
  let dfPath = show $ dataFilePath $ dataFile []
  putStrLn $ "Printing everything stored in " ++ dfPath ++ ":"
  putStrLn . unlines . map show $ fromRight [] hist

inputNewTransactions :: [Tx] -> IO ()
inputNewTransactions hist = do
  newTxs <- newTransactions
  putStrLn "Writing to file.."
  writeToDataFile $ dataFile (newTxs ++ hist)
  putStrLn "Wrote to file."
  print . show $ newTxs
