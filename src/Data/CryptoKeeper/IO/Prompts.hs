module Data.CryptoKeeper.IO.Prompts where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w)
import Data.CryptoKeeper.Currency
import Data.CryptoKeeper.Tx (Tx(..))
import Data.CryptoKeeper.Util (fromByteString', imapM_)

-- import Data.CryptoKeeper.Util ()
import Text.Read (readMaybe)

data Answer
  = Yes
  | No
  deriving (Show, Read, Eq)

data Option a =
  Option String
         (IO a)

getYesNo :: Answer -> IO Answer
getYesNo defaultAns = do
  ans <- getChar
  case ans of
    'y' -> return Yes
    'Y' -> return Yes
    'n' -> return No
    'N' -> return No
    '\n' -> return defaultAns
    _ -> do
      putStrLn "Please enter \"y\" or \"n\""
      getYesNo defaultAns

procOpt :: Option a -> IO a
procOpt (Option _ a) = a

printOpt :: Int -> Option () -> IO ()
printOpt i (Option qStr _) = putStrLn $ show i ++ ": " ++ qStr

putOptions :: [Option ()] -> Maybe Int -> IO ()
putOptions options defaultOpt = do
  imapM_ printOpt options
  optN <-
    getValidInput
      "Choose option: "
      (defaultOpt, "Please enter the number of the desired option: ")
  procOpt $ options !! optN

getValidInput :: Read a => String -> (Maybe a, String) -> IO a
getValidInput qMsg (maybeFallback, failMsg) = do
  putStrLn qMsg
  inp <- getLine
  case readMaybe inp of
    Just a -> return a
    Nothing ->
      case maybeFallback of
        Just fb -> return fb
        Nothing -> do
          putStrLn failMsg
          getValidInput qMsg (Nothing, failMsg)

putQuestion :: String -> Answer -> IO Answer
putQuestion str defaultAns = do
  let suffix =
        if defaultAns == Yes
          then " (Y/n)"
          else " (y/N)"
  putStrLn $ str ++ suffix
  getYesNo defaultAns

newTransactions :: IO [Tx]
newTransactions = do
  cFrom <-
    do putStrLn "Enter amount and currency used to buy (example: 0.41140 ETH):"
       getMoney
  cTo <-
    do putStrLn "Enter amount and currency bought (example: 0.21 XMR):"
       getMoney
  desc <-
    do putStrLn
         "Enter an optional description of the transaction (leave empty if none):"
       getLine
  let tx = Tx cFrom cTo desc
  putStrLn $ "Added transaction: " ++ show tx
  answer <- putQuestion "Add another?" No
  if answer == Yes
    then (:) tx <$> newTransactions
    else return [tx]

getMoney :: IO Money
getMoney = do
  mnBS <- BS.split (BS.c2w ' ') <$> BS.getLine -- Get amount and currency, eg: 0.4132 XMR
  let mn =
        case mnBS of
          [vstr, mc] ->
            let val = readMaybe $ fromByteString' vstr :: Maybe Float
                cur = toCurrency $ fromByteString' mc
             in case (val, cur) of
                  (Just v, Just c) -> Just $ Money v c
                  _ -> Nothing
          _ -> Nothing
  case mn of
    Just m -> return m
    Nothing -> return $ Money 0 XMR
    -- Nothing -> do
      -- putStrLn "Invalid format, please try again. Example: 0.423 XBT"
      -- putStrLn $ "Recognized currencies: " ++ recognizedCurrencies ++ ". "
      -- getMoney
