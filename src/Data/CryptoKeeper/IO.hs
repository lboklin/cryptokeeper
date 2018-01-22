module Data.CryptoKeeper.IO where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.CryptoKeeper.Tx (Tx)
import Data.Ratio (numerator)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory
import System.FilePath

data DataFile = DataFile
  { appName :: String
  , fileName :: String
  , contents :: [Tx]
  }

debugLog :: Show a => String -> a -> IO a
debugLog str a = do
  putStrLn $ str ++ show a
  return a

timeInMicros :: IO Integer
timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime

timeInMillis :: IO Integer
timeInMillis = (`div` 1000) <$> timeInMicros

timeInSeconds :: IO Integer
timeInSeconds = (`div` 1000) <$> timeInMillis

timeInSeconds' :: IO Double
timeInSeconds' = (/ 1000000) . fromIntegral <$> timeInMicros

getDataFilePath :: String -> String -> IO FilePath
getDataFilePath appname dataFileName = do
  datadir <- getXdgDirectory XdgData appname
  createDirectoryIfMissing True datadir
  return $ datadir </> dataFileName

writeToDataFile :: DataFile -> IO ()
writeToDataFile (DataFile aname fname txs) =
  getDataFilePath aname fname >>= flip B.writeFile (encode txs)

appendDataFile :: DataFile -> IO ()
appendDataFile (DataFile aname fname txs) =
  getDataFilePath aname fname >>= flip B.appendFile (encode txs)

readDataFile :: FromJSON a => DataFile -> IO (Either String a)
readDataFile (DataFile appname dataFileName _) = do
  fp <- getDataFilePath appname dataFileName
  -- File reading has to be strict, otherwise it will remain
  -- open and locked as long as we haven't requested all of
  -- its contents.
  fContent <- BS.readFile fp
  -- Now we can go back to being lazy.
  let bs = B.fromStrict fContent
  return $ eitherDecode bs

dataFilePath :: DataFile -> FilePath
dataFilePath df = appName df ++ fileName df
