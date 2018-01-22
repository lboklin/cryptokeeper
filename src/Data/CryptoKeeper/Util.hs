{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoKeeper.Util where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8 (unpack)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B.Char8 (unpack)
import Numeric (showFFloat)

surroundWith :: Char -> String -> String
surroundWith c str =
  case c of
    '(' -> "(" ++ str ++ ")"
    '[' -> "[" ++ str ++ "]"
    chr -> [chr] ++ str ++ [chr]

formatFloatN :: Float -> Int -> String
formatFloatN floatNum numOfDecimals =
  showFFloat (Just numOfDecimals) floatNum ""

imapM_ :: Monad m => (Int -> a -> m b) -> [a] -> m ()
imapM_ f xs =
  let it _ [] (z:zs) = it 1 [f 0 z] zs
      it n ys (z:zs) = it (n + 1) (f n z : ys) zs
      it _ _ _ = Nothing
   in return . const () $ it 0 [] xs

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe eab =
  case eab of
    Left _ -> Nothing
    Right b -> Just b

fromByteString :: B.ByteString -> String
fromByteString bs = filter (/= '\"') $ B.Char8.unpack bs

fromByteString' :: BS.ByteString -> String
fromByteString' bs = filter (/= '\"') $ BS.Char8.unpack bs
