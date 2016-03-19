{-# LANGUAGE OverloadedStrings #-}

module Day4.Day4 where
import Crypto.Hash (hash, digestToHexByteString, MD5,Digest)
import qualified Data.ByteString.Char8 as BS

hashSecret :: Int -> BS.ByteString -> BS.ByteString
hashSecret n s = digestToHexByteString (hash $ appendNumberToSecret n s :: Digest MD5 )

appendNumberToSecret :: Int -> BS.ByteString -> BS.ByteString
appendNumberToSecret n s = BS.concat [s, BS.pack $ show n]

allHashes :: BS.ByteString -> [(Int, BS.ByteString)]
allHashes s = map (\n -> (n, hashSecret n s)) [0 ..]

startsWithNZeros :: Int -> BS.ByteString -> Bool
startsWithNZeros n = BS.isPrefixOf $ BS.replicate n '0'

getNumber :: Int -> BS.ByteString -> Int
getNumber numZeros = fst . head . filter (startsWithNZeros numZeros . snd) . allHashes

main = print $ getNumber 5 "yzbqklnj"
