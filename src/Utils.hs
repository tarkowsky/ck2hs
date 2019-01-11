{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Data.DateTime
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hex
import Data.Functor.Identity

fromHex :: ByteString -> ByteString
fromHex = runIdentity . unhex

strDate :: String -> DateTime
strDate str = fromGregorian' (read y) (read m) (read d)
  where y:m:d:_ = split '.' str

yearDiff :: DateTime -> DateTime -> Int
yearDiff a b = fromIntegral ((diffSeconds a b) `div` (60 * 60 * 24 * 365))

cutNothing :: [Maybe a] -> [a]
cutNothing ((Just a):rest) = a:(cutNothing rest)
cutNothing (Nothing:rest) = cutNothing rest
cutNothing [] = []

replace :: Eq a => a -> a -> [a] -> [a]
replace a b (h:t) = if h == a
    then b:(replace a b t)
    else h:(replace a b t)
replace a b [] = []

split :: (Eq a) => a -> [a] -> [[a]]
split c l = helper [] l
  where helper built [] = [reverse built]
        helper built (h:t) = if h == c
            then (reverse built):(helper [] t)
            else helper (h:built) t

splitByHeader :: ByteString -> ByteString -> [ByteString]
splitByHeader header "" = [""]
splitByHeader header str = h:(BS.append t' first):rest
  where (h, t) = BS.breakSubstring header str
        (t', t'') = BS.splitAt (BS.length header) t
        first:rest = splitByHeader header t''
