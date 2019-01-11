{-# LANGUAGE OverloadedStrings #-}
module LoadPatches where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import System.Directory
import Data.Map (Map)
import qualified Data.Map as M
import Types
import Utils

--                 (filename, character bytes, footer bytes)
type HSCharacter = (String, ByteString, ByteString)

hsHeader :: ByteString
hsHeader = fromHex "E38090486F6E657953656C656374"

patchPrefix = "p."
patchSuffix = ".png"
patchBaseline = "baseline"

getFileNames :: String -> IO [(String, String)]
getFileNames dirPath = do
  files <- getDirectoryContents dirPath
  let sFiles = filter ((== patchPrefix) . (take (length patchPrefix))) files
  let pngFiles = filter ((== (reverse patchSuffix)) .
                  (take (length patchSuffix)) . reverse) sFiles
  return (map (\f -> (f, dirPath ++ ('\\':f))) pngFiles)

loadHSCharacter :: (String, String) -> IO HSCharacter
loadHSCharacter (shortName, fileName) = do
  bytes <- BS.readFile fileName
  let _:charBytes:t = splitByHeader hsHeader bytes
  let footer = BS.concat t
  return (getPatchName shortName, charBytes, footer)

getSampleCharacters :: String -> IO [HSCharacter]
getSampleCharacters dirPath = do
  files <- getFileNames dirPath
  mapM loadHSCharacter files

fst3 (a, _, _) = a

getBaseLine :: [HSCharacter] -> (ByteString, ByteString)
getBaseLine = (\(_, a, b) -> (a, b)) . head .
                (filter ((== patchBaseline) . fst3))

getPatchName :: String -> String
getPatchName name = reverse $ drop (length patchSuffix) $ reverse $
                      drop (length patchPrefix) $ name

createPatch :: ByteString -> ByteString -> Patch
createPatch baseline output = if (BS.length baseline) /= (BS.length output)
  then error "patch length mismatch"
  else M.fromList (cutNothing (map func [0..((BS.length baseline) - 1)]))
    where func n = if (BS.index baseline n) == (BS.index output n)
          then Nothing
          else (Just (n, (BS.index output n)))

loadPatches :: String -> HandleData -> IO HandleData
loadPatches dirPath hd = do
  chars <- getSampleCharacters dirPath
  let (baseline, baseFooter) = getBaseLine chars
  let c = filter ((/= patchBaseline) . fst3) chars
  let patches = map (\(name, bytes, _) -> (name, createPatch baseline bytes)) c
  let footers = M.fromList (map (\(name, _, footer) -> (name, footer)) c)
  return hd{
      hdBaseline = baseline,
      hdBaseFooter = baseFooter,
      hdPatches = M.union (M.fromList patches) (hdPatches hd),
      hdFooters = M.union footers (hdFooters hd)
    }
