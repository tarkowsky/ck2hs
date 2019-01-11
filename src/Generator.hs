{-# LANGUAGE OverloadedStrings #-}
module Generator where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
--import Data.Serialize
import Data.Word
import Data.Array.ST
import Control.Monad.ST
import Types
import qualified Data.Map as M
import Data.Char
import Evaluation
import EntryOperations
import Control.Monad
import System.Random (RandomGen)
import qualified System.Random as R
import Data.STRef
import Utils

imageSize = (252, 352)

outputFileCount = 20

pngMaleSeparator :: ByteString
pngMaleSeparator = fromHex "1A"

pngFemaleSeparator :: ByteString
pngFemaleSeparator = fromHex "1C"

twoByteUTF8 :: Int -> [Word8]
twoByteUTF8 n = [128 + 64 + (fromIntegral (n `div` 64)),
                  128 + (fromIntegral (n `mod` 64))]

encodeUTF8Char :: Int -> [Word8]
encodeUTF8Char n = if n < 128 then [fromIntegral n] else twoByteUTF8 n

encodeUTF8 :: String -> [Word8]
encodeUTF8 = concat . (map (encodeUTF8Char . ord))

makeSeed :: Character -> R.StdGen
makeSeed char = R.mkStdGen (fst (foldl helper (0, 1) (characterDNA char)))
  where helper (r, p) n = (r + n * p, p * 13)

processTriggers :: (RandomGen g) => LoaderState -> HandleData -> Character
                      -> STUArray y Int Word8 -> STRef y ByteString
                      -> g -> [Entry] -> ST y g
processTriggers ls hd char array foot g ent = processTrigger g ent
  where processTrigger g ((EEq "trigger" (EObj ents)):t) = do
          case foldl (&&) True (map (evaluate ls char) ents) of
            True -> processTrigger g t
            False -> return g
        processTrigger g ((EEq "apply_patch" (EOne patchName)):t) = do
          case M.lookup patchName (hdPatches hd) of
            Just patch -> applyPatch patch array
            Nothing -> error ("patch not found: " ++ (show patchName))
          processTrigger g t
        processTrigger g ((EEq "branch" (EObj ents)):t) = do
          g' <- foldM processTrigger g (map fromEntry ents)
          processTrigger g' t
        processTrigger g ((EEq "random" (EObj ents)):t) = do
          let (roll, g2) = R.randomR (0, (length ents) - 1) g
          let (g3, g4) = R.split g2
          processTrigger g3 [ents !! roll]
          processTrigger g4 t
        processTrigger g ((EEq "use_clothes" (EOne clothesPatch)):t) = do
          case M.lookup clothesPatch (hdFooters hd) of
            Just footer -> writeSTRef foot footer
            Nothing -> error ("no clothes patch " ++ clothesPatch)
          processTrigger g t
        processTrigger g (e:t) = error ("unknown entry " ++ (show e))
        processTrigger g [] = return g

generateST :: LoaderState -> HandleData -> Character ->
                ST y (ByteString, ByteString)
generateST ls hd char = do
  let baseline = hdBaseline hd
  let seed = makeSeed char
  array <- newListArray (0, (BS.length baseline) - 1) (BS.unpack baseline)
  footerRef <- newSTRef (hdBaseFooter hd)
  foldM (\g -> \ent -> case ent of
    (EObj ents) -> processTriggers ls hd char array footerRef g ents
    _ -> return g) seed (hdTriggers hd)
  applyPatch (namePatch (characterSex char) (characterName char)) array
  elems <- getElems array
  footer <- readSTRef footerRef
  return (BS.pack elems, footer)

generateCharacter :: LoaderState -> Int -> Character -> IO ()
generateCharacter ls n char = do
  let (separator, hd) = case characterSex char of
        Male -> (pngMaleSeparator, lsMaleData ls)
        Female -> (pngFemaleSeparator, lsFemaleData ls)
  let (generated, footer) = runST (generateST ls hd char)
  let fileName = (hdOutput hd) ++ "\\ck2_" ++ (show n) ++ ".png"
  BS.writeFile fileName (BS.concat
      [lsBaseImage ls, separator, generated, footer])

generateEmptyFiles :: HandleData -> Int -> IO ()
generateEmptyFiles hd n = mapM_ (\n -> do
  let fileName = (hdOutput hd) ++ "\\ck2_" ++ (show n) ++ ".png"
  writeFile fileName " "
  ) [n..outputFileCount]

generate :: LoaderState -> IO ()
generate ls = do
  let chars = (lsPlayerChar ls):(lsConsrots ls)
  (f, m) <- foldM (\(fc, mc) -> \char ->
    let (fr, mr, n) = case characterSex char of
          Male -> (fc, mc + 1, mc)
          Female -> (fc + 1, mc, fc)
    in if (fr > outputFileCount || mr > outputFileCount)
      then return (fc, mc)
      else do
        generateCharacter ls n char
        return (fr, mr)
    ) (1, 1) chars
  generateEmptyFiles (lsFemaleData ls) f
  generateEmptyFiles (lsMaleData ls) m

femaleNameLocations = [793, 2402]
maleNameLocations = [791, 1912]
nameLength = 16

namePatch :: Sex -> String -> Patch
namePatch sex name = foldl M.union M.empty (map (oneNamePatch name)
  (if sex == Male then maleNameLocations else femaleNameLocations))

oneNamePatch :: String -> Int -> Patch
oneNamePatch name pos = M.fromList (zip [pos..] name2)
  where name0 = encodeUTF8 name
        name1 = take 16 name0
        name2 = name1 ++ (replicate (16 - length name0) 0)

applyPatch :: Patch -> STUArray y Int Word8 -> ST y ()
applyPatch patch array =
  mapM_ (\(i, e) -> writeArray array i e) (M.assocs patch)
