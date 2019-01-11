{-# LANGUAGE IncoherentInstances #-}
--NOTES
-- culture and religion are inherited from dynasty
-- religion from coat of arms takes priority over "official"
-- ethnicity is inherited from culture
module LoadCK2 where

import Parser
import Types
import Data.Array
import System.Directory
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.ByteString as BS
import Character
import Control.Monad (foldM)
import EntryOperations
import LoadPatches
import Utils
import System.IO
import Data.Char
import Sugar

loadSave :: String -> IO (Character, [Character])
loadSave fileName = error "dupa"

lastMods :: [Entry] -> [String]
lastMods entries = case singleEntry "last_mods" entries of
  Just x -> x
  _ -> []

dirTXTFiles :: String -> IO [String]
dirTXTFiles dir = do
  contents <- getDirectoryContents dir
  return $ filter ((== "txt.") . (take 4) . reverse) contents

directoryReader :: String -> String -> IO [(String, Str)]
directoryReader dirPath innerPath = do
  let path = dirPath ++ '\\':innerPath
  dirExists <- doesDirectoryExist path
  if dirExists
    then do
      files <- dirTXTFiles path
      let paths = map (\fn -> path ++ '/':fn) files
      contents <- mapM readFile paths
      return (zip files contents)
    else return []

modReader :: String -> String -> IO (String -> IO [(String, Str)])
modReader docDir modFilePath = do
  let path  = (docDir ++ '/':modFilePath)
  exists <- doesFileExist path
  if exists
    then do
      modSettings <- parse <$> readFile path
      return $ case singleEntry "path" modSettings of
        Nothing -> error ("no path provided for mod " ++ modFilePath)
        Just modDirPath -> directoryReader (docDir ++ '/':modDirPath)
    else return (const (return []))

modReaders :: String -> IO [String -> IO [(String, Str)]]
modReaders docPath = do
  settings <- parse <$> readFile (docPath ++ "/settings.txt")
  let modList = lastMods settings
  mapM (modReader docPath) modList

processDynasty :: [Entry] -> Dynasty
processDynasty ents = Dynasty{
      dntName = dntMap M.! "name",
      dntCulture = M.lookup "culture" dntMap,
      dntReligion = religion
  }
  where dntMap = (fromEntries ents) :: Map Str Str
        religion = case singleEntry "coat_of_arms" ents of
          Just ent -> case singleEntry "religion" ent of
            Just (EEq _ (EOne rel)) -> Just rel
            _ -> M.lookup "religion" dntMap
          Nothing -> M.lookup "religion" dntMap

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr (Just a) _ = Just a
maybeOr _ (Just a) = Just a
maybeOr _ _ = Nothing

initialLoaderState :: String -> IO LoaderState
initialLoaderState ck2Path = do
  homePath <- getHomeDirectory
  let docPath = homePath ++ "/Documents/Paradox Interactive/Crusader Kings II"
  allReaders <- ((directoryReader ck2Path):) <$> modReaders docPath
  let load = loadDirs allReaders
  settings <- load "honey_select/settings"
  let settingsDict = fromEntries settings
  traits <- fromEntries <$> load "common/traits"
  dynasties <- ((M.map processDynasty) . fromEntries)
                  <$> load "common/dynasties"
  cultureEntries <- load "common/cultures"
  let culturesList = map processCultureGroup cultureEntries
  femaleTriggers <- map syntaxSugar <$> load "honey_select/female"
  maleTriggers <- map syntaxSugar <$> load "honey_select/male"
  baseImage <- BS.readFile
                (resolvePathStr homePath $ settingsDict M.! "base_image_path")
  let ls0 = LoaderState{
      lsTraitList = traits,
      lsDynasties = dynasties,
      lsConsrots = [],
      lsCultureGroups = M.fromList (concat culturesList),
      lsHomeDir = homePath,
      lsSaveDir = resolvePath ls0 $ settingsDict M.! "savegame_directory",
      lsBaseImage = baseImage,
      lsFemaleData = emptyHandleData{ hdTriggers = femaleTriggers },
      lsMaleData = emptyHandleData{ hdTriggers = maleTriggers },
      lsPlayerId = undefined,
      lsPlayerName = undefined,
      lsPlayerChar = undefined,
      lsDate = undefined
    }
  ls <- foldM parseSettings ls0 settings
  return ls

resolvePathStr :: String -> String -> String
resolvePathStr h ('~':str) = h ++ str
resolvePathStr h str = str

resolvePath :: LoaderState -> String -> String
resolvePath ls = resolvePathStr (lsHomeDir ls)

emptyHandleData :: HandleData
emptyHandleData = HandleData{
  hdBaseline = undefined,
  hdTriggers = [],
  hdPatches = M.empty,
  hdFooters = undefined,
  hdBaseFooter = undefined,
  hdOutput = undefined
}

loadDirs :: [String -> IO [(String, Str)]] -> String -> IO [Entry]
loadDirs readers dirName = do
  pairs <- mapM (\r -> r dirName) readers
  let fileContents = map snd (sort (concat pairs))
  return $ concat $ map parse $ fileContents

parseMain :: LoaderState -> Entry -> LoaderState
parseMain ls (EEq "player" (EObj obj)) = foldl parsePlayerEntry ls obj
parseMain ls (EEq "player_name" (EOne name)) = ls{lsPlayerName = name}
parseMain ls (EEq "character" (EObj obj)) = foldl parseCharacter ls obj
parseMain ls (EEq "dynasties" (EObj obj)) = foldl parseDynasty ls obj
parseMain ls (EEq "date" (EOne date)) = ls{lsDate = strDate date}
parseMain ls _ = ls

parsePlayerEntry :: LoaderState -> Entry -> LoaderState
parsePlayerEntry ls (EEq "id" (EOne pid)) = ls{lsPlayerId = pid}
parsePlayerEntry ls _ = ls

parseHandleData :: LoaderState -> HandleData -> Entry -> IO HandleData
parseHandleData ls hd (EEq "patch_directory" (EOne path)) =
  loadPatches (resolvePath ls path) hd
parseHandleData ls hd (EEq "output_directory" (EOne path)) =
  return hd{hdOutput = resolvePath ls path}
parseHandleData ls hd _ = return hd

parseSettings :: LoaderState -> Entry -> IO LoaderState
parseSettings ls (EEq "female_data" (EObj ents)) = do
  hd <- foldM (parseHandleData ls) (lsFemaleData ls) ents
  return ls{lsFemaleData = hd}
parseSettings ls (EEq "male_data" (EObj ents)) = do
  hd <- foldM (parseHandleData ls) (lsMaleData ls) ents
  return ls{lsMaleData = hd}
parseSettings ls _ = return ls

processCultureGroup :: Entry -> [(String, String)]
processCultureGroup (EEq groupName (EObj entries)) =
  map (\c -> (c, groupName))
  (filter (/= "graphical_cultures") (fromEntries entries))
processCultureGroup _ = []

alterDynasty :: [Entry] -> Maybe Dynasty -> Maybe Dynasty
alterDynasty ents Nothing = Just (processDynasty ents)
alterDynasty ents (Just oldDyn) = Just Dynasty{
    dntName = name,
    dntCulture = orProp dntCulture,
    dntReligion = orProp dntReligion
  }
  where newDyn = processDynasty ents
        orProp prop = maybeOr (prop newDyn) (prop oldDyn)
        name = case singleEntry "name" ents of
          Just n -> n
          Nothing -> dntName oldDyn

parseDynasty :: LoaderState -> Entry -> LoaderState
parseDynasty ls (EEq key (EObj ents)) = ls{lsDynasties =
      M.alter (alterDynasty ents) key (lsDynasties ls)}
parseDynasty ls _ = ls

latestCK2File :: String -> IO String
latestCK2File dirPath = do
  files <- getDirectoryContents dirPath
  let files' = filter (( == (reverse ".ck2")) . (take 4) . reverse) files
  pairs <- mapM (\file -> do
    let fileName' = dirPath ++ '\\':file
    time <- getModificationTime fileName'
    return (time, fileName')
    ) files'
  return $ snd $ maximum pairs

loadSaveGame :: LoaderState -> IO LoaderState
loadSaveGame ls0 = do
  fileName <- latestCK2File (lsSaveDir ls0)
  putStrLn ("latest savegame is " ++ (show fileName))
  fileContents <- (do
      h <- openFile fileName ReadMode
      System.IO.hSetEncoding h latin1
      hGetContents h
    )
  let contents = if ("CK2txt" == take 6 fileContents)
      then '{':(drop 6 fileContents)
      else error "not CK2txt file"
  let (EObj mainList):_ = parse contents
  return (foldl parseMain ls0 mainList)
