module Character where

import Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Array (Array, (!))
import EntryOperations
import Data.DateTime
import Utils
import Data.Char

parseCharacter :: LoaderState -> Entry -> LoaderState
parseCharacter ls (EEq charId (EObj entries)) = if charId == lsPlayerId ls
  then ls{lsPlayerChar = character}
  else if playerConsort && alive
    then ls{lsConsrots = character:(lsConsrots ls)}
    else ls
  where character = Character{
            characterId = charId,
            characterName = dict !!! "bn",
            characterAlive = alive,
            characterSex = if M.member "fem" dict then Female else Male,
            characterTraits = traits,
            characterDNA = prpString (dict !!! "dna"),
            characterEthnicity = ethnicity,
            characterCulture = culture,
            characterReligion = religion,
            characterDynasty = dynasty,
            characterAge = age,
            characterProperties = prpString (dict !!! "prp"),
            characterPrisoner = (M.member "imprisoned" dict),
            characterPracticalAge = practicalAge
          }
        dict = (fromEntries entries) :: Map Str Entry
        dynasties = lsDynasties ls
        traits = nameTraits (lsTraitList ls) (dict !!# "tr")
        culture = dntLookup "cul" dntCulture
        religion = dntLookup "rel" dntReligion
        alive = not (M.member "d_d" dict)
        ethnicity = case M.lookup "eth" dict of
          Just (EOne val) -> val
          _ -> culture
        playerConsort =
          (elem (EEq "spouse" (EOne (lsPlayerId ls))) entries)
          || (elem (EEq "consort_of" (EOne (lsPlayerId ls))) entries)
          || (elem (EEq "lover" (EOne (lsPlayerId ls))) entries)
        dntLookup :: Str -> (Dynasty -> Maybe Str) -> Str
        dntLookup key getter = case M.lookup key dict of
          Just (EOne val) -> val
          _ -> case getter (dynasties M.! (dict !!! "dnt")) of
            Just x -> x
            Nothing -> ""
        dynasty = case M.lookup "dnt" dict of
          Just (EOne dntId) -> Just (dntName (dynasties  M.! dntId))
          _ -> Nothing
        age = yearDiff (lsDate ls) (strDate (fromEntry (dict M.! "b_d")))
        practicalAge = case M.lookup "immortal" dict of
          Just (EOne ageStr) -> read ageStr
          Nothing -> age
parseCharacter ls _ = ls

prpString :: Str -> [Int]
prpString = map (\c -> 1 + (ord c) - (ord 'a'))

nameTraits :: (Array Int Str) -> [Entry] -> Set Str
nameTraits arr [] = S.empty
nameTraits arr ((EOne str):rest) =
                            S.insert (arr ! (read str)) (nameTraits arr rest)
nameTraits arr (_:rest) = nameTraits arr rest
