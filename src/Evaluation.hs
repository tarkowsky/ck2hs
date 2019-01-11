module Evaluation where

import Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Utils

evaluate :: LoaderState -> Character -> Entry -> Bool
evaluate ls char (EEq "AND" (EObj ents)) =
  foldl (&&) True (map (evaluate ls char) ents)
evaluate ls char (EEq "OR" (EObj ents)) =
  foldl (||) False (map (evaluate ls char) ents)
evaluate ls char (EEq "NOT" (EObj ents)) =
  not $ foldl (&&) True (map (evaluate ls char) ents)
evaluate ls char (EEq "NOR" (EObj ents)) =
  not $ foldl (||) False (map (evaluate ls char) ents)
evaluate ls char (EEq "has_trait" (EOne traitName)) =
  S.member traitName (characterTraits char)
evaluate ls char (EEq "has_culture" (EOne cultureName)) =
  cultureName == characterCulture char
evaluate ls char (EEq "has_culture_group" (EOne groupName)) =
  groupName == M.findWithDefault "" (characterCulture char) (lsCultureGroups ls)
evaluate ls char (EEq "has_ethnicity" (EOne cultureName)) =
  cultureName == characterEthnicity char
evaluate ls char (EEq "has_ethnicity_group" (EOne groupName)) =
  groupName == M.findWithDefault "" (characterEthnicity char) (lsCultureGroups ls)
evaluate ls char (EEq "has_religion" (EOne religionName)) =
  religionName == characterReligion char
evaluate ls char (EEq "has_dna" (EObj ents)) =
  foldl (&&) True (map (evaluateProperty (characterDNA char)) ents)
evaluate ls char (EEq "has_portrait" (EObj ents)) =
  foldl (&&) True (map (evaluateProperty (characterProperties char)) ents)
evaluate ls char (EEq "age" (EOne ageStr)) =
  read ageStr <= characterAge char
evaluate ls char (EEq "practical_age" (EOne ageStr)) =
  read ageStr <= characterPracticalAge char
evaluate ls char (EEq "is_female" (EOne "yes")) =
  Female == characterSex char
evaluate ls char (EEq "prisoner" (EOne "yes")) =
  characterPrisoner char
evaluate ls char (EEq "player" ent) =
  evaluate ls (lsPlayerChar ls) ent
evaluate ls char entry = error ("unable to evaluate entry " ++ (show entry))

evaluateProperty :: [Int] -> Entry -> Bool
evaluateProperty list (EEq pos (EOne val)) =
  case split '/' (replace '\\' '/' val) of
    n:outOf:_ -> samePropValue (read n) (read pos) (read outOf) list
    _ -> error ("no limit given for property value " ++ (show val))
evaluateProperty _ entry =
  error ("unable to evaluate property " ++ (show entry))

samePropValue :: Int -> Int -> Int -> [Int] -> Bool
samePropValue val pos limit list =
  (val `mod` limit) == ((list !! pos) `mod` limit)
