module Sugar where

import Types

shallowSugar :: Entry -> Entry
shallowSugar (EEq "trait_patch" (EOne traitName)) = (EObj [
      EEq "trigger" (EObj [EEq "has_trait" (EOne traitName)]),
      EEq "apply_patch" (EOne traitName)
    ])
shallowSugar (EEq "has_dna" (EObj ents)) = (EEq "has_dna" (EObj (map dnaSugar ents)))
shallowSugar ent = ent

syntaxSugar :: Entry -> Entry
syntaxSugar ent = let ent2 = shallowSugar ent in case ent2 of
  (EOne str) -> ent2
  (EEq str ent') -> (EEq str (syntaxSugar ent'))
  (EObj ents) -> (EObj (map syntaxSugar ents))

dnaSugar :: Entry -> Entry
dnaSugar (e@(EEq pos (EOne n))) = case pos of
      "neck" -> helper 0
      "chin" -> helper 1
      "mouth" -> helper 2
      "nose" -> helper 3
      "cheeks" -> helper 4
      "eyes" -> helper 6
      "ears" -> helper 7
      "hair_color" -> helper 8
      "eye_color" -> helper 9
      _ -> e
    where helper x = (EEq (show x) (EOne n))
dnaSugar e = e
