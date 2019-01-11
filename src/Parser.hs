module Parser where

import Types

eatToken :: String -> (Token, String)
eatToken (' ' :str) = eatToken str
eatToken ('\n':str) = eatToken str
eatToken ('\t':str) = eatToken str
eatToken ('#' :str) = eatToken (dropWhile (/= '\n') str)
eatToken ('{' :str) = (TOpen, str)
eatToken ('}' :str) = (TClose, str)
eatToken ('=' :str) = (TEq, str)
eatToken ('\"':str) = eatString str
eatToken (c:str) = eatIdent (c:str)
eatToken [] = (TIgnore, "")

specialChar :: Char -> String
specialChar '"' = "\""
specialChar c = '\\':[c]

eatString :: String -> (Token, String)
eatString ('\"':str) = ((TString []), str)
eatString ('\\':c:str) = ((TString ((specialChar c) ++ str')), rest)
  where ((TString str'), rest) = eatString str
eatString (c:str) = ((TString (c:str')), rest)
  where ((TString str'), rest) = eatString str

eatIdent :: String -> (Token, String)
eatIdent (' ' :str) = ((TString []), str)
eatIdent ('\n':str) = ((TString []), str)
eatIdent ('\t':str) = ((TString []), str)
eatIdent ('{' :str) = ((TString []), '{':str)
eatIdent ('}':str) = ((TString []), '}':str)
eatIdent ('=':str) = ((TString []), '=':str)
eatIdent [] = ((TString []), [])
eatIdent (c:str) = ((TString (c:ident)), rest)
  where ((TString ident), rest) = eatIdent str

eatAll :: ([a] -> (b, [a])) -> [a] -> [b]
eatAll eat [] = []
eatAll eat l = a:(eatAll eat l')
  where (a, l') = eat l

tokenize = eatAll eatToken

eatEntry :: [Token] -> (Entry, [Token])
eatEntry ((TString a):TEq:rest) = ((EEq a b), rest')
  where (b, rest') = eatEntry rest
eatEntry (TOpen:rest) = eatObj [] rest
eatEntry ((TString a):rest) = ((EOne a), rest)
eatEntry (t:_) = error ("error eating entry " ++ (show t))
eatEntry [] = error "error eating entry []"
--eatEntry (x:t) = error $ "got " ++ (show x) ++ " when loading entry "
-- ++ (show $ take 3 t)

eatObj :: [Entry] -> [Token] -> (Entry, [Token])
eatObj ret (TClose:rest) = (EObj (reverse ret), rest)
eatObj ret tokens = eatObj (entry:ret) rest
  where (entry, rest) = eatEntry tokens

entrise = eatAll eatEntry

parse :: String -> [Entry]
parse = entrise . (filter (/= TIgnore)) . tokenize
