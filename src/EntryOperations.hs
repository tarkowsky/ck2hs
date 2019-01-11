{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
module EntryOperations where

import Types
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Array (Array)
import qualified Data.Array as A

class FromEntry a where
  fromEntries :: [Entry] -> a
  fromEntry :: Entry -> a
  fromEntry (EObj entries) = fromEntries entries
  fromEntry (EEq _ (EObj entries)) = fromEntries entries
  fromEntry _ = error "default fromEntry failed"

instance FromEntry Entry where
  fromEntry = id
  fromEntries = head

instance (FromEntry Str) where
  fromEntry (EOne str) = str
  fromEntry (EEq str _) = str
  fromEntry e = error "fromEntry converstion to Str failed\n" ++ (show e)
  fromEntries _ = error "fromEntries converion to Str failed"

instance FromEntry a => FromEntry [a] where
  fromEntries entries = map fromEntry entries

instance FromEntry a => FromEntry (Map Str a) where
  fromEntries entries = M.fromList (map prepare entries)
    where prepare (EOne str) = (str, fromEntry (EOne str))
          prepare (EEq str ent) = (str, fromEntry ent)
          prepare (EObj entries) = ("", fromEntries entries)

instance FromEntry a => FromEntry (Array Int a) where
  fromEntries entries = (A.listArray (1, length entries) (map fromEntry entries))

(!!!) :: Map Str Entry -> Str -> Str
(!!!) m k = case M.lookup k m of
  Just (EOne str) -> str
  _ -> error ("dictionary lookup failed (one) for " ++ (show k) ++ "in\n" ++ (show m))

(!!#) :: Map Str Entry -> Str -> [Entry]
(!!#) m k = case M.lookup k m of
  Just (EObj entries) -> entries
  _ -> error ("dictionary lookup failed (obj) for " ++ (show k))

singleEntry :: (FromEntry a) => Str -> [Entry] -> Maybe a
singleEntry str ((EEq str' e):t) = if str == str'
  then Just (fromEntry e)
  else singleEntry str t
singleEntry str (_:t) = singleEntry str t
singleEntry str [] = Nothing
