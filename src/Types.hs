module Types where

import Data.Set (Set)
import Data.Map (Map)
import Data.Array (Array)
import Control.Monad.State.Lazy (StateT)
import Data.Word
import Data.DateTime
import Data.ByteString (ByteString)

type Str = String

type LM = StateT LoaderState IO

data Token = TOpen | TClose | TEq | TString Str | TIgnore
  deriving (Show, Eq)

data Entry = EOne String | EEq String Entry | EObj [Entry]
  deriving (Show, Eq)

data Sex = Male | Female
  deriving (Show, Eq)

data Character = Character {
  characterId :: Str,
  characterName :: Str,
  characterSex :: Sex,
  characterTraits :: Set Str,
  characterDNA :: [Int],
  characterEthnicity :: Str,
  characterCulture :: Str,
  characterAlive :: Bool,
  characterReligion :: Str,
  characterDynasty :: Maybe Str,
  characterAge :: Int,
  characterProperties :: [Int],
  characterPrisoner :: Bool,
  characterPracticalAge :: Int
} deriving Show

data LoaderState = LoaderState {
    lsPlayerId :: Str,
    lsPlayerName :: Str,
    lsHomeDir :: Str,
    lsConsrots :: [Character],
    lsPlayerChar :: Character,
    lsTraitList :: Array Int Str,
    lsCultureGroups :: Map Str Str,
    lsDynasties :: Map Str Dynasty,
    lsSaveDir :: Str,
    lsBaseImage :: ByteString,
    lsDate :: DateTime,
    lsFemaleData :: HandleData,
    lsMaleData :: HandleData
  }

data HandleData = HandleData {
    hdBaseline :: ByteString,
    hdBaseFooter :: ByteString,
    hdTriggers :: [Entry],
    hdPatches :: Map Str Patch,
    hdOutput :: String,
    hdFooters :: Map Str ByteString
  }

data Dynasty = Dynasty {
    dntName :: Str,
    dntReligion :: Maybe Str,
    dntCulture :: Maybe Str
  } deriving Show

data Property = Property {
  prpName :: String,
  prpLocation :: Int,
  prpType :: PropertyType
} deriving Show

type Patch = Map Int Word8

data PropertyType = PrpSlider | PrpColor | PrpBool
  deriving Show
