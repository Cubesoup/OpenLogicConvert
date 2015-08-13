module Types where

import qualified Data.Map as Map

---------------------------
---------------------------
-- Abstract Config Types --
---------------------------
---------------------------

data OLConfig = OLConfig { tokenMap :: Map.Map String OLToken
                         , replaceMap :: Map.Map String OLReplace -- keys are String?
                         , formulaVars :: OLFormulaChoice }
    deriving (Show, Eq)

emptyConfig :: OLConfig
emptyConfig = OLConfig { tokenMap = Map.empty
                       , replaceMap = Map.empty
                       , formulaVars = OLFormulas "no formula choice" }

data OLFormulaChoice = OLFormulas String
  deriving (Show, Eq)

data OLToken = OLToken { nameOf     :: String
                       , singularOf :: String
                       , pluralOf   :: String
                       , articleOf  :: String }
  deriving (Show, Eq)

data OLArgument = Optional String
                | Mandatory String
                | NegationPlace
  deriving (Show, Eq)

instance Ord OLArgument where
  compare (Mandatory str) (Mandatory str') = compare str str'
  compare (Mandatory _) _ = GT
  compare (Optional str) (Optional str') = compare str str'
  compare (Optional _) NegationPlace = GT
  compare NegationPlace NegationPlace = EQ
  compare NegationPlace _ = LT


optional :: OLArgument -> Bool
optional (Mandatory _) = False
optional _             = True

mandatory :: OLArgument -> Bool
mandatory = not . optional

data OLReplace = OLReplace { command :: String
                           , arguments :: [OLArgument]
                           , cases :: Map.Map [OLArgument] String }
  deriving (Show, Eq)
-- the "cases" field is a map from the supplied optional arguments to
-- the replacement string for that case. the negation '/' is treated as
-- an optinal argument. The mandatory arguments are not part of this list,
-- and are assumed to be always present.
