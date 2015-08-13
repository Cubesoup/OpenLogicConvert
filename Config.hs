{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-- TODO: Tags, Sanity check on replacement rules for just after parsing.

module Config

( parseConfig
, emptyConfig
, OLConfig
, OLFormulaChoice
, OLToken
, OLArgument
, OLReplace )

where

import Types

import Language.LBNF
import qualified  Data.Map as Map

import System.Environment
import Data.String.Utils (split)
import Data.Char (isAlphaNum)

----------------------------
----------------------------
-- Parser For Config File --
----------------------------
----------------------------

bnfc [lbnf|

token LBracket '[' ;
token RBracket ']' ;
token LBrace '{' ;
token RBrace '}' ;
token LParen '(' ;
token RParen ')' ;
token NegateTok '/' ;
token Backslash '\\' ;

comment "--" ;

----------------------------------
-- Replacement Rule Definitions -- 
---------------------------------- 

OptionalArg.  Argument ::= LBracket Ident RBracket ;
MandatoryArg. Argument ::= LBrace Ident RBrace ;
NegationArg.  Argument ::= "/" ;

NoArgs.   ArgList ::= ;
SomeArgs. ArgList ::= Argument ArgList ;

LatexCommand. LatexCommand ::= Backslash Ident ;

ReplaceRule. ReplaceRule ::= LatexCommand CommandSpec ;

AllMandatory. CommandSpec ::= ArgList "->" String ;
SomeOptional. CommandSpec ::= ArgList "|" CaseList ;

NoCases.   CaseList ::= "." "->" String ;
SomeCases. CaseList ::= [IdentOrNeg] "->" String CaseList ;

separator IdentOrNeg "," ;

IdentThing. IdentOrNeg ::= Ident ;
NegThing.   IdentOrNeg ::= "/" ;

NoReplace.   ReplaceRuleList ::= ;
SomeReplace. ReplaceRuleList ::= ReplaceRule ";" ReplaceRuleList ;

-----------------------
-- Token Definitions --
-----------------------

TokenDef. TokenDef ::= "!!" String "->" MArticle String "/" String ;

NoArticle.   MArticle ::= ;
JustArticle. MArticle ::= LParen String RParen ;

NoTokenDef.   TokenDefList ::= ;
SomeTokenDef. TokenDefList ::= TokenDef ";" TokenDefList ;

----------
-- Tags --
----------

-- TODO. it would be nice if the value of tags didn't change throughout the
-- book. This way we can understand what's going on by reading the config
-- file, as opposed to the source for the book.

------------------------
-- Entire Config File --
------------------------
ConfigFile. ConfigFile ::= "Formula Metavariables:" Ident
                           "Replacements:" ReplaceRuleList
                           "Tokens:" TokenDefList ;

|]

-------------------------------------------------------------
-------------------------------------------------------------
-- Translation From Abstract Syntax Tree to Abstract Types --
-------------------------------------------------------------
-------------------------------------------------------------

interpretFile :: ConfigFile -> OLConfig
interpretFile (ConfigFile fcon reps toks) =
    OLConfig { tokenMap = interpretTokenDefs toks
             , replaceMap = interpretReplacements reps
             , formulaVars = interpretFormulaChoice fcon }

interpretTokenDefs :: TokenDefList -> Map.Map String OLToken
interpretTokenDefs tokens = Map.fromList $ map (\x -> (nameOf x, x)) olTokens
  where
    olTokens :: [OLToken]
    olTokens = map interpretTokenDef (toTokenList tokens)
    toTokenList :: TokenDefList -> [TokenDef]
    toTokenList NoTokenDef = []
    toTokenList (SomeTokenDef tk rest) = tk : (toTokenList rest)

interpretTokenDef :: TokenDef -> OLToken
interpretTokenDef (TokenDef name marticle singular plural) =
    OLToken { nameOf = name
            , singularOf = singular
            , pluralOf = plural
            , articleOf = interpretMArticle marticle }

interpretMArticle :: MArticle -> String
interpretMArticle NoArticle = "a"
interpretMArticle (JustArticle _ a _) = a


interpretReplacement :: ReplaceRule -> OLReplace
interpretReplacement (ReplaceRule cmd spec) =
    interpretCommandSpec (interpretLatexCommand cmd) spec

-- the OLReplace this returns probably won't have the 'command' field
-- filled in. For use in 'interpretReplace' only.
interpretCommandSpec :: String -> CommandSpec -> OLReplace
interpretCommandSpec name (AllMandatory args replacetext) =
    OLReplace { arguments = interpretArgList args
              , cases = Map.singleton [] replacetext
              , command = name }
interpretCommandSpec name (SomeOptional args caselist) =
    OLReplace { arguments = interpretArgList args

              , cases = interpretCaseList caselist
              , command = name }

interpretReplacements :: ReplaceRuleList -> Map.Map String OLReplace
interpretReplacements rs = Map.fromList $ map (\x -> (command x,x)) (toRules rs)
  where
    toRules :: ReplaceRuleList -> [OLReplace]
    toRules NoReplace = []
    toRules (SomeReplace x xs) = (interpretReplacement x) : (toRules xs)

interpretLatexCommand :: LatexCommand -> String
interpretLatexCommand (LatexCommand _ (Ident str)) = str

interpretArgList :: ArgList -> [OLArgument]
interpretArgList NoArgs = []
interpretArgList (SomeArgs x xs) = (interpretArgument x) : (interpretArgList xs)

interpretArgument :: Argument -> OLArgument
interpretArgument NegationArg = NegationPlace
interpretArgument (MandatoryArg _ (Ident str) _) = Mandatory str
interpretArgument (OptionalArg _ (Ident str) _) = Optional str

interpretCaseList :: CaseList -> Map.Map [OLArgument] String
interpretCaseList (NoCases str) = Map.singleton [] str
interpretCaseList (SomeCases when str rest) =
    Map.insert (identsToArgs when) str (interpretCaseList rest)
  where
    identsToArgs :: [IdentOrNeg] -> [OLArgument]
    identsToArgs = map identToArg
    identToArg :: IdentOrNeg -> OLArgument
    identToArg (NegThing) = NegationPlace
    identToArg (IdentThing (Ident str)) = Optional str
    -- this is ok because the 'whendefined' list should never contain
    -- names of mandatory arguments, which are always defined.


interpretFormulaChoice :: Ident -> OLFormulaChoice
interpretFormulaChoice (Ident s) = OLFormulas s

-------------------------------------------
-- Sanity Checking and Parser for Export --
-------------------------------------------

parseConfig :: String -> Either String OLConfig 
parseConfig x = case (pConfigFile . myLexer) x of
                  Ok config -> Right (interpretFile config)
                  Bad err   -> Left err

type Error = String

sanityCheck :: OLConfig -> Maybe Error -- Nothing means no problems, config is sane.
sanityCheck cfg = undefined

-- token config is always sensible afaik

replacementRuleSensible :: OLReplace -> Bool
replacementRuleSensible rule = undefined
  where
    optArgs = filter optional (arguments rule)
    -- the caseArgs must be possible arguments (ruleArgs)
    argsSensible :: [OLArgument] -> Bool
    argsSensible caseArgs = all (\x -> x `elem` optArgs) caseArgs

-- the #thing instances in the replacement string must be
-- in the caseArgs for that case.
identifiersAreDefined :: [OLArgument] -> String -> Bool
identifiersAreDefined args str = all (\x -> x `elem` (map argName args)) identifierList
  where
    identifierList :: [String]
    identifierList = map (takeWhile identThing) (split "#" str)
    identThing :: Char -> Bool
    identThing x = isAlphaNum x || x == '_'

argName :: OLArgument -> String
argName NegationPlace = "!!!matchNothing!!!" -- lol what a hack
argName (Mandatory x) = x
argName (Optional x) = x

-- no argument list can contain duplicate arguments
argsUnique :: [OLArgument] -> Bool
argsUnique [] = True
argsUnique (x:xs) = (argsUnique xs) && (not (x `elem` xs))
    
-- the negation "/" not occur after the first argument.
negationSensible :: [OLArgument] -> Bool
negationSensible [] = True
negationSensible xs = not (NegationPlace `elem` (tail xs))

formulaConfigSensible :: OLFormulaChoice -> Bool
formulaConfigSensible (OLFormulas str) = str `elem` okFormulaTypes

okFormulaTypes :: [String] 
okFormulaTypes = ["greek","latin"]

                          

