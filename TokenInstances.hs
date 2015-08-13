{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-- module for recognizing instances of tokens, replacements, and
-- formula metavariable syntax in a latex source file and replacing
-- then with the appropriate text.

module TokenInstances

( doTokenReplacement )

where

import Types

import Language.LBNF
import qualified Data.Map as Map
import Data.Char (toUpper, isSpace)


---------------------------------------------
-- Recognizing Token Instances In The Text --
---------------------------------------------
-- We try to parse each word as a token instance
-- accoring to the following grammar.

bnfc [lbnf|

token LBrace '{' ;
token RBrace '}' ;
token Excl '!' ;
token ArticlePrefix 'a' ;
token UCasePrefix '^' ;
token PluralSuffix 's' ;
token TIdent letter ( letter | digit | '_' | ' ')* ;

TokenInstance. TokenInstance ::= Excl Excl MUpper MArticle BracesThing MPlural ;

BracesThing. BracesThing ::= LBrace TIdent RBrace ;

IsPlural. MPlural ::= PluralSuffix ;
NoPlural. MPlural ::= ;

ToUpper. MUpper ::= UCasePrefix ;
NoUpper. MUpper ::= ;

WithArticle. MArticle ::= ArticlePrefix ;
NoArticle.   MArticle ::= ; 

|]


----------------------------------------------------------
-- Compute Token Replacement String From Token Instance --
----------------------------------------------------------

-- if we find a token instance,  we attempt to find the token
-- in the OLConfig, and replace the token instance
-- with the appropriate text.

translate :: Map.Map String OLToken -> TokenInstance -> Maybe String
translate tdefs tinst@(TokenInstance _ _ tup tar tname tpl) =
  case (Map.lookup (unBrace tname) tdefs) of
    Nothing   -> Nothing
    Just tdef -> Just (articleStr ++ mainStr)
      where
        maybeUpper = if (transUpper tup)
                       then (\s -> (toUpper (head s)) : (tail s))
                       else id
        articleStr = if (transArticle tar)
                       then ((articleOf tdef) ++ " ")
                       else ""
        mainStr = if (transPlural tpl)
                    then (pluralOf tdef)
                    else (singularOf tdef)

transUpper :: MUpper -> Bool
transUpper NoUpper = False
transUpper (ToUpper _) = True           

transArticle :: MArticle -> Bool
transArticle NoArticle = False
transArticle (WithArticle _) = True             

transPlural :: MPlural -> Bool
transPlural NoPlural = False
transPlural (IsPlural _) = True            

unBrace :: BracesThing -> String
unBrace (BracesThing _ (TIdent str) _) = str

-----------------------
-- Replacement Logic --
-----------------------


tokenWords :: String -> [String]
tokenWords [] = []
tokenWords [x] = [[x]]
tokenWords (x:y:xs)
    | x == '!' && y == '!'  = ("!!" ++ match) : (tokenWords rest)
    | otherwise = [x] : (tokenWords (y:xs)) -- very slow
  where
    match = eatTokenString xs
    rest  = drop (length match) xs
    
                        
eatTokenString :: String -> String
eatTokenString xs = match ++ (if (head rest) == 's' then "s" else "")
  where
    match = takeWhile' (/= '}') xs
    rest = drop (length match) xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' pred xs = match ++ [(head (drop (length match) xs))]
  where
    match = takeWhile pred xs

doTokenReplacement :: OLConfig -> String -> String
doTokenReplacement cfg ltx = concatMap (tokenReplace cfg) (tokenWords ltx)

tokenReplace :: OLConfig -> String -> String
tokenReplace cfg word = case (pTokenInstance . myLexer) word of
    Bad _   -> word -- could not parse token instance, so keep same string.
    Ok tok -> case translate (tokenMap cfg) tok of
                Nothing  -> word -- token not in config, so keep same string.
                Just new -> new
