{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- TODO: rewrite "doCommandReplacement" to use the new OLReplace type (more complex).
-- TODO: write the thing that replaces !A in math mode with the greek / latin letters.

module Main where

import Replacements
import TokenInstances
import Linearizer
import Config

import System.FilePath
import System.IO
import Data.Convertible
import Control.Monad
import qualified Data.Map as Map
import Language.LBNF.Runtime

-- using getopt-generics to handle command line arguments.
-- (https://github.com/zalora/getopt-generics#getopt-generics)
import Data.Typeable
import GHC.Generics
import System.Console.GetOpt.Generics
import System.Environment

-- using hatex to do most of the LaTeX things.
-- (https://hackage.haskell.org/package/HaTeX)
import Text.LaTeX.Base hiding (input)
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Render       

---------------------------------
-- Command Line Option Parsing --
---------------------------------

-- getopt-generics uses this data structure to generate the 'getArguments' function
-- , which we use to parse the command line arguments (results in an Options).
data Options
  = Options {
    input  :: Maybe FilePath,
    output :: Maybe FilePath,
    config :: Maybe FilePath
    }
  deriving (Show, GHC.Generics.Generic)  

instance System.Console.GetOpt.Generics.Generic Options
instance HasDatatypeInfo Options

helpModifiers :: [Modifier]
helpModifiers = [
  AddOptionHelp "input" "The file you want to convert. Must be a .tex file",
  AddOptionHelp "output" "Where you want to store the result of the conversion. If the file exists it will be overwritten (be careful!). Must end in .tex",
  AddOptionHelp "config" "The location of the configuration file. Probably ends in .opt but doesn't need to."
  ]

-- default argument values (assumes run in book directory).
defaultInputPath :: FilePath
defaultInputPath = "open-logic-complete.tex"

defaultOutputPath :: FilePath
defaultOutputPath = "processed-book.tex"

defaultConfigPath :: FilePath
defaultConfigPath = "book-configuration.opt" 
-- 'opt' is a common and generic extension. Since the intention is for it
-- to resisde in the book directory, we don't need to be too specific.


------------------------
-- Main Program Logic --
------------------------

main :: IO ()
main = do
  -- command line option handling with getopt-generics
  options <- modifiedGetArguments helpModifiers 

  let inputPath = if (input options) == Nothing then defaultInputPath
                      else (\(Just x) -> x) (input options)
      outputPath = if (output options) == Nothing then defaultOutputPath
                       else (\(Just x) -> x) (output options)
      configPath = if (config options) == Nothing then defaultConfigPath
                       else (\(Just x) -> x) (config options)

  when ((takeExtension inputPath) /= ".tex")
       (putStrLn "the input file must be a LaTeX source file")

  when ((takeExtension outputPath) /= ".tex")
       (putStrLn "the output file name must end in .tex")

  when ((takeExtension configPath) /= ".opt")
       (putStrLn "the configuration file name must end in .opt")

  -- parse config file
  rawConfig <- readFile configPath
  case parseConfig rawConfig of
    Left err -> error $ "could not parse config file: " ++ err
    Right config -> do
      putStrLn "config file successfully parsed..."      

      -- parse book file ('input')
      msource <- parseLaTeXFile inputPath
      case msource of
        (Left perr)    -> putStrLn (show perr)
        (Right source) -> do
          putStrLn "book input file successfully parsed as LaTeX..."
         
          -- deal with \olinclude commands, obtain book as data structure
          monolith <- linearizeStructure source
          putStrLn "linearized..."

           -- write the resulting text to output file (the entire book)
          writeFile outputPath (doTokenReplacement config (convert (render monolith)))


                 
--------------------------------
-- Other Things (unorganized) --
--------------------------------
{-    
                
-- the "replace this with that" thing. Probably a fold.
-- Involves functions like "translate" in the Tokens module.
doCommandReplacement :: OLConfig -> LaTeX -> LaTeX
doCommandReplacement cfg ltx = texmap replaceable replace ltx
  where
    replaceable :: LaTeX -> Bool
    replaceable ltx = if not (isCommand ltx) then False
                        else not $ (Map.lookup (commandString ltx) (replacements cfg))
                                == Nothing
    -- only defined if replaceable is True
    replace :: LaTeX -> LaTeX
    replace ltx = replaceCommandString ltx $ (\(Just x) -> x) $ 
                  Map.lookup (commandString ltx) (replacements cfg)

isCommand :: LaTeX -> Bool
isCommand (TeXCommS _) = True
isCommand (TeXComm _ _) = True
isCommand _ = False

-- only defined if isCommand is True
commandString :: LaTeX -> String       
commandString (TeXCommS s) = s
commandString (TeXComm s _) = s
commandString _ = error "called 'commandString' on something not a command"

-- only defined if isCommand is True
replaceCommandString :: LaTeX -> String -> LaTeX
replaceCommandString (TeXCommS _) s = (TeXCommS s)
replaceCommandString (TeXComm _ args) s = (TeXComm s args)
replaceCommandString _ _ = error "called 'replacecommandstring' on something not a command"
-}
