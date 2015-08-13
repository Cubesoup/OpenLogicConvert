module Linearizer where

-- the linearizeStructure function will deal with any \olimport commands,
-- replacing them with the LaTeX of the file they were to import.
--
-- Note: Only deals with the optional subdirectory argument and mandatory
-- argument. so \olimport[computability]{lambda-calculus} and
-- \olimport{lambda-calculus} are OK but the other two arguments are not dealt with.
-- This should be alright as the book does not currently use the other two
-- arguments

import Text.LaTeX.Base
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Parser
import System.FilePath.Posix
import System.IO
import System.Directory
import Data.Convertible

linearizeStructure :: LaTeX -> IO LaTeX
linearizeStructure = texmapM (sameCommand "olimport") getLaTeX

sameCommand :: String -> (LaTeX -> Bool)
sameCommand str (TeXCommS name) = str == name
sameCommand str (TeXComm name _) = str == name
sameCommand str _ = False

getLaTeX :: LaTeX -> IO LaTeX
getLaTeX (TeXComm str args) = if str /= "olimport"
  then error "called getLaTeX on non olimport command"
  else do

    case length args of
      1 -> do
           ltx <- olImport Nothing (txt (args !! 0))
           linltx <- linearizeStructure ltx
           return linltx
      2 -> do
           startDir <- getCurrentDirectory
           setCurrentDirectory (txt (args !! 0))
           ltx <- olImport Nothing (txt (args !! 0))
           linltx <- linearizeStructure ltx
           setCurrentDirectory startDir
           return linltx

txt :: TeXArg -> String
txt (FixArg (TeXRaw txt)) = convert txt
txt (OptArg (TeXRaw txt)) = convert txt
txt _ = error "called 'txt' on unsuppoted argument constructor"

olImport :: Maybe FilePath -> FilePath -> IO LaTeX
olImport subdir path = do

  let file = case subdir of
               Nothing -> path
               Just sd -> path </> sd

  rawText <- readFile $ file <.> "tex"

  let latex = parseLaTeX $ convert rawText

  case latex of
    Left e -> error $ "could not parse " ++ file ++ ": " ++ (show e)
    Right ltx -> case getBody ltx of
                   Nothing -> error "\\olimported a file with no body"
                   (Just body) -> return body
    
    
    




