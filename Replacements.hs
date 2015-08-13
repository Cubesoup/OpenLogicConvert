module Replacements where

import Types
import Text.LaTeX.Base.Syntax
import qualified Data.Map as Map

data RuleInstance = RuleInstance { name :: String
                                 , args :: [TeXArg] }


followReplaceRule :: OLConfig -> RuleInstance -> Maybe String
followReplaceRule cfg rule =
    case Map.lookup (name rule) (replaceMap cfg) of
      Nothing -> Nothing -- no replacement occurs
      Just r  -> Just $ computeInstanceString rule r

computeInstanceString :: RuleInstance -> OLReplace -> String
computeInstanceString rule replace = undefined

toOLArgument :: TeXArg -> OLArgument
toOLArgument (FixArg ltx) = Mandatory (render ltx)
toOLArgument (OptArg ltx) = Optional (render ltx)
toOLArgument _ = error "can't convert argument with 'toOLArgument'"


-- 1. is rule for which we have instance in config?
--    if not, don't touch it. if yes, get its OLReplace and continue to 2.
-- 2. match arguments to their counterparts (TeXArg <-> OLArgument)
--    (requires some thought).
-- 3. if all mandatory arguments supplied and a case for optional arguments
--    supplied exists in 'cases', compute replacement string by replacing
--    the named arguments (#arg1) in the string with their counterparts
--    from the arguments supplied. (literal string replacement? {}?).
-- 4a. option a is to parse the new string as LaTeX and replace the old
--     command with it in the "AST".
-- 4b. option b is to insert the text verbatim into the "AST". Does this
--     work?
-- probably go with option a.

