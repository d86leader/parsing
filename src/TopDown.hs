module TopDown
( match
) where

import Rules (NonTerminal, Symbol(..), Line(..), Rules)
import Data.HashMap.Lazy ((!))
import Control.Monad ((>=>))


-- return type of parsing subfunctions
-- each entry tells which portion of the string was matched by branching
-- so [] means no match
type ParseResult = [String]
parsed  = return
noParse = []


matchesSymbol :: Rules -> Symbol -> String -> ParseResult
-- wildcard always matches
matchesSymbol  _  Wildcard  (c:cs)  = parsed cs
-- matching symbol must be exact
matchesSymbol  _  (Term x)  (c:cs)
    | x == c     = parsed cs
    | otherwise  = noParse
-- terminal symbol but no string left - no match
matchesSymbol  _  Wildcard  "" = noParse
matchesSymbol  _  (Term _)  "" = noParse
-- non-terminal must be examined
matchesSymbol  rules  (Nonterm nonterm)  str =
    let rule = rules ! nonterm
    in matchesRule rules rule str


matchesRule :: Rules -> [Line] -> String -> ParseResult
matchesRule  rules  lines  str =
    -- branch to each line with monadic bind
    lines >>= matchesLine rules str

-- tells whether string matches the rule line
matchesLine :: Rules -> String -> Line -> ParseResult
matchesLine _ str (Line []) = parsed str -- no symbols means consume nothing
matchesLine rules str (Line syms) =
        -- a function for each symbol that tells whether string matches it
    let matchers  = map (matchesSymbol rules) syms
        -- a function that is like applying all matches one after another
        matchPipe = foldr1 (>=>) matchers
    in matchPipe str


didMatch :: ParseResult -> Bool
didMatch [] = False
didMatch rs = any (== "") rs


match :: Rules -> NonTerminal -> String -> Bool
match  rules  start  str =
    let rule = rules ! start
        result = matchesRule rules rule str
    in didMatch result
