module TopDown
( match
, matchHistory
) where

import Rules (NonTerminal, Symbol(..), Line(..), Rules)
import Data.HashMap.Lazy ((!))
import Control.Monad ((>=>), mzero, liftM, ap)
import Control.Monad.Writer.Lazy (WriterT(..), writer, runWriterT)


-- Used to show parse result. Main value is what is left to parse, written
-- value is parse history
-- i want to have list envelope my ParseNode, so i define it as transformer
type ParseTree = WriterT [Line] []

-- return type of parsing subfunctions
-- each entry tells how matching went
-- so [] means no match
type ParseResult = ParseTree String

parsed :: String -> [Line] -> ParseResult
parsed symb ls = writer (symb, ls)
noParse :: ParseResult
noParse = return []

-- put all values inside nodes
pack :: [Line] -> ParseTree Line
pack = WriterT . map putAndLog where
    putAndLog line =
        (line, [line])


matchesSymbol :: Rules -> Symbol -> String -> ParseResult
-- wildcard always matches
matchesSymbol  _  Wildcard  (c:cs)  = parsed cs []
-- matching symbol must be exact
matchesSymbol  _  (Term x)  (c:cs)
    | x == c     = parsed cs []
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
    pack lines >>= matchesLine rules str

-- tells whether string matches the rule line
matchesLine :: Rules -> String -> Line -> ParseResult
matchesLine _ str (Line []) = parsed str [] -- no symbols means consume nothing
matchesLine rules str (Line syms) =
        -- a function for each symbol that tells whether string matches it
    let matchers  = map (matchesSymbol rules) syms
        -- a function that is like applying all matches one after another
        matchPipe = foldr1 (>=>) matchers
    in matchPipe str


emptyNode :: (String, w) -> Bool
emptyNode (str, _) = str == ""

didMatch :: ParseResult -> Bool
didMatch tree = case runWriterT tree of
    [] -> False
    rs -> any emptyNode rs
-- didMatch (WriterT []) = False
-- didMatch (WriterT rs) = any emptyNode rs

getHistory :: ParseResult -> [Line]
getHistory tree = case runWriterT tree of
    [] -> []
    rs -> let nodesLeft = dropWhile (not . emptyNode) rs
          in case nodesLeft of
              [] -> []
              (_, his):_ -> his


match :: Rules -> NonTerminal -> String -> Bool
match  rules  start  str =
    let rule = rules ! start
        result = matchesRule rules rule str
    in didMatch result

matchHistory :: Rules -> NonTerminal -> String -> [Line]
matchHistory  rules  start  str =
    let rule = rules ! start
        result = matchesRule rules rule str
    in getHistory result
