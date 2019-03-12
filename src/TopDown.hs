module TopDown
( match
, matchHistory
) where

import Rules (NonTerminal, Symbol(..), Line(..), Rules)
import Data.HashMap.Lazy ((!))
import Control.Monad ((>=>), mzero, liftM, ap)


-- used to show parse result. First is what is left to parse, second is
-- parse history
data ParseNode a = ParseNode a [Line]

instance Functor ParseNode where
    fmap f (ParseNode x ss) = ParseNode (f x) ss
instance Applicative ParseNode where
    pure x = ParseNode x []
    (ParseNode f ss1) <*> (ParseNode x ss2) =
        ParseNode (f x) (ss1 ++ ss2)
instance Monad ParseNode where
    (ParseNode x ss1) >>= f =
        let ParseNode r ss2 = f x
        in ParseNode r (ss1 ++ ss2)


-- i want to have list envelope my ParseNode, so i define it as transformer
-- also
data ParseNodeT m a = ParseNodeT {runParseNodeT :: m (ParseNode a)}

instance Monad m => Monad (ParseNodeT m) where
    return = ParseNodeT . return . return
    -- f :: a -> t m b
    (ParseNodeT mtx) >>= f = ParseNodeT $ do
        ParseNode x ss1 <- mtx
        ParseNode y ss2 <- runParseNodeT $ f x
        return $ ParseNode y (ss1 ++ ss2)
-- also lesser instances defined with monadic operations
instance Monad m => Functor (ParseNodeT m) where
    fmap = liftM
instance Monad m => Applicative (ParseNodeT m) where
    pure = return
    (<*>) = ap


-- return type of parsing subfunctions
-- each entry tells how matching went
-- so [] means no match
type ParseResult = ParseNodeT [] String

parsed :: String -> [Line] -> ParseResult
parsed symb ls = ParseNodeT [ParseNode symb ls]
noParse :: ParseResult
noParse = ParseNodeT []

-- put all values inside nodes
pack :: [Line] -> ParseNodeT [] Line
pack = ParseNodeT . map putAndLog where
    putAndLog line =
        ParseNode line [line]


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


emptyNode :: ParseNode String -> Bool
emptyNode (ParseNode str _) = str == ""

didMatch :: ParseResult -> Bool
didMatch (ParseNodeT []) = False
didMatch (ParseNodeT rs) = any emptyNode rs

getHistory :: ParseResult -> [Line]
getHistory (ParseNodeT []) = []
getHistory (ParseNodeT rs) =
    let nodesLeft = dropWhile (not . emptyNode) rs
    in case nodesLeft of
        [] -> []
        (ParseNode _ his):_ -> his


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
