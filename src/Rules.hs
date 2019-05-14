{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rules
( NonTerminal(..), Symbol(..), Line(..), nil, Rules
, Phrase, (**), literal, Grammar(..)
, leftmost, rightmost
, toEither
) where

-- Description: how context-free parsing rules are formed

import Data.Hashable (Hashable, hashWithSalt)
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Prelude hiding ((**))

-- nonterminal symbols are special as they appear on lhs of rules
newtype NonTerminal = NonTerminal Char
    deriving (Eq)
-- satisfy some requirements to use them in map
instance Show NonTerminal where
    show (NonTerminal x) = '|':x:'|':[]
instance Hashable NonTerminal where
    hashWithSalt x (NonTerminal c) = hashWithSalt x c

-- symbols used in rules
data Symbol  = Nonterm NonTerminal | Term Char
    deriving (Eq)
instance Show Symbol where
    show (Nonterm x) = show x
    show (Term x) = show x
instance Hashable Symbol where
    hashWithSalt x (Nonterm nt) = hashWithSalt x nt
    hashWithSalt x (Term c) = hashWithSalt x c

newtype Line = Line [Symbol]
    deriving (Show, Eq, Semigroup, Monoid)
nil :: Line
nil = mempty

toEither :: Symbol -> Either Char NonTerminal
toEither (Nonterm nt) = Right nt
toEither (Term c) = Left c

-- convenience functions
leftmost :: Line -> Symbol
leftmost (Line l) = head l
rightmost :: Line -> Symbol
rightmost (Line l) = last l


-- context-free rules
type Rules = HashMap NonTerminal [Line]
-- context-free grammar
newtype Grammar = Grammar (HashSet NonTerminal, HashSet Char, NonTerminal, Rules)


-- Class for ease of writing rules
class Show a => Phrase a where
    literal :: a -> Line
    (**) :: a -> Line -> Line
    --
    literal = Line . map Term . show
    x ** l = literal x <> l
infixr 6 **

instance Phrase NonTerminal where
    literal x = Line [Nonterm x]
    x ** (Line l) = Line $ (Nonterm x) : l
instance Phrase Char where
    literal c = Line [Term c]
    c ** (Line l) = Line $ (Term c) : l
