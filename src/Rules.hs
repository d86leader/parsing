module Rules
( NonTerminal(..), Symbol(..), Line(..), Rules
) where

-- Description: how context-free parsing rules are formed

import Data.Hashable (Hashable, hashWithSalt)
import Data.HashMap.Lazy (HashMap)

-- nonterminal symbols are special as they appear on lhs of rules
newtype NonTerminal = NonTerminal Char
    deriving (Eq)
-- satisfy some requirements to use them in map
instance Hashable NonTerminal where
    hashWithSalt x (NonTerminal c) = hashWithSalt x c
instance Show NonTerminal where
    show (NonTerminal c) = show c

-- symbols used in rules. Wildcard is terminal matching any symbol
data Symbol  = Nonterm NonTerminal | Term Char | Wildcard
instance Show Symbol where
    show (Nonterm x) = show x
    show (Term x) = show x
    show Wildcard = "*"
newtype Line = Line [Symbol]
    deriving (Show)

-- context-free rules
type Rules = HashMap NonTerminal [Line]
