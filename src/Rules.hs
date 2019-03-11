module Rules
( NonTerminal(..), Symbol(..), Line(..), Rules
) where

-- Description: how context-free parsing rules are formed

import Data.Hashable (Hashable, hashWithSalt)
import Data.HashMap.Lazy (HashMap)

-- nonterminal symbols are special as they appear on lhs of rules
newtype NonTerminal = NonTerminal Char
    deriving (Eq, Show)
-- satisfy some requirements to use them in map
instance Hashable NonTerminal where
    hashWithSalt x (NonTerminal c) = hashWithSalt x c

-- symbols used in rules. Wildcard is terminal matching any symbol
data Symbol  = Nonterm NonTerminal | Term Char | Wildcard
    deriving (Show)
newtype Line = Line [Symbol]

-- context-free rules
type Rules = HashMap NonTerminal [Line]
