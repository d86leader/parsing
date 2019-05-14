{-# LANGUAGE ImplicitParams #-}
module Precede
(
) where


import Data.HashSet (HashSet)
import Data.HashMap.Lazy (HashMap, (!))
import Rules (NonTerminal(..), Symbol(..), Line(..), Grammar(..), Rules
             ,leftmost, rightmost)

type Set = HashSet


-- conveniece comparisons
(==>) :: (?rules :: Rules) => NonTerminal -> Line -> Bool
nt ==> line = line `elem` (?rules ! nt)

-- symbol is leftmost in any rhs of derivation from nt
lderiv :: (?rules :: Rules) => NonTerminal -> Symbol -> Bool
nt `lderiv` sym = or [leftmost line == sym | line <- ?rules ! nt]
-- same but symbol must be the last i nline
rderiv :: (?rules :: Rules) => NonTerminal -> Symbol -> Bool
nt `rderiv` sym = or [rightmost line == sym | line <- ?rules ! nt]


-- leftPrec :: (?rules :: Rules) => Symbol -> Set Symbol
-- rightPrec :: Symbol -> Set Symbol
