{-# LANGUAGE ImplicitParams #-}
module Precede
( leftPrec, rightPrec
) where


import Data.Either (rights)
import Data.HashSet (HashSet, unions)
import Data.HashMap.Lazy (HashMap, (!))
import Rules (NonTerminal(..), Symbol(..), Line(..), Grammar(..), Rules
             ,leftmost, rightmost, toEither)
import qualified Data.HashSet as Set

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

-- all leftmost nonterminals that can be derived from given
allLderived :: (?rules :: Rules) => NonTerminal -> [Symbol]
allLderived nt =
    let lines = ?rules ! nt
    in map leftmost lines
allRderived :: (?rules :: Rules) => NonTerminal -> [Symbol]
allRderived nt =
    let lines = ?rules ! nt
    in map rightmost lines


-- this is the function l(U) from our lectures
leftPrec :: (?rules :: Rules) => NonTerminal -> Set Symbol
leftPrec sym =
    let immediate = allLderived sym
        immNonterm = rights . map toEither $ immediate
        recs = map leftPrec immNonterm
    in unions $ Set.fromList immediate : recs
rightPrec :: (?rules :: Rules) => NonTerminal -> Set Symbol
rightPrec sym =
    let immediate = allRderived sym
        immNonterm = rights . map toEither $ immediate
        recs = map rightPrec immNonterm
    in unions $ Set.fromList immediate : recs
