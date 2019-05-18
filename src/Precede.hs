{-# LANGUAGE ImplicitParams, GADTSyntax #-}
module Precede
( leftPrec, rightPrec
, precedenceList
) where


import Data.Either (rights)
import Control.Monad (join)
import Data.HashSet (HashSet, unions)
import Data.HashMap.Lazy (HashMap, (!), elems)
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
        immNonterm = filter (/= sym) . rights . map toEither $ immediate
        recs = map leftPrec immNonterm
    in unions $ Set.fromList immediate : recs
rightPrec :: (?rules :: Rules) => NonTerminal -> Set Symbol
rightPrec sym =
    let immediate = allRderived sym
        immNonterm = filter (/= sym) . rights . map toEither $ immediate
        recs = map rightPrec immNonterm
    in unions $ Set.fromList immediate : recs

-- list version
leftPrec' :: (?rules :: Rules) => NonTerminal -> [Symbol]
leftPrec' = Set.toList . leftPrec
rightPrec' :: (?rules :: Rules) => NonTerminal -> [Symbol]
rightPrec' = Set.toList . rightPrec


data Precedence =
    Symbol :=. Symbol
    | Symbol :>. Symbol
    | Symbol :<. Symbol
    | Symbol `NoComp` Symbol
    deriving (Show)

precedenceList :: Rules -> [Precedence]
precedenceList rules =
    let ?rules = rules in
    let rightHands = map (\(Line l) -> l) . join . elems $ rules
        consecPairs list = zip list $ tail list
        pairs = join . map consecPairs $ rightHands
        --
        eqs = map (uncurry (:=.)) pairs
        lefts = join . map findLefts $ pairs
        rights = join . map findRights $ pairs
    in eqs ++ lefts ++ rights

findLefts :: (?rules :: Rules) => (Symbol, Symbol) -> [Precedence]
findLefts (Term left, Nonterm right) =
    [(Term left) :<. r
        | r <- leftPrec' right
    ]
findLefts _ = []

findRights :: (?rules :: Rules) => (Symbol, Symbol) -> [Precedence]
findRights (Nonterm left, Term right) =
    [l :>. (Term right)
        | l <- rightPrec' left
    ]
findRights (Nonterm left, Nonterm right) =
    [l :>. r
        | l <- rightPrec' left
        , r <- leftPrec' right
    ]
findRights _ = []
