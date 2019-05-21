module ParsePrecede
( parse
) where


import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import Control.Monad (join)
import Precede (Prec(..))
import Rules (NonTerminal, Symbol(..), Rules, Line(..))
import Data.HashMap.Lazy (HashMap, (!), lookup)
import qualified Data.HashMap.Lazy as HashMap

import Prelude hiding (lookup)


parse :: HashMap (Symbol, Symbol) Prec
      -> Rules -> NonTerminal
      -> String -> [(NonTerminal, Line)]
parse precedence rules start str =
    let str' = map Term str
    in fromMaybe [] $ parse' [] str'
    where
    --
    revRules = HashMap.fromList . join . map rev . HashMap.toList $ rules
    rev (nt, ls) = zip ls $ repeat nt
    --
    recMaybe :: a -> Maybe [a] -> Maybe [a]
    recMaybe _ Nothing = Nothing
    recMaybe x (Just xs) = Just (x:xs)
    --
    parse' :: [Symbol] -> [Symbol] -> Maybe [(NonTerminal, Line)]
    parse' [] (first:str) = parse' [first] str
    parse' [start] []     = Just []
    parse' (top:stack) (first:str) =
        case lookup (top, first) precedence  of
            Nothing -> Nothing
            Just LeftPrec -> parse' (first:top:stack) str
            Just EqPrec ->   parse' (first:top:stack) str
            Just RightPrec ->
                case fold (top:stack) of
                    ([], _, _) -> Nothing
                    (stack', nt, l) ->
                        recMaybe (nt, l) $ parse' stack' (first:str)
    parse' stack [] =
        case fold stack of
            ([], _, _) -> Nothing
            (stack', nt, l) -> recMaybe (nt, l) $ parse' stack' []
    -- fold the stack according to precedence and rules
    -- empty fold signals error as any fold returns at least a nonterminal
    -- also return a rule used for folding
    fold :: [Symbol] -> ([Symbol], NonTerminal, Line)
    fold stack =
        let (base, rest) = splitEqPrec stack
        in case lookup (Line base) revRules of
            Nothing -> ([], undefined, undefined)
            Just nt -> let stack' = (Nonterm nt) : rest
                       in (stack', nt, Line base)
    --
    eqPrec :: (Symbol, Symbol) -> Bool
    eqPrec (s1, s2) =
        case lookup (s2, s1) precedence of -- swapped because stack grows backwards
            Just EqPrec -> True
            otherwise -> False
--         precedence ! (s1, s2) == EqPrec
    --
    splitEqPrec :: [Symbol] -> ([Symbol], [Symbol])
    splitEqPrec syms =
        let conseq = zip syms $ tail syms
            (base', rest') = break (not . eqPrec) conseq
            base = reverse $ head syms : map snd base'
            rest = map snd rest'
        in (base, rest)
