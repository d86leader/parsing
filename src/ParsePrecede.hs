module ParsePrecede
(
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
      -> [Symbol] -> String -> [Line]
parse precedence rules start stack str =
    let str' = map Term str
    in if length rules /= length (HashMap.toList rules)
       then error "Rules are not stratified"
       else fromMaybe [] $ parse' [] str'
    where
    --
    revRules = HashMap.fromList . join . map rev . HashMap.toList $ rules
    rev (nt, ls) = zip ls $ repeat nt
    --
    parse' :: [Symbol] -> [Symbol] -> Maybe [Line]
    parse' [] (first:str) = parse' [first] str
    parse' [start] []     = Just []
    parse' (top:stack) (first:str) =
        case precedence ! (top, first) of
            LeftPrec -> parse' (first:top:stack) str
            EqPrec ->   parse' (first:top:stack) str
            RightPrec -> case fold (top:stack) of
                           [] -> Nothing
                           stack' -> parse' stack' (first:str)
    parse' _ _ = Nothing
    -- fold the stack according to precedence and rules
    -- empty fold signals error as any fold returns at least a nonterminal
    fold :: [Symbol] -> [Symbol]
    fold stack =
        let (base, rest) = splitEqPrec stack
        in case lookup (Line base) revRules of
            Nothing -> []
            Just nt -> (Nonterm nt) : rest
    --
    eqPrec :: (Symbol, Symbol) -> Bool
    eqPrec (s1, s2) =
        precedence ! (s1, s2) == EqPrec
    --
    splitEqPrec :: [Symbol] -> ([Symbol], [Symbol])
    splitEqPrec = undefined
