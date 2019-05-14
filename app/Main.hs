{-# LANGUAGE ImplicitParams #-}
module Main where

import TopDown (match, matchHistory)
import Rules (NonTerminal(..), Symbol(..), Line(..), Rules, Phrase
             ,(**), nil, literal)
import Data.HashMap.Lazy (fromList)
import Precede (leftPrec, rightPrec)
import qualified PrettyDeriv as P (print)
import Prelude hiding ((**))


a = NonTerminal 'A'
b = NonTerminal 'B'
b' = NonTerminal 'b'
t = NonTerminal 'T'
t' = NonTerminal 't'
m = NonTerminal 'M'
start = a

aRhs = '!' ** b **'!' ** nil
     : []

bRhs = literal b'
     : []

b'Rhs = literal t
      : b' ** '+' ** t ** nil
      : []

tRhs = literal t'
     : []

t'Rhs = literal m
      : t' ** '*' ** m ** nil
      : []

mRhs = literal 'a'
     : literal 'b'
     : literal 'c'
     : literal 'd'
     : '(' ** b ** ')' ** nil
     : []

rulesList = [(a, aRhs)
            ,(b, bRhs)
            ,(b', b'Rhs)
            ,(t, tRhs)
            ,(t', t'Rhs)
            ,(m, mRhs)
            ]
rules = fromList rulesList


main :: IO ()
main = do
    let ?rules = rules
    print $ rightPrec a
    print $ rightPrec b
    print $ rightPrec b'
    print $ rightPrec t
    print $ rightPrec t'
    print $ rightPrec m

derivationLoop :: IO ()
derivationLoop = do
    putStrLn "input string"
    str <- getLine
    if str == ""
    then return ()
    else let history = matchHistory rules start str
         in putStr (P.print history) >> main
