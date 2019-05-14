module Main where

import TopDown (match, matchHistory)
import Rules (NonTerminal(..), Symbol(..), Line(..), Rules, Language
             ,(<.), (.>), nil, literal)
import Data.HashMap.Lazy (fromList)
import qualified PrettyDeriv as P (print)


a = NonTerminal 'A'
b = NonTerminal 'B'
t = NonTerminal 'T'
m = NonTerminal 'M'
start = a

aRhs = '!' .> b .>'!' .> nil
     : []

bRhs = literal t
     : t .> '+' .> b .> nil
     : t .> '-' .> b .> nil
     : []

tRhs = literal m
     : m .> '*' .> t .> nil
     : m .> '/' .> t .> nil
     : []

mRhs = literal 'a'
     : literal 'b'
     : literal 'c'
     : literal 'd'
     : '(' .> b .> ')' .> nil
     : []

rulesList = [(a, aRhs)
            ,(b, bRhs)
            ,(t, tRhs)
            ,(m, mRhs)
            ]
rules = fromList rulesList


main :: IO ()
main = do
    putStrLn "input string"
    str <- getLine
    if str == ""
    then return ()
    else let history = matchHistory rules start str
         in putStr (P.print history) >> main
