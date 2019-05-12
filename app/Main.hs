module Main where

import TopDown (match, matchHistory)
import Rules (NonTerminal(..), Symbol(..), Line(..), Rules)
import Data.HashMap.Lazy (fromList)
import qualified PrettyDeriv as P (print)


a = NonTerminal 'A'
b = NonTerminal 'B'
t = NonTerminal 'T'
m = NonTerminal 'M'
start = a

aRhs = [Line [Term '!', Nonterm b, Term '!']]

bRhs = [Line [Nonterm t]
       ,Line [Nonterm t, Term '+', Nonterm b]
       ,Line [Nonterm t, Term '-', Nonterm b]
       ]

tRhs = [Line [Nonterm m]
       ,Line [Nonterm m, Term '*', Nonterm t]
       ,Line [Nonterm m, Term '/', Nonterm t]
       ]

mRhs = [Line [Term 'd']
       ,Line [Term 'a']
       ,Line [Term 'b']
       ,Line [Term 'c']
       ,Line [Term '(', Nonterm b, Term ')']
       ]

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
