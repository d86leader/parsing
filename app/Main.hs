module Main where

import TopDown (match, matchHistory)
import Rules (NonTerminal(..), Symbol(..), Line(..), Rules)
import Data.HashMap.Lazy (fromList)
import qualified PrettyDeriv as P (print)


start = NonTerminal 'B'
term = NonTerminal 'T'
multipl = NonTerminal 'M'

startRhs = [Line [Nonterm term, Term '+', Nonterm start]
           ,Line [Nonterm term]
           ]

termRhs = [Line [Nonterm multipl]
          ,Line [Nonterm multipl, Term '*', Nonterm term]
          ]

multiplRhs = [Line [Term 'a']
             ,Line [Term 'b']
             ]


rulesList = [( start, startRhs )
            ,( term, termRhs )
            ,( multipl, multiplRhs )
            ]

rules = fromList rulesList


main :: IO ()
main = do
    putStrLn "input string"
    str <- getLine
    if str == ""
    then return ()
    else let history = matchHistory rules start str
         in print (P.print history) >> main
