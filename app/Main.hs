module Main where

import TopDown (match)
import Rules (NonTerminal(..), Symbol(..), Line(..), Rules)
import Data.HashMap.Lazy (fromList)


start = NonTerminal 'S'
number = NonTerminal 'N'
operation = NonTerminal 'O'

exprRhs = [Line [Nonterm number, Nonterm operation, Nonterm start]
          ,Line [Nonterm number]
          ]

numberRhs = [Line []
            ,Line [Term '0', Nonterm number]
            ,Line [Term '1', Nonterm number]
            ]

operationRhs = [Line [Term '+']
               ,Line [Term '*']
               ]


rulesList = [( start, exprRhs )
            ,( number, numberRhs )
            ,( operation, operationRhs )
            ]

rules = fromList rulesList


main :: IO ()
main = do
    putStrLn "input string"
    str <- getLine
    if str == ""
    then return ()
    else if match rules start str
        then putStrLn "Matches" >> main
        else putStrLn "No match" >> main
