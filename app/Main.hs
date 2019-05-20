{-# LANGUAGE ImplicitParams #-}
module Main where

import TopDown (match, matchHistory)
import Rules (NonTerminal(..), Symbol(..), Line(..), Rules, Phrase
             ,(**), nil, literal)
import Data.HashMap.Lazy (fromList)
import Precede (leftPrec, rightPrec, precedenceList)
import ParsePrecede (parse)
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
main = derivationLoop
--     let ?rules = rules
--     print $ rightPrec a
--     print $ rightPrec b
--     print $ rightPrec b'
--     print $ rightPrec t
--     print $ rightPrec t'
--     print $ rightPrec m
--     putStrLn "Precedence matrix:"
--     mapM_ print $ precedenceList rules

derivationLoop :: IO ()
derivationLoop = do
    putStrLn "input string"
    str <- getLine
    if str == ""
    then return ()
    else ( putStrLn "top-down:" >>
--            deriveTD str >>
           putStrLn "precedence:" >>
           derivePrec str >>
           derivationLoop
         )

deriveTD :: String -> IO ()
deriveTD str = let history = matchHistory rules start str
               in putStr (P.print history)

derivePrec :: String -> IO ()
derivePrec str = let ?rules = rules in
                 let matrix = fromList $ precedenceList rules
                     history = parse matrix rules start str
                 in mapM_ prettyPrint history
    where prettyPrint (nt, line) =
            putStrLn $ show nt ++ " -> " ++ show line
