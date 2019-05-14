module PrettyDeriv
( print
) where

-- Description: pretty printing the list of derivations

import Prelude hiding (print)
import Rules (NonTerminal(..), Symbol(..), Line(..))

print :: [Line] -> String
print = runPrint ("", [NonTerminal 'S'])


type Stack = [NonTerminal]

runPrint :: (String, Stack) -> [Line] -> String
runPrint (s, _) [] = s
runPrint (str, stck) (line:rest) =
    let (str', stck') = printLine stck line
    in runPrint (str ++ str', stck') rest

printLine :: Stack -> Line -> (String, Stack)
printLine [] line = error "Could not read from stack"
printLine (n:rest) line =
    let (rhs, stack) = printRhs ("", []) line
        r = show n ++ " -> " ++ reverse rhs
    in (r, reverse stack ++ rest)

-- prints line reversed
printRhs :: (String, Stack) -> Line -> (String, Stack)
printRhs (str, stck) (Line []) = ('\n':str, stck)
printRhs (str, stck) (Line ((Term c):rest)) =
    printRhs (c:str, stck) (Line rest)
printRhs (str, stck) (Line ((Nonterm n):rest)) =
    printRhs (show n ++ str, n : stck) (Line rest)
