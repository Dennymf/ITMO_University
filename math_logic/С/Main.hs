module Main where

import Parser
import Lexer
import Rules
import Data.Char
import qualified Data.Map as Map

main :: IO ()
main = do
    input <- getContents
    let firstLine = (head (lines input))
    let statement = parseStatement (scanTokens firstLine)
    let result = getResult statement
    let exprs = map(parser . scanTokens) (filter (not . all isSpace) (tail (lines input)))
    let temp = iter exprs 0 Map.empty Map.empty
    let out = ("|-" ++ show (result)) : (toListString temp 1)
    case getLast temp of
        Error _  -> putStrLn(printList out)
        rule -> putStrLn((printList out) ++ (if getExpr rule /= result then "The proof proves different expression." else ""))