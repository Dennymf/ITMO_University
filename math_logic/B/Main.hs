module Main where

import Lexer
import Parser
import Axioms
import Checker
import Util
import qualified Data.Map as Map

main = do
    input <- getContents
    let (Head hypothesis toProof proof) = parseHead (scanTokens input)
    let hypotheses = convertToListExpr hypothesis
    if toProof == (listLast proof)
    then
        case checkProof Map.empty hypotheses Map.empty proof 1 Map.empty of
            (Right (prove, index)) ->  putStrLn $ treeToString hypotheses toProof index prove 0
            (Left index)           ->  putStrLn $ "Proof is incorrect at line " ++  show (index + 1)
    else
        putStrLn "The proof does not prove the required expression"
