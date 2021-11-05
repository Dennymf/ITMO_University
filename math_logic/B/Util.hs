module Util where

import Lexer
import Parser
import qualified Data.Map as Map

data Rule
  = Axiom Int Int
  | Hypothesis Int
  | ModusPonens Int Int Int
   deriving (Show, Eq, Ord)

putString :: [Expr] -> String
putString expr = case expr of
    [] -> ""
    (x:[]) -> do
        show x
    (x:xs) -> do
        show x ++ ", " ++ putString(xs)

putList :: [String] -> IO()
putList list = case list of
    [] -> putStrLn ""
    (x:xs) -> do
        putStrLn (x)
        putList xs

listToString :: [String] -> String
listToString list = case list of
    [] -> ""
    (x:xs) -> do
        x ++ "\n" ++ listToString xs

listLast :: [a] -> a
listLast [x] = x
listLast (_:xs) = listLast xs

convertToListExpr :: Exprs -> [Expr]
convertToListExpr (Exprs es) = es
convertToListExpr Empty = []

searchInList :: [Expr] -> Expr -> Bool
searchInList [] expr = False
searchInList [expr1] expr =
    if expr1 /= expr
    then
        False
    else
        True
searchInList (expr1:exprs) expr =
    if expr1 == expr
    then
        True
    else
      searchInList exprs expr

add :: Expr -> Map.Map Expr [(Expr, Int)] -> Int -> Map.Map Expr [(Expr, Int)]
add (v :->: k) impls ind =
    case Map.lookup k impls of
        Just vs -> Map.insert k ((v, ind) : vs) impls
        Nothing -> Map.insert k [(v, ind)] impls
add _ impls _ = impls

getExpr :: Map.Map Int Expr -> Int -> Expr
getExpr map key = case Map.lookup key map of
    (Just value) -> value

getLineIndex :: Rule -> Int
getLineIndex rule = case rule of
    Axiom line _         -> line
    Hypothesis line      -> line
    ModusPonens line _ _ -> line