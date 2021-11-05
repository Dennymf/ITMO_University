module Axioms where

import Lexer
import Parser
import Util
import qualified Data.Map as Map

axiom :: Expr -> Maybe Int
axiom expr@(a :->: (b :->: a'))
    | a == a' = Just 1
axiom expr@((a :->: b) :->: ((a' :->: (b' :->: c)) :->: (a'' :->: c')))
    | a == a' && a == a'' && b == b' && c == c' = Just 2
axiom expr@(a :->: (b :->: (a' :&: b')))
    | a == a' && b == b' = Just 3
axiom expr@((a :&: b) :->: a')
    | a == a' = Just 4
axiom expr@((a :&: b) :->: b')
    | b == b' = Just 5
axiom expr@(a :->: (a' :|: b))
    | a == a' = Just 6
axiom expr@(b' :->: (a :|: b))
    | b == b' = Just 7
axiom expr@((a :->: c) :->: ((b :->: c') :->: ((a' :|: b') :->: c'')))
    | a == a' && b == b' && c == c' && c == c'' = Just 8
axiom expr@((a :->: b) :->: ((a' :->: Neg b') :->: Neg a''))
    | a == a' && a == a'' && b == b' = Just 9
axiom expr@(a :->: (Neg a' :->: b))
    | a == a' = Just 10
axiom _ = Nothing

convertToListString :: Int -> [Expr] -> Expr -> Int -> [String]
convertToListString depth hypotheses expr 1 = do
    let (a :->: (b :->: a')) = expr
    [
        "[" ++ show(depth + 2) ++ "] " ++ putString(a : b : hypotheses)  ++ " |- " ++ show(a') ++ " [Ax]",
        "[" ++ show(depth + 1) ++ "] " ++ putString(a : hypotheses)  ++ " |- " ++ show(b :->: a') ++ " [I->]",
        "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [I->]"]
convertToListString depth hypotheses expr 2 = do
    let ((a :->: b) :->: ((a' :->: (b' :->: c)) :->: (a'' :->: c'))) = expr
    [
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: b) : (a' :->: (b' :->: c)) : a'' : hypotheses)  ++ " |- " ++ show(a :->: (b :->: c')) ++ " [Ax]",
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: b) : (a' :->: (b' :->: c)) : a'' : hypotheses)  ++ " |- " ++ show(a) ++ " [Ax]",
        "[" ++ show(depth + 4) ++ "] " ++ putString((a :->: b) : (a' :->: (b' :->: c)) : a'' : hypotheses)  ++ " |- " ++ show(b :->: c') ++ " [E->]",
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: b) : (a' :->: (b' :->: c)) : a'' : hypotheses)  ++ " |- " ++ show(a :->: b) ++ " [Ax]",
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: b) : (a' :->: (b' :->: c)) : a'' : hypotheses)  ++ " |- " ++ show(a) ++ " [Ax]",
        "[" ++ show(depth + 4) ++ "] " ++ putString((a :->: b) : (a' :->: (b' :->: c)) : a'' : hypotheses)  ++ " |- " ++ show(b) ++ " [E->]",
        "[" ++ show(depth + 3) ++ "] " ++ putString((a :->: b) : (a' :->: (b' :->: c)) : a'' : hypotheses)  ++ " |- " ++ show(c') ++ " [E->]",
        "[" ++ show(depth + 2) ++ "] " ++ putString((a :->: b) : (a' :->: (b' :->: c)) : hypotheses)  ++ " |- " ++ show(a'' :->: c') ++ " [I->]",
        "[" ++ show(depth + 1) ++ "] " ++ putString((a :->: b) : hypotheses)  ++ " |- " ++ show((a' :->: (b' :->: c)) :->: (a'' :->: c')) ++ " [I->]",
        "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [I->]"]
convertToListString depth hypotheses expr 3 = do
    let (a :->: (b :->: (a' :&: b'))) = expr
    [
        "[" ++ show(depth + 3) ++ "] " ++ putString(a : b : hypotheses)  ++ " |- " ++ show(a) ++ " [Ax]",
        "[" ++ show(depth + 3) ++ "] " ++ putString(a : b : hypotheses)  ++ " |- " ++ show(b) ++ " [Ax]",
        "[" ++ show(depth + 2) ++ "] " ++ putString(a : b : hypotheses)  ++ " |- " ++ show(a' :&: b') ++ " [I&]",
        "[" ++ show(depth + 1) ++ "] " ++ putString(a : hypotheses)  ++ " |- " ++ show(b :->: (a' :&: b')) ++ " [I->]",
        "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [I->]"]
convertToListString depth hypotheses expr 4 = do
    let ((a :&: b) :->: a') = expr
    [
        "[" ++ show(depth + 2) ++ "] " ++ putString((a :&: b) : hypotheses) ++ " |- " ++ show(a :&: b) ++ " [Ax]",
        "[" ++ show(depth + 1) ++ "] " ++ putString((a :&: b) : hypotheses) ++ " |- " ++ show(a) ++ " [El&]",
        "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [I->]"]
convertToListString depth hypotheses expr 5 = do
    let ((a :&: b) :->: b') = expr
    [
        "[" ++ show(depth + 2) ++ "] " ++ putString((a :&: b) : hypotheses) ++ " |- " ++ show(a :&: b) ++ " [Ax]",
        "[" ++ show(depth + 1) ++ "] " ++ putString((a :&: b) : hypotheses) ++ " |- " ++ show(b) ++ " [Er&]",
        "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [I->]"]
convertToListString depth hypotheses expr 6 = do
    let (a :->: (a' :|: b)) = expr
    [
        "[" ++ show(depth + 2) ++ "] " ++ putString(a : hypotheses) ++ " |- " ++ show(a') ++ "[Ax]",
        "[" ++ show(depth + 1) ++ "] " ++ putString(a : hypotheses) ++ " |- " ++ show(a' :|: b) ++ "[Il|]",
        "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [I->]"]
convertToListString depth hypotheses expr 7 = do
    let (b :->: (a :|: b')) = expr
    [
        "[" ++ show(depth + 2) ++ "] " ++ putString(b : hypotheses) ++ " |- " ++ show(b) ++ "[Ax]",
        "[" ++ show(depth + 1) ++ "] " ++ putString(b : hypotheses) ++ " |- " ++ show(a :|: b) ++ "[Ir|]",
        "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [I->]"]
convertToListString depth hypotheses expr 8 = do
    let ((a :->: c) :->: ((b :->: c') :->: ((a' :|: b') :->: c''))) = expr
    [
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: c) : (b :->: c) : (a :|: b) : a : hypotheses)  ++ " |- " ++ show(a :->: c) ++ " [Ax]",
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: c) : (b :->: c) : (a :|: b) : a : hypotheses)  ++ " |- " ++ show(a) ++ " [Ax]",
        "[" ++ show(depth + 4) ++ "] " ++ putString((a :->: c) : (b :->: c) : (a :|: b) : a : hypotheses)  ++ " |- " ++ show(c) ++ " [E->]",
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: c) : (b :->: c) : (a :|: b) : b : hypotheses)  ++ " |- " ++ show(b :->: c) ++ " [Ax]",
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: c) : (b :->: c) : (a :|: b) : b : hypotheses)  ++ " |- " ++ show(b) ++ " [Ax]",
        "[" ++ show(depth + 4) ++ "] " ++ putString((a :->: c) : (b :->: c) : (a :|: b) : b : hypotheses)  ++ " |- " ++ show(c) ++ " [E->]",
        "[" ++ show(depth + 4) ++ "] " ++ putString((a :->: c) : (b :->: c) : (a :|: b) : hypotheses)  ++ " |- " ++ show(a :|: b) ++ " [Ax]",
        "[" ++ show(depth + 3) ++ "] " ++ putString((a :->: c) : (b :->: c) : (a :|: b) : hypotheses)  ++ " |- " ++ show(c) ++ " [E|]",
        "[" ++ show(depth + 2) ++ "] " ++ putString((a :->: c) : (b :->: c) : hypotheses)  ++ " |- " ++ show((a :|: b) :->: c) ++ " [I->]",
        "[" ++ show(depth + 1) ++ "] " ++ putString((a :->: c) : hypotheses)  ++ " |- " ++ show((b :->: c) :->: ((a :|: b) :->: c)) ++ " [I->]",
        "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [I->]"]
convertToListString depth hypotheses expr 9 = do
    let ((a :->: b) :->: ((a' :->: Neg b') :->: Neg a'')) = expr
    [
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: b) : (a' :->: Neg b') : a : hypotheses) ++ " |- " ++ show(a :->: Neg b) ++ " [Ax]",
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: b) : (a' :->: Neg b') : a : hypotheses) ++ " |- " ++ show(a) ++ " [Ax]",
        "[" ++ show(depth + 4) ++ "] " ++ putString((a :->: b) : (a' :->: Neg b') : a : hypotheses) ++ " |- " ++ show(Neg b) ++ " [E->]",
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: b) : (a' :->: Neg b') : a : hypotheses) ++ " |- " ++ show(a :->: b) ++ " [Ax]",
        "[" ++ show(depth + 5) ++ "] " ++ putString((a :->: b) : (a' :->: Neg b') : a : hypotheses) ++ " |- " ++ show(a) ++ " [Ax]",
        "[" ++ show(depth + 4) ++ "] " ++ putString((a :->: b) : (a' :->: Neg b') : a : hypotheses) ++ " |- " ++ show(b) ++ " [E->]",
        "[" ++ show(depth + 3) ++ "] " ++ putString((a :->: b) : (a' :->: Neg b') : a : hypotheses) ++ " |- " ++ show(Bottom) ++ " [E->]",
        "[" ++ show(depth + 2) ++ "] " ++ putString((a :->: b) : (a' :->: Neg b') : hypotheses) ++ " |- " ++ show(Neg a'') ++ " [I->]",
        "[" ++ show(depth + 1) ++ "] " ++ putString((a :->: b) : hypotheses) ++ " |- " ++ show((a' :->: Neg b') :->: Neg a'') ++ " [I->]",
        "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [I->]"]
convertToListString depth hypotheses expr 10 = do
    let (a :->: (Neg a' :->: b)) = expr
    [
        "[" ++ show(depth + 5) ++ "] " ++ putString(a : Neg a' : Bottom : hypotheses)  ++ " |- " ++ show(Bottom) ++ " [Ax]",
        "[" ++ show(depth + 4) ++ "] " ++ putString(a : Neg a' : Bottom : hypotheses)  ++ " |- " ++ show(b) ++ " [E_|_]",
        "[" ++ show(depth + 3) ++ "] " ++ putString(a : Neg a' : hypotheses)  ++ " |- " ++ show(Bottom :->: b) ++ " [I->]",
        "[" ++ show(depth + 4) ++ "] " ++ putString(a : Neg a' : hypotheses)  ++ " |- " ++ show(Neg a) ++ " [Ax]",
        "[" ++ show(depth + 4) ++ "] " ++ putString(a : Neg a' : hypotheses)  ++ " |- " ++ show(a) ++ " [Ax]",
        "[" ++ show(depth + 3) ++ "] " ++ putString(a : Neg a' : hypotheses)  ++ " |- " ++ show(Bottom) ++ " [E->]",
        "[" ++ show(depth + 2) ++ "] " ++ putString(a : Neg a' : hypotheses)  ++ " |- " ++ show(b) ++ " [E->]",
        "[" ++ show(depth + 1) ++ "] " ++ putString(a : hypotheses)  ++ " |- " ++ show(Neg a' :->: b) ++ " [I->]",
        "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [I->]"]

treeToString :: [Expr] -> Expr -> Map.Map Int Expr -> Map.Map Expr Rule -> Int -> String
treeToString hypotheses expr index prove depth =
    case Map.lookup expr prove of
        Just (Axiom _ i) -> listToString $ convertToListString depth hypotheses expr i
        Just (Hypothesis _) -> "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [Ax]" ++ "\n"
        Just (ModusPonens _ i j) -> treeToString hypotheses (getExpr index i) index prove (depth + 1) ++
                    treeToString hypotheses (getExpr index j) index prove (depth + 1) ++
                    "[" ++ show(depth) ++ "] " ++ putString(hypotheses)  ++ " |- " ++ show(expr) ++ " [E->]" ++ "\n"