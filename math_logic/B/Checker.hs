module Checker where

import Lexer
import Parser
import Axioms
import Util
import qualified Data.Map as Map
import Data.Maybe

checkProof :: Map.Map Expr Rule -> [Expr] -> Map.Map Expr [(Expr, Int)] -> [Expr] -> Int -> Map.Map Int Expr -> Either Int (Map.Map Expr Rule, Map.Map Int Expr)
checkProof prove hypotheses impls (expr : exprs) line indexs =
    case checkRules of
        (Just prove') ->
            if exprs == []
            then
                Right (prove', Map.insert line expr indexs)
            else
                checkProof prove' hypotheses (add expr impls line) exprs (line + 1) (Map.insert line expr indexs)
        Nothing -> Left (line)
        where
            checkRules = case Map.lookup expr prove of
                    (Just _) -> Just prove
                    _        -> case axiom expr of
                        (Just num) -> Just $ Map.insert expr (Axiom line num) prove
                        _          -> case checkHypotheses of
                                (Just rule) -> Just $ Map.insert expr rule prove
                                _           -> case checkModusPonens of
                                        (Just rule) -> Just $ Map.insert expr rule prove
                                        Nothing -> Nothing
            checkHypotheses = case searchInList hypotheses expr of
                    True  -> Just (Hypothesis line)
                    False -> Nothing
            checkModusPonens =
                case Map.lookup expr impls of
                    Just list ->
                        case checkModusPonens' list of
                            (Just (ind1, ind2)) -> Just (ModusPonens line ind1 ind2)
                            Nothing -> Nothing
                    Nothing -> Nothing
            checkModusPonens' list =
                case filter (\(expr', indx) -> isJust (Map.lookup expr' prove)) list of
                    ((expr'', line) : _) ->
                        case Map.lookup expr'' prove of
                            (Just rule) -> Just(line, getLineIndex rule)
                            _           -> error "Check ModusPonens"
                    []            -> Nothing
