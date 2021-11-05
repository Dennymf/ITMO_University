module Rules where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust, isNothing)
import Data.Either (isLeft, isRight)
import Lexer
import Parser

data Annotate
    = Axiom Int Expr
    | AxiomSch String Expr
    | ModusPonens Int Int Expr
    | Intro Int String Expr
    | Error String
    deriving (Eq, Show)

getExpr :: Annotate -> Expr
getExpr annotate = case annotate of
    Axiom _ expr -> expr
    AxiomSch _ expr -> expr
    ModusPonens _ _ expr -> expr
    Intro _ _ expr -> expr

axiomSch :: Expr -> Either (String) (Int)
axiomSch expr@(a :->: (b :->: a'))
    | a == a' = Right 1
axiomSch expr@((a :->: b) :->: ((a' :->: (b' :->: c)) :->: (a'' :->: c')))
    | a == a' && a' == a'' && b == b' && c == c' = Right 2
axiomSch expr@(a :->: (b :->: (a' :&: b')))
    | a == a' && b == b' = Right 3
axiomSch expr@((a :&: b) :->: a')
    | a == a' = Right 4
axiomSch expr@((a :&: b) :->: b')
    | b == b' = Right 5
axiomSch expr@(a :->: (a' :|: b))
    | a == a' = Right 6
axiomSch expr@(b' :->: (a :|: b))
    | b == b' = Right 7
axiomSch expr@((a :->: c) :->: ((b :->: c') :->: ((a' :|: b') :->: c'')))
    | a == a' && b == b' && c == c' && c' == c'' = Right 8
axiomSch expr@((a :->: b) :->: ((a' :->: Neg b') :->: Neg a''))
    | a == a' && a' == a'' && b == b' = Right 9
axiomSch expr@((Neg (Neg a)) :->: a')
    | a == a' = Right 10
axiomSch expr = let
    axi11 = axiom11 expr
    axi12 = axiom12 expr
    in if isRight axi11 || isLeft axi12 && axi11 /= Left "is not proved"
    then
        axi11
    else
        axi12

axiom11 :: Expr -> Either (String) (Int)
axiom11 ((ForAll x phi) :->: phi') =
    case findSub x phi phi' of
        Nothing -> Left "is not proved"
        Just v  ->
            if isNothing v || sub x phi phi' (getFreeVar (fromJust v))
            then
                Right 11
            else
                Left ("variable " ++ x ++ " is not free for term " ++ show (fromJust v) ++ " in @-axiom")
axiom11 _ = Left "is not proved"

axiom12 :: Expr -> Either (String) (Int)
axiom12 (phi :->: (Exists x phi')) =
    case findSub x phi' phi of
        Nothing -> Left "is not proved"
        Just v  ->
            if isNothing v || sub x phi' phi (getFreeVar (fromJust v))
            then
                Right 12
            else
                Left ("variable " ++ x ++ " is not free for term " ++ show (fromJust v) ++ " in ?-axiom")
axiom12 _ = Left "is not proved"

forall :: Expr -> Map.Map Expr Int -> Either (String) (Annotate)
forall expr@(psi :->: (ForAll x phi)) indexs
    | Map.member (psi :->: phi) indexs && not (checkFree x psi) = Right(Intro (indexs Map.! (psi :->: phi)) "@" expr)
    | Map.member (psi :->: phi) indexs && checkFree x psi = Left("variable " ++  x ++ " occurs free in @-rule")
    | otherwise                                           = Left "is not proved"
forall _ _                       = Left "is not proved"

exists :: Expr-> Map.Map Expr Int -> Either (String) (Annotate)
exists expr@((Exists x phi) :->: psi) indexs
    | Map.member (phi :->: psi) indexs && not (checkFree x psi) = Right (Intro (indexs Map.! (phi :->: psi)) "?" expr)
    | Map.member (phi :->: psi) indexs && checkFree x psi = Left  ("variable " ++ x ++ " occurs free in ?-rule")
    | otherwise                                           = Left "is not proved"
exists _ _                       = Left "is not proved"

intro :: Expr -> Map.Map Expr Int -> Annotate
intro expr indexs = let
    fr = forall expr indexs
    ex = exists expr indexs
    in if isRight ex || isLeft fr && ex /= Left "is not proved"
    then
        case ex of
            (Right rule) -> rule
            (Left err)   -> Error err
    else
        case fr of
            (Right rule) -> rule
            (Left err)   -> Error err

axiom :: Expr -> Int
axiom expr@(((Var "a") :=: (Var "b")) :->: ((Succ (Var "a")) :=: (Succ (Var "b")))) = 1
axiom expr@(((Var "a") :=: (Var "b")) :->: (((Var "a") :=: (Var "c")) :->: ((Var "b") :=: (Var "c"))))  = 2
axiom expr@(((Succ (Var "a")) :=: (Succ (Var "b"))) :->: ((Var "a") :=: (Var "b"))) = 3
axiom expr@(Neg ((Succ (Var "a")) :=: Zero)) = 4
axiom expr@(((Var "a") :+: (Succ (Var "b"))) :=: (Succ ((Var "a") :+: (Var "b")))) = 5
axiom expr@(((Var "a") :+: Zero) :=: (Var "a")) = 6
axiom expr@(((Var "a") :*: Zero) :=: Zero) = 7
axiom expr@(((Var "a") :*: (Succ (Var "b"))) :=: (((Var "a") :*: (Var "b")) :+: (Var "a"))) = 8
axiom _ = 0

axiom9 :: Expr -> Bool
axiom9 expr@((phi0 :&: (ForAll x (phi :->: phi'))) :->: phii)
    | checkInduction x phi phii phi0 phi' = True
axiom9 _ = False

checkInduction :: String -> Expr -> Expr -> Expr -> Expr -> Bool
checkInduction x phi phii phi0 phi' =
    if phii == phi
    then
        match (findSub x phi phi0) Zero && match (findSub x phi phi') (Succ (Var x))
    else
        False
    where
        match a b = case (a, b) of
            (Nothing, _) -> False
            (Just Nothing, _) -> True
            (Just (Just t), t') -> t == t'

checkSub :: Eq a => String -> t2 -> t2 -> t3 -> t3 -> Bool -> (String -> t2 -> t3 -> Bool -> Maybe (Maybe a)) -> Maybe (Maybe a)
checkSub x a b a' b' f func =
    case (func x a a' f, func x b b' f) of
        (Nothing, _)           -> Nothing
        (_, Nothing)           -> Nothing
        (Just p, Just Nothing) -> Just p
        (Just Nothing, Just p) -> Just p
        (Just p, Just p')      -> if p == p' then Just p else Nothing

findSub :: String -> Expr -> Expr -> Maybe (Maybe Term)
findSub x expr1 expr2 = findSubExpr x expr1 expr2 True

findSubExpr :: String -> Expr -> Expr -> Bool -> Maybe (Maybe Term)
findSubExpr x (a :->: b) (a' :->: b') f     = checkSub x a b a' b' f findSubExpr
findSubExpr x (a :|: b) (a' :|: b') f       = checkSub x a b a' b' f findSubExpr
findSubExpr x (a :&: b) (a' :&: b') f       = checkSub x a b a' b' f findSubExpr
findSubExpr x (a :=: b) (a' :=: b') f       = checkSub x a b a' b' f findSubTerm
findSubExpr x (Neg a) (Neg b) f             = findSubExpr x a b f
findSubExpr x (ForAll a b) (ForAll a' b') f = if a == a' then findSubExpr x b b' (f && (a /= x)) else Nothing
findSubExpr x (Exists a b) (Exists a' b') f = if a == a' then findSubExpr x b b' (f && (a /= x)) else Nothing
findSubExpr x a b _                         = if a == b then Just Nothing else Nothing

findSubTerm :: String -> Term -> Term -> Bool -> Maybe (Maybe Term)
findSubTerm x (a :+: b) (a' :+: b') f             = checkSub x a b a' b' f findSubTerm
findSubTerm x (a :*: b) (a' :*: b') f             = checkSub x a b a' b' f findSubTerm
findSubTerm x (Succ a) (Succ b) f                 = findSubTerm x a b f
findSubTerm x Zero Zero _                         = Just Nothing
findSubTerm x a b f
    | a == (Var x) && b == (Var x) && not f    = Just Nothing
    | a == b && a /= (Var x)                   = Just Nothing
    | a == (Var x) && f                        = Just (Just b)
    | otherwise                                = Nothing

getFreeVar :: Term -> Set.Set Term
getFreeVar Zero      = Set.empty
getFreeVar (Var a)   = Set.singleton (Var a)
getFreeVar (Succ a)  = getFreeVar a
getFreeVar (a :+: b) = Set.union (getFreeVar a) (getFreeVar b)
getFreeVar (a :*: b) = Set.union (getFreeVar a) (getFreeVar b)

sub :: String -> Expr -> Expr -> Set.Set Term -> Bool
sub x expr1 expr2 impls = subExpr x expr1 expr2 Set.empty impls

subExpr :: String -> Expr -> Expr -> Set.Set Term -> Set.Set Term-> Bool
subExpr x (PreVar a) (PreVar b) bind impls       = True
subExpr x (Neg a) (Neg b) bind impls             = subExpr x a b bind impls
subExpr x (ForAll a b) (ForAll a' b') bind impls = subExpr x b b' (Set.insert (Var a) bind) impls
subExpr x (Exists a b) (Exists a' b') bind impls = subExpr x b b' (Set.insert (Var a) bind) impls
subExpr x (a :=: b) (a' :=: b') bind impls       = subTerm x a a' bind impls && subTerm x b b' bind impls
subExpr x (a :->: b) (a' :->: b') bind impls     = subExpr x a a' bind impls && subExpr x b b' bind impls
subExpr x (a :|: b) (a' :|: b') bind impls       = subExpr x a a' bind impls && subExpr x b b' bind impls
subExpr x (a :&: b) (a' :&: b') bind impls       = subExpr x a a' bind impls && subExpr x b b' bind impls

subTerm :: String -> Term -> Term -> Set.Set Term -> Set.Set Term -> Bool
subTerm _ Zero Zero _ _                  = True
subTerm x (Succ a) (Succ b) bind impls     = subTerm x a b bind impls
subTerm x (a :+: b) (a' :+: b') bind impls = subTerm x a a' bind impls && subTerm x b b' bind impls
subTerm x (a :*: b) (a' :*: b') bind impls = subTerm x a a' bind impls && subTerm x b b' bind impls
subTerm x a b bind impls           = if (Var x) == a && (Var x) /= b then (Set.member (Var x) bind) || (Set.null (Set.intersection bind impls)) else True


checkFree :: String -> Expr -> Bool
checkFree x (PreVar a)   = False
checkFree x (Neg a)      = checkFree x a
checkFree x (ForAll a b) = if a == x then False else checkFree x b
checkFree x (Exists a b) = if a == x then False else checkFree x b
checkFree x (a :=: b)    = checkFree' x a || checkFree' x b
checkFree x (a :->: b)   = checkFree x a || checkFree x b
checkFree x (a :&: b)    = checkFree x a || checkFree x b
checkFree x (a :|: b)    = checkFree x a || checkFree x b

checkFree' :: String -> Term -> Bool
checkFree' x Zero      = False
checkFree' x (Var y)   = x == y
checkFree' x (Succ a)  = checkFree' x a
checkFree' x (a :+: b) = checkFree' x a || checkFree' x b
checkFree' x (a :*: b) = checkFree' x a || checkFree' x b

modusPonens :: Expr ->  Map.Map Expr Int -> Map.Map Expr [(Expr, Int)] -> Maybe Annotate
modusPonens expr indexs modusMap =
    case Map.lookup expr modusMap of
        Just list ->
            case sortBy (\(a, k) (b, n) -> flip compare (indexs Map.! a, k) (indexs Map.! b, n)) $ filter (isJust . (`Map.lookup` indexs) . fst) list of
                [] -> Nothing
                ((x, v) : _ ) -> Just (ModusPonens (indexs Map.! x) v expr)
        Nothing   -> Nothing

checkRule :: Expr -> Map.Map Expr Int -> Map.Map Expr [(Expr, Int)] -> Annotate
checkRule expr indexs impls =
    case axiom expr of
        0 -> case axiomSch expr of
            (Right num) -> AxiomSch (show num) expr
            (Left err)        ->
                case axiom9 expr of
                    True -> AxiomSch "A9" expr
                    _    ->
                        case modusPonens expr indexs impls of
                            Just rule -> rule
                            _         ->
                                case intro expr indexs of
                                    Error err1 -> if err1 == "is not proved" then Error err else Error err1
                                    rule'      -> rule'
        n -> Axiom n expr

toListString :: [Annotate] -> Int -> [String]
toListString [] index = [""]
toListString (x:xs) index =
    case x of
        Axiom num expr       -> ("[" ++ show (index) ++ ". Ax. A" ++ show (num) ++ "] " ++ show(expr)) : (toListString xs (index + 1))
        AxiomSch num expr    -> ("[" ++ show (index) ++ ". Ax. sch. " ++ num ++ "] " ++ show(expr)) : (toListString xs (index + 1))
        ModusPonens l k expr -> ("[" ++ show (index) ++ ". M.P. " ++ show (l + 1) ++ ", " ++ show (k + 1) ++ "] " ++ show(expr)) : (toListString xs (index + 1))
        Intro k symbol expr  -> ("[" ++ show (index) ++ ". " ++ symbol ++ "-intro " ++ show (k + 1) ++ "] " ++ show(expr)) : (toListString xs (index + 1))
        Error err            -> if err /= "is not proved" then ["Expression " ++ show index ++ ": " ++ err ++ "."]
                                                else ["Expression " ++ show index ++ " " ++ err ++ "."]

iter :: [Expr] -> Int ->  Map.Map Expr Int -> Map.Map Expr [(Expr, Int)] -> [Annotate]
iter [] _ _ _ = []
iter (expr : exprs) index indexs impls =
    case checkRule expr indexs impls of
        Error err   -> [Error err]
        rule        -> (rule) : (iter exprs (index + 1) (Map.insert expr index indexs) (add expr impls index))
        
add :: Expr -> Map.Map Expr [(Expr, Int)] -> Int -> Map.Map Expr [(Expr, Int)]
add (v :->: k) impls ind =
    case Map.lookup k impls of
        Just vs -> Map.insert k ((v, ind) : vs) impls
        Nothing -> Map.insert k [(v, ind)] impls
add _ impls _ = impls

printList :: [String] -> String
printList [x]    = x
printList (x:xs) = x ++ "\n" ++ (printList xs)

getLast :: [a] -> a
getLast [x] = x
getLast (x:xs) = getLast xs