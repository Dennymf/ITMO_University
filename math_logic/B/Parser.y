{
module Parser where
import Lexer
}

%name parseHead
%tokentype { Token }
%error { parseError }

%token
    '&'     { TAnd   }
    '|'     { TOr    }
    "|-"    { TTurn  }
    "->"    { TArrow }
    '!'     { TNot   }
    ','     { TCommo }
    '('     { TOpBr  }
    ')'     { TClBr  }
    var     { TVar $$}

%right "->"
%left '|'
%left '&'
%left '!'

%%

Head : Exprs "|-" Expr Proof { Head (if $1 == [] then Empty else Exprs $1) $3 $4 }

Exprs :
    Expr ',' Exprs          { $1 : $3    }
    | Expr                  { $1 : []    }
    | {- empty -}           { []         }

Expr :
    Expr "->" Expr          { $1 :->: $3 }
    | Expr '|' Expr         { $1 :|: $3  }
    | Expr '&' Expr         { $1 :&: $3  }
    | '(' Expr ')'          { $2         }
    | '!' Expr              { Neg $2     }
    | var                   { Var $1     }

Proof :
    Expr Proof              { $1 : $2    }
    | {- empty -}           { []         }

{
parseError :: [Token] -> a
parseError _  = error "Parse error"

data Head = Head Exprs Expr [Expr]
    deriving (Eq)
instance Show Head where
    show (Head ctx expr ls) = (show ctx) ++ " |- " ++ (show expr) ++ "\n" ++ (unlines $ map show ls)

data Exprs
    = Exprs [Expr]
    | Empty
    deriving (Show, Eq)

data Expr
    = Expr :&: Expr
    | Expr :|: Expr
    | Expr :->: Expr
    | Neg Expr
    | Var String
    | Bottom
    deriving (Eq, Ord)

instance Show Expr where
    show (e1 :->: e2) = "(" ++ (show e1) ++ " -> " ++ (show e2) ++ ")"
    show (e1 :&: e2) = "(" ++ (show e1) ++ " & " ++ (show e2) ++ ")"
    show (e1 :|: e2) = "(" ++ (show e1) ++ " | " ++ (show e2) ++ ")"
    show (Neg e1) = "(" ++ (show e1) ++ " -> " ++ "_|_)"
    show (Var v) = v
    show Bottom = "_|_"
}