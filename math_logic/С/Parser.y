{
module Parser where
import Lexer
}

%name parser Expr
%name parseStatement Statement
%tokentype { Token }
%error { parseError }

%token
    var         { TVar $$   }
    pre         { TPred $$  }
    "|-"        { TTurn     }
    "->"        { TImpl     }
    '|'         { TOr       }
    '&'         { TAnd      }
    '!'         { TNot      }
    '('         { TOpBr     }
    ')'         { TClBr     }
    '@'         { TForAll   }
    '?'         { TExists   }
    '.'         { TDot      }
    '='         { TEq       }
    '+'         { TPlus     }
    '*'         { TMulti    }
    '0'         { TZero     }
    '\''        { TSucc     }

%%

Statement   : "|-" Expr              { Statement $2   }

Expr  : Disj                         { $1             }
      | Disj "->" Expr               { $1 :->: $3     }

Disj : Conj                          { $1             }
     | Disj '|' Conj                 { $1 :|: $3      }

Conj : Unary                         { $1             }
     | Conj '&' Unary                { $1 :&: $3      }

Unary : Pred                         { $1             }
      | '!' Unary                    { Neg $2         }
      | '(' Expr ')'                 { $2             }
      | '@' var '.' Expr             { ForAll $2 $4   }
      | '?' var '.' Expr             { Exists $2 $4   }

Pred : pre                           { PreVar $1 }
     | Term '=' Term                 { $1 :=: $3      }

Term : Add                           { $1             }
     | Term '+' Add                  { $1 :+: $3      }

Add : Mul                            { $1             }
    | Add '*' Mul                    { $1 :*: $3      }

Mul : var                            { Var $1         }
    | '(' Term ')'                   { $2             }
    | '0'                            { Zero           }
    | Mul '\''                       { Succ $1        }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Statement = Statement { getResult :: Expr } deriving Show

data Term
    = Term :+: Term
    | Term :*: Term
    | Var String
    | Succ Term
    | Zero
    deriving (Ord, Eq)

instance Show Term where
    show (a :+: b)      = "(" ++ (show a) ++ "+" ++ (show b) ++ ")"
    show (a :*: b)      = "(" ++ (show a) ++ "*" ++ (show b) ++ ")"
    show (Var a)        = a
    show (Zero)         = "0"
    show (Succ a)       = (show a) ++ "'"

data Expr
    = Expr :->: Expr
    | Expr :|: Expr
    | Expr :&: Expr
    | Neg Expr
    | ForAll String Expr
    | Exists String Expr
    | PreVar String
    | Term :=: Term
    deriving (Ord, Eq)

instance Show Expr where
    show (a :->: b)             = "(" ++ (show a) ++ "->" ++ (show b) ++ ")"
    show (a :|: b)              = "(" ++ (show a) ++ "|" ++ (show b) ++ ")"
    show (a :&: b)              = "(" ++ (show a) ++ "&" ++ (show b) ++ ")"
    show (Neg a)                = "(!" ++ (show a) ++ ")"
    show (ForAll v a)           = "(@" ++ v ++ "." ++ (show a) ++ ")"
    show (Exists v a)           = "(?" ++ v ++ "." ++ (show a) ++ ")"
    show (PreVar a)             = a
    show (a :=: b)              = "(" ++ (show a) ++ "=" ++ (show b) ++ ")"
}