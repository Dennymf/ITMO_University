{
module Parser where
import Lexer
}

%name parseExpr
%tokentype { Token }
%error { parseError }

%token
    '&'     { TAnd   }
    '|'     { TOr    }
    "->"    { TArrow }
    '!'     { TNot   }
    '('     { TOpBr  }
    ')'     { TClBr  }
    var     { TVar $$}

%%

Expr :
    Disj "->" Expr          { $1 :->: $3 }
    | Disj                  { $1         }

Disj :
    Disj '|' Conj           { $1 :|: $3  }
    | Conj                  { $1         }

Conj :
    Conj '&' Unary          { $1 :&: $3  }
    | Unary                 { $1         }

Unary :
    '!' Unary               { Neg $2     }
    | '(' Expr ')'          { $2         }
    | var                   { Var $1     }

{
parseError :: [Token] -> a
parseError _ = error "Parse error: "

data Expr
    = Expr :->: Expr
    | Expr :|: Expr
    | Expr :&: Expr
    | Neg Expr
    | Var String
    deriving (Ord, Eq)

}