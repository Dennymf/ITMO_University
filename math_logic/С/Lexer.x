{
module Lexer where
}

%wrapper "basic"

$upper = A-Z            -- alphabetic characters
$lower = a-z            -- lower alphabetic characters
tokens :-

  $white+                       ;
  $lower+                       { \s -> TVar s  }
  $upper+                       { \s -> TPred s }
  "|-"                          { \s -> TTurn   }
  "->"                          { \s -> TImpl   }
  \|                            { \s -> TOr     }
  \&                            { \s -> TAnd    }
  \!                            { \s -> TNot    }
  \(                            { \s -> TOpBr   }
  \)                            { \s -> TClBr   }
  \@                            { \s -> TForAll }
  \?                            { \s -> TExists }
  \.                            { \s -> TDot    }
  \=                            { \s -> TEq     }
  \+                            { \s -> TPlus   }
  \*                            { \s -> TMulti  }
  0                             { \s -> TZero   }
  \'                            { \s -> TSucc   }
{
data Token
    = TVar String
    | TPred String
    | TTurn
    | TImpl
    | TOr
    | TAnd
    | TNot
    | TOpBr
    | TClBr
    | TForAll
    | TExists
    | TDot
    | TEq
    | TPlus
    | TMulti
    | TZero
    | TSucc
    deriving (Eq,Show)

scanTokens = alexScanTokens
}