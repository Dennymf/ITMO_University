{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    $white+                     ;
    \&                          { \s -> TAnd   }
	\|                          { \s -> TOr    }
    "|-"                        { \s -> TTurn  }
	"->"                        { \s -> TArrow }
	\!                          { \s -> TNot   }
	\,                          { \s -> TCommo }
	\(                          { \s -> TOpBr  }
	\)                          { \s -> TClBr  }
    $alpha [$alpha $digit \']*  { \s -> TVar s }

{
data Token
    = TAnd
    | TOr
    | TTurn
    | TArrow
    | TNot
    | TCommo
    | TOpBr
    | TClBr
    | TVar String
    deriving (Eq, Show)

scanTokens = alexScanTokens
}