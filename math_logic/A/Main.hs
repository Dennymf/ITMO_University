module Main where
import Lexer
import Parser
		  
instance Show Expr where
  show (l :&: r) = "(&," ++ show l ++ "," ++ show r ++ ")"
  show (l :|: r) = "(|," ++ show l ++ "," ++ show r ++ ")"
  show (l :->: r) = "(->," ++ show l ++ "," ++ show r ++ ")"
  show (Neg e) = "(!" ++ show e ++ ")"
  show (Var v) = v

main :: IO ()
main = interact (show . parseExpr . alexScanTokens)