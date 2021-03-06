{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12 t13
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,122) ([24576,4312,16384,0,8192,0,3072,539,0,0,0,0,0,0,0,0,0,0,0,0,32768,1,0,1,0,2,0,0,0,49152,8624,24576,4312,4096,0,2048,0,0,0,0,16,0,8,32768,0,16384,12,0,0,0,0,1032,4,516,2,258,1,34499,32768,17249,49152,8624,0,0,0,0,0,0,0,0,0,128,33024,128,0,16,0,64,0,0,0,0,13848,4,6924,2,0,0,0,0,4224,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","%start_parseStatement","Statement","Expr","Disj","Conj","Unary","Pred","Term","Add","Mul","var","pre","\"|-\"","\"->\"","'|'","'&'","'!'","'('","')'","'@'","'?'","'.'","'='","'+'","'*'","'0'","'\\''","%eof"]
        bit_start = st Prelude.* 31
        bit_end = (st Prelude.+ 1) Prelude.* 31
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..30]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (14) = happyShift action_13
action_0 (15) = happyShift action_14
action_0 (20) = happyShift action_15
action_0 (21) = happyShift action_16
action_0 (23) = happyShift action_17
action_0 (24) = happyShift action_18
action_0 (29) = happyShift action_19
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (9) = happyGoto action_8
action_0 (10) = happyGoto action_9
action_0 (11) = happyGoto action_10
action_0 (12) = happyGoto action_11
action_0 (13) = happyGoto action_12
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (16) = happyShift action_3
action_1 (5) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (16) = happyShift action_3
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (14) = happyShift action_13
action_3 (15) = happyShift action_14
action_3 (20) = happyShift action_15
action_3 (21) = happyShift action_16
action_3 (23) = happyShift action_17
action_3 (24) = happyShift action_18
action_3 (29) = happyShift action_19
action_3 (6) = happyGoto action_32
action_3 (7) = happyGoto action_6
action_3 (8) = happyGoto action_7
action_3 (9) = happyGoto action_8
action_3 (10) = happyGoto action_9
action_3 (11) = happyGoto action_10
action_3 (12) = happyGoto action_11
action_3 (13) = happyGoto action_12
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (31) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (31) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (17) = happyShift action_30
action_6 (18) = happyShift action_31
action_6 _ = happyReduce_3

action_7 (19) = happyShift action_29
action_7 _ = happyReduce_5

action_8 _ = happyReduce_7

action_9 _ = happyReduce_9

action_10 (26) = happyShift action_27
action_10 (27) = happyShift action_28
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (28) = happyShift action_26
action_11 _ = happyReduce_16

action_12 (30) = happyShift action_25
action_12 _ = happyReduce_18

action_13 _ = happyReduce_20

action_14 _ = happyReduce_14

action_15 (14) = happyShift action_13
action_15 (15) = happyShift action_14
action_15 (20) = happyShift action_15
action_15 (21) = happyShift action_16
action_15 (23) = happyShift action_17
action_15 (24) = happyShift action_18
action_15 (29) = happyShift action_19
action_15 (9) = happyGoto action_24
action_15 (10) = happyGoto action_9
action_15 (11) = happyGoto action_10
action_15 (12) = happyGoto action_11
action_15 (13) = happyGoto action_12
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (14) = happyShift action_13
action_16 (15) = happyShift action_14
action_16 (20) = happyShift action_15
action_16 (21) = happyShift action_16
action_16 (23) = happyShift action_17
action_16 (24) = happyShift action_18
action_16 (29) = happyShift action_19
action_16 (6) = happyGoto action_22
action_16 (7) = happyGoto action_6
action_16 (8) = happyGoto action_7
action_16 (9) = happyGoto action_8
action_16 (10) = happyGoto action_9
action_16 (11) = happyGoto action_23
action_16 (12) = happyGoto action_11
action_16 (13) = happyGoto action_12
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (14) = happyShift action_21
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (14) = happyShift action_20
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_22

action_20 (25) = happyShift action_43
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (25) = happyShift action_42
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (22) = happyShift action_41
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (22) = happyShift action_40
action_23 (26) = happyShift action_27
action_23 (27) = happyShift action_28
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_10

action_25 _ = happyReduce_23

action_26 (14) = happyShift action_13
action_26 (21) = happyShift action_37
action_26 (29) = happyShift action_19
action_26 (13) = happyGoto action_39
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (14) = happyShift action_13
action_27 (21) = happyShift action_37
action_27 (29) = happyShift action_19
action_27 (11) = happyGoto action_38
action_27 (12) = happyGoto action_11
action_27 (13) = happyGoto action_12
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (14) = happyShift action_13
action_28 (21) = happyShift action_37
action_28 (29) = happyShift action_19
action_28 (12) = happyGoto action_36
action_28 (13) = happyGoto action_12
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (14) = happyShift action_13
action_29 (15) = happyShift action_14
action_29 (20) = happyShift action_15
action_29 (21) = happyShift action_16
action_29 (23) = happyShift action_17
action_29 (24) = happyShift action_18
action_29 (29) = happyShift action_19
action_29 (9) = happyGoto action_35
action_29 (10) = happyGoto action_9
action_29 (11) = happyGoto action_10
action_29 (12) = happyGoto action_11
action_29 (13) = happyGoto action_12
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (14) = happyShift action_13
action_30 (15) = happyShift action_14
action_30 (20) = happyShift action_15
action_30 (21) = happyShift action_16
action_30 (23) = happyShift action_17
action_30 (24) = happyShift action_18
action_30 (29) = happyShift action_19
action_30 (6) = happyGoto action_34
action_30 (7) = happyGoto action_6
action_30 (8) = happyGoto action_7
action_30 (9) = happyGoto action_8
action_30 (10) = happyGoto action_9
action_30 (11) = happyGoto action_10
action_30 (12) = happyGoto action_11
action_30 (13) = happyGoto action_12
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (14) = happyShift action_13
action_31 (15) = happyShift action_14
action_31 (20) = happyShift action_15
action_31 (21) = happyShift action_16
action_31 (23) = happyShift action_17
action_31 (24) = happyShift action_18
action_31 (29) = happyShift action_19
action_31 (8) = happyGoto action_33
action_31 (9) = happyGoto action_8
action_31 (10) = happyGoto action_9
action_31 (11) = happyGoto action_10
action_31 (12) = happyGoto action_11
action_31 (13) = happyGoto action_12
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_2

action_33 (19) = happyShift action_29
action_33 _ = happyReduce_6

action_34 _ = happyReduce_4

action_35 _ = happyReduce_8

action_36 (28) = happyShift action_26
action_36 _ = happyReduce_17

action_37 (14) = happyShift action_13
action_37 (21) = happyShift action_37
action_37 (29) = happyShift action_19
action_37 (11) = happyGoto action_46
action_37 (12) = happyGoto action_11
action_37 (13) = happyGoto action_12
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (27) = happyShift action_28
action_38 _ = happyReduce_15

action_39 (30) = happyShift action_25
action_39 _ = happyReduce_19

action_40 _ = happyReduce_21

action_41 _ = happyReduce_11

action_42 (14) = happyShift action_13
action_42 (15) = happyShift action_14
action_42 (20) = happyShift action_15
action_42 (21) = happyShift action_16
action_42 (23) = happyShift action_17
action_42 (24) = happyShift action_18
action_42 (29) = happyShift action_19
action_42 (6) = happyGoto action_45
action_42 (7) = happyGoto action_6
action_42 (8) = happyGoto action_7
action_42 (9) = happyGoto action_8
action_42 (10) = happyGoto action_9
action_42 (11) = happyGoto action_10
action_42 (12) = happyGoto action_11
action_42 (13) = happyGoto action_12
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (14) = happyShift action_13
action_43 (15) = happyShift action_14
action_43 (20) = happyShift action_15
action_43 (21) = happyShift action_16
action_43 (23) = happyShift action_17
action_43 (24) = happyShift action_18
action_43 (29) = happyShift action_19
action_43 (6) = happyGoto action_44
action_43 (7) = happyGoto action_6
action_43 (8) = happyGoto action_7
action_43 (9) = happyGoto action_8
action_43 (10) = happyGoto action_9
action_43 (11) = happyGoto action_10
action_43 (12) = happyGoto action_11
action_43 (13) = happyGoto action_12
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_13

action_45 _ = happyReduce_12

action_46 (22) = happyShift action_40
action_46 (27) = happyShift action_28
action_46 _ = happyFail (happyExpListPerState 46)

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Statement happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 :->: happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 :|: happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 :&: happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Neg happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 9 happyReduction_12
happyReduction_12 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (ForAll happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 9 happyReduction_13
happyReduction_13 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Exists happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 (HappyTerminal (TPred happy_var_1))
	 =  HappyAbsSyn10
		 (PreVar happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 :=: happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :+: happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 :*: happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn13
		 (Var happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  13 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  13 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn13
		 (Zero
	)

happyReduce_23 = happySpecReduce_2  13 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Succ happy_var_1
	)
happyReduction_23 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 31 31 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TVar happy_dollar_dollar -> cont 14;
	TPred happy_dollar_dollar -> cont 15;
	TTurn -> cont 16;
	TImpl -> cont 17;
	TOr -> cont 18;
	TAnd -> cont 19;
	TNot -> cont 20;
	TOpBr -> cont 21;
	TClBr -> cont 22;
	TForAll -> cont 23;
	TExists -> cont 24;
	TDot -> cont 25;
	TEq -> cont 26;
	TPlus -> cont 27;
	TMulti -> cont 28;
	TZero -> cont 29;
	TSucc -> cont 30;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 31 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

parseStatement tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
