module Magma.Signal where

import Magma.Base
import Magma.Signalable

data Gate
	= Not
	| And
	| Nand
	| Or
	| Nor
	| Xor
	| Xnor
	deriving (Show, Eq, Enum)

data Signal a
	= Var Variable [Signal a]
	| Val a
	| Sig Gate [Signal a]
	deriving Eq

instance Signalable a => Show (Signal a) where
	show (Var s xs) = show s ++ case xs of
		[] -> ""
		xs -> ":" ++ show xs
	show (Val v)
		| v == high = "high"
		| v == low  = "low"
		| otherwise = error "unknown"
	show (Sig g sigs) = show g ++ show sigs

instance Signalable a => Signalable (Signal a) where
	high  = Val high
	low   = Val low
	nots  = Sig Not . (:[])
	ands  = Sig And
	nands = Sig Nand
	ors   = Sig Or
	nors  = Sig Nor
	xors  = Sig Xor
	xnors = Sig Xnor
	
not2 :: Signalable a => Signal a -> Signal a
not2 = nots

and2  a b = Sig And  [a, b]
nand2 a b = Sig Nand [a, b]
or2   a b = Sig Or   [a, b]
nor2  a b = Sig Nor  [a, b]
xor2  a b = Sig Xor  [a, b]
xnor2 a b = Sig Xnor [a, b]

data Variable
	= Single String
	| Multiple Int String

instance Ord Variable where
	compare (Single a0) (Single a1) = compare a0 a1
	compare (Multiple _ a0) (Multiple _ a1) = compare a0 a1
	compare (Single _) (Multiple _ _) = LT
	compare (Multiple _ _) (Single _) = GT

instance Eq Variable where
	(==) a b = (==EQ) $ compare a b

instance Show Variable where
	show (Single s) = s
	show (Multiple i s) = s ++ "(" ++ show i ++ ")"

getName :: Variable -> String
getName (Single s)     = s
getName (Multiple n s) = s
