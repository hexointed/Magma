{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Magma.Signal where

import Magma.Signalable
import Magma.Target
import Magma.Base

data Gate
	= Not
	| And
	| Nand
	| Or
	| Nor
	| Xor
	| Xnor
	deriving (Show, Eq)

data Func
	= Mux
	deriving (Show, Eq)

mux :: Signal a -> (Signal a, Signal a) -> Signal a
mux a (b,c) = Fn Mux [a, b, c]

data Signal a
	= Var Variable [Signal a]
	| Val a
	| Sig Gate [Signal a]
	| Fn Func [Signal a]
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
	show (Fn f sigs) = show f ++ show sigs

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
	
data Variable
	= Single String
	| Multiple Int String

instance Ord Variable where
	compare (Single a0) (Single a1) = compare a0 a1
	compare (Multiple i0 a0) (Multiple i1 a1) 
		| compare a0 a1 == EQ = compare i0 i1
		| otherwise           = compare a0 a1
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
