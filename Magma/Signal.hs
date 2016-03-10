{-# LANGUAGE FlexibleInstances #-}

module Magma.Signal where

import Magma.Base

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
	= Var String
	| Val a
	| Sig Gate [Signal a]
	deriving Eq

instance Signalable a => Show (Signal a) where
	show (Var s) = s
	show (Val v)
		| v == high = "high"
		| v == low  = "low"
		| otherwise = error "unknown"
	show (Sig g sigs) = show g ++ show sigs

class Eq a => Signalable a where
	high  :: a
	low   :: a

instance Signalable Bool where
	high = True
	low  = False

instance Signalable a => Signalable (Signal a) where
	high = Val high
	low  = Val low

not2 :: Signal Bool -> Signal Bool
not2 a    = Sig Not [a]

and2, nand2, or2, nor2, xor2, xnor2 :: Signal Bool -> Signal Bool -> Signal Bool
and2  a b = Sig And  [a, b]
nand2 a b = Sig Nand [a, b]
or2   a b = Sig Or   [a, b]
nor2  a b = Sig Nor  [a, b]
xor2  a b = Sig Xor  [a, b]
xnor2 a b = Sig Xnor [a, b]

andl  xs = Sig And  xs
nandl xs = Sig Nand xs
orl   xs = Sig Or   xs
norl  xs = Sig Nor  xs
xorl  xs = Sig Xor  xs
xnorl xs = Sig Xnor xs
