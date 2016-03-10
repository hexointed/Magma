{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Magma.Arith (fullAdder, rc_adder, cla_adder, bitConst) where

import Magma.Signal
import Magma.Base

instance Signalable a => Num [Signal a] where
	(+) a b = sum where (sum, carry) = rc_adder a b low
	(*) = undefined
	abs = bitAbs
	signum as = take 1 as ++ [group2Fold or2 high (tail as)]
	negate = bitNegate
	fromInteger = bitConst'

fullAdder a b c = (sum, carry)
	where 
		sum = xor2 x0 c
		carry = or2 (and2 x0 c) (and2 a b)
		x0 = xor2 a b

rc_adder []     []     carry_in = ([], carry_in)
rc_adder (a:as) (b:bs) carry_in = (sum:sums, carry_out)
	where
		(sum, carry_out) = fullAdder a b carry
		(sums, carry) = rc_adder as bs carry_in

cla_adder a b carry_in = (sum, carry_out)
	where
		(sum, prop, gen) = cla_adder' a b carry_in
		carry_out = or2 gen $ and2 prop carry_in

cla_adder' [a] [b] carry_in = ([sum], prop, gen)
	where
		sum = xor2 prop carry_in
		prop = xor2 a b
		gen = and2 a b
cla_adder' as bs carry_in = (sums, prop, gen)
	where
		(s0, p0, g0) = cla_adder' (drop size as) (drop size bs) carry_in
		(s1, p1, g1) = cla_adder' (take size as) (take size bs) carry
		carry = or2 g0 $ and2 p0 carry_in
		size = div (length as) 2
		prop = and2 p0 p1
		gen = or2 g1 $ and2 p1 g0
		sums = s1 ++ s0

bitAbs [] = []
bitAbs a = sum 
	where 
		(sum, carry) = rc_adder z a' (head a)
		z = map (\x -> low) a
		a' = map (xor2 $ head a) a

bitNegate a = sum
	where
		(sum, carry) = rc_adder z a' high
		z = map (\x -> low) a
		a' = map not2 a

bitConst' :: (Integral a, Signalable b) => a -> [Signal b]
bitConst' as
	| as >= 0   = reverse $ bitConst'' as
	| otherwise = bitNegate $ reverse $ bitConst'' (-as)
	
bitConst length a = filll length (head as) as
	where as = bitConst' a

bitConst'' 0    = [low]
bitConst'' (-1) = [high]
bitConst'' a = l : bitConst'' hs
	where
		l = if a `mod` 2 == 0 then low else high
		hs = div a 2
