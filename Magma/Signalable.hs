module Magma.Signalable where

import Magma.Base

class Eq a => Signalable a where
	high  :: a
	low   :: a
	nots  :: a -> a
	ands  :: [a] -> a
	nands :: [a] -> a
	nands = nots . ands
	ors   :: [a] -> a
	nors  :: [a] -> a
	nors  = nots . ors
	xors  :: [a] -> a
	xnors :: [a] -> a
	xnors = nots . xors

instance Signalable Bool where
	high = True
	low  = False
	nots = not
	ands = and
	ors  = or
	xors as
		| odd . length $ filter (==True) as = True
		| otherwise                         = False

not2 :: Signalable a => a -> a
not2 = nots

and2  a b = ands  [a, b]
nand2 a b = nands [a, b]
or2   a b = ors   [a, b]
nor2  a b = nors  [a, b]
xor2  a b = xors  [a, b]
xnor2 a b = xnors [a, b]

