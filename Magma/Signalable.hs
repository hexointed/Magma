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

