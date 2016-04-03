module Magma.Pattern where

import Data.Maybe
import Magma.Base
import Magma.Signal
import Magma.Explicit
import Magma.Signalable

matches :: Signalable a => (Int, Explicit a) -> Signal a -> Bool
matches (n,b) a = match' [(n,a)] b []

match' :: Signalable a => 
	[(Int, Signal a)] -> Explicit a -> [(String, Int)] -> Bool
match' ((n, Sig g xs):ns) inp m = case lookup' n inp of
	S g' ys -> 
		g == g' && 
		length xs == length ys &&
		match' (ns ++ (zip ys xs)) inp m
	_       -> False
match' ((n, Var (Single v) xs):ns) inp m = case lookup v m of
	Just a  -> a == n && match' ns inp m
	Nothing -> match' ns inp ((v,n):m)
match' ((n, Val v):ns) inp m = case lookup' n inp of
	D d xs ->
		length xs == 0 &&
		d == v &&
		match' ns inp m
	_      -> False
match' [] inp m = True
match' s _ _ = error $ show s

pat :: String -> Signal a
pat s = Var (Single s) []
