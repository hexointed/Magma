{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Magma.Explicit where

import Data.Reify
import Data.Maybe
import Data.List
import Control.Applicative 
import Data.Traversable
import Magma.Signal

data S a signal 
	= S Gate [signal]
	| D a [signal]
	| V Variable [signal]
	| F Func [signal]

type Sig a = S a Int
type SRef a = (Int, Sig a)
type Explicit a = [SRef a]

instance MuRef (Signal a) where
	type DeRef (Signal a) = S a
	mapDeRef f (Val s)    = D s <$> traverse f ([] :: [Signal a])
	mapDeRef f (Var s xs) = V s <$> traverse f xs
	mapDeRef f (Sig g s)  = S g <$> traverse f s
	mapDeRef f (Fn g xs)  = F g <$> traverse f xs

instance (Show a, Show signal) => Show (S a signal) where
	show (D sig  sigs) = show sig
	show (V sig  sigs) = show sig ++ show sigs
	show (S gate sigs) = "S <" ++ show gate ++ "> " ++ show sigs
	show (F func sigs) = "F <" ++ show func ++ "> " ++ show sigs

instance (Eq a, Ord signal) => Eq (S a signal) where
	(==) (S g ss) (S h ts) = g == h && sort ss == sort ts
	(==) (D a ss) (D b ts) = a == b && sort ss == sort ts
	(==) (V v ss) (V w ts) = v == w && sort ss == sort ts
	(==) (F f ss) (F g ts) = f == g && ss == ts
	(==) _        _        = False

toExplicit :: Signal a -> IO (Explicit a)
toExplicit s = do
	Graph xs i <- reifyGraph s
	return xs

toSignal :: Explicit a -> Signal a
toSignal links = head $ map snd nodeLookupList 
	where
		mkNode (lbl, F f adj) = (lbl, Fn f  $ map lookupNode adj)
		mkNode (lbl, S g adj) = (lbl, Sig g $ map lookupNode adj)
		mkNode (lbl, V v adj) = (lbl, Var v $ map lookupNode adj)
		mkNode (lbl, D d adj) = (lbl, Val d)
		nodeLookupList = map mkNode links
		lookupNode lbl = fromJust $ lookup lbl nodeLookupList

deps :: S a signal -> [signal]
deps (F f s) = s
deps (S g s) = s
deps (D a s) = s
deps (V v s) = s

gateEq :: Gate -> S a signal -> Bool
gateEq g' (S g s) = g == g'
gateEq _  _       = False

fanout :: Int -> Explicit a -> Int
fanout i = length . filter id . map (any (==i) . deps . snd)

replaceDeps :: S a signal -> [signal] -> S a signal
replaceDeps (F f _) xs = F f xs
replaceDeps (S g _) xs = S g xs
replaceDeps (D a _) xs = D a xs
replaceDeps (V v _) xs = V v xs
