{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Magma.Explicit where

import Data.Reify
import Data.Maybe
import Control.Applicative 
import Data.Traversable
import Magma.Signal

data S a signal 
	= S Gate [signal]
	| D a [signal]
	| V Variable [signal]
	deriving Eq

type Sig a = S a Int
type SRef a = (Int, Sig a)
type Explicit a = [SRef a]

instance MuRef (Signal a) where
	type DeRef (Signal a) = S a
	mapDeRef f   (Val s)    = D s <$> traverse f ([] :: [Signal a])
	mapDeRef f   (Var s xs) = V s <$> traverse f xs
	mapDeRef f h@(Sig g s)  = S g <$> traverse f s

instance (Show a, Show signal) => Show (S a signal) where
	show (D sig  sigs) = show sig
	show (V sig  sigs) = show sig ++ show sigs
	show (S gate sigs) = "S <" ++ show gate ++ "> " ++ show sigs

toExplicit :: Signal a -> IO (Explicit a)
toExplicit s = do
	Graph xs i <- reifyGraph s
	return xs

toSignal :: Explicit a -> Signal a
toSignal links = head $ map snd nodeLookupList 
	where
		mkNode (lbl, S g adj) = (lbl, Sig g $ map lookupNode adj)
		mkNode (lbl, V v adj) = (lbl, Var v $ map lookupNode adj)
		mkNode (lbl, D d adj) = (lbl, Val d)
		nodeLookupList = map mkNode links
		lookupNode lbl = fromJust $ lookup lbl nodeLookupList

getDeps :: S a signal -> [signal]
getDeps (S g s) = s
getDeps (D a s) = s
getDeps (V v s) = s

gateEq :: Gate -> S a signal -> Bool
gateEq g' (S g s) = g == g'
gateEq _  _       = False
