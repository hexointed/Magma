{-# LANGUAGE FlexibleInstances #-}

module Magma.Optim (optimize, propagate) where 

import Data.Maybe
import Magma.Base
import Magma.Signal
import Magma.Explicit

highS, lowS :: Signalable a => S a Int
highS = D high []
lowS  = D low []

highS' = ((== highS) . snd)
lowS'  = ((== lowS) . snd)

optimize :: Signal Bool -> IO (Signal Bool)
optimize = undefined

propagate :: Signal Bool -> IO (Signal Bool)
propagate s = do
	s' <- toExplicit s
	return $ toSignal $ valProp s'

valProp :: [(Int, S Bool Int)] -> [(Int, S Bool Int)]
valProp m = valProp' m [] [1]

valProp' :: [(Int, S Bool Int)] -> [Int] -> [Int] -> [(Int, S Bool Int)]
valProp' m vs []     = m
valProp' m vs (n:ns)
	| n `elem` vs = valProp' m vs ns
	| otherwise   = valProp'' m' n
 		where 
			m' = valProp' m (n:vs) (ns' ++ ns)
			ns' = case lookup n m of
				Just (S g sigs) -> sigs
				Just (V v sigs) -> []
				Just (D a sigs) -> []

valProp'' :: [(Int, S Bool Int)] -> Int -> [(Int, S Bool Int)]
valProp'' map n = replaceWith (\(x,_) -> x==n) (n, this') map
	where
		this' = valProp''' this sigs'
		sigs' = zip sigs $ lookupM sigs map
		(this, sigs) = case lookup n map of
			Just s@(S g sigs) -> (s, sigs)
			Just s@(V v sigs) -> (s, sigs)
			Just s@(D a sigs) -> (s, sigs)

valProp''' :: S Bool Int -> Explicit Bool -> S Bool Int
valProp''' h@(V v _) xs = h
valProp''' h@(D d _) xs = h

valProp''' (S Not _) [(i, D d [])]
	| d == low  = highS
	| otherwise = lowS
	
valProp''' (S And _) xs
	| any lowS' xs = lowS
	| otherwise    = case filter highS' xs of
		[]  -> highS
		[y] -> snd y
		ys  -> S And $ map fst ys

valProp''' (S Nand _) xs
	| any lowS' xs = highS
	| otherwise    = case filter highS' xs of
		[]  -> lowS
		[y] -> S Not [fst y]
		ys  -> S Nand $ map fst ys

valProp''' (S Or _) xs
	| any highS' xs = highS
	| otherwise     = case filter lowS' xs of
		[]  -> lowS
		[y] -> snd y
		ys  -> S Or $ map fst ys

valProp''' (S Nor _) xs
	| any highS' xs = lowS
	| otherwise     = case filter lowS' xs of
		[]  -> highS
		[y] -> S Not [fst y]
		ys  -> S Nor $ map fst ys

valProp''' (S Xor _) xs
	| odd $ length ts = case ys of
		[]  -> highS
		[y] -> S Not [fst y]
		ys  -> S Xnor $ map fst ys
	| otherwise       = case ys of
		[]  -> lowS
		[y] -> snd y
		ys  -> S Xor $ map fst ys
		where 
			ts = filter highS' xs
			ys = filter (not . highS') $ filter (not . lowS') xs

valProp''' (S Xnor _) xs
	| odd $ length ts = case ys of
		[]  -> lowS
		[y] -> snd y
		ys  -> S Xor $ map fst ys
	| otherwise       = case ys of
		[]  -> highS
		[y] -> S Not [fst y]
		ys  -> S Xnor $ map fst ys
		where
			ts = filter highS' xs
			ys = filter (not . highS') $ filter (not . lowS') xs

valProp''' h@(S g _) xs = h
