module Magma.Optim  where 

import Data.Maybe
import Magma.Base
import Magma.Signal
import Magma.Explicit
import Magma.Signalable

highS, lowS :: Signalable a => Sig a
highS = D high []
lowS  = D low []

highS', lowS' :: Signalable a => SRef a -> Bool
highS' = ((== highS) . snd)
lowS'  = ((== lowS) . snd)

optimize :: Signalable a => Signal a -> IO (Signal a)
optimize = undefined

propagate :: Signalable a => Signal a -> IO (Signal a)
propagate s = do
	s' <- toExplicit s
	return $ toSignal $ valProp s'

valProp :: Signalable a => Explicit a -> Explicit a
valProp m = valProp' m [] [1]

valProp' :: Signalable a => Explicit a -> [Int] -> [Int] -> Explicit a
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

valProp'' :: Signalable a => Explicit a -> Int -> Explicit a
valProp'' map n = replaceWith (\(x,_) -> x==n) (n, this') map
	where
		this' = valProp''' this sigs'
		sigs' = zip sigs $ lookupM sigs map
		(this, sigs) = case lookup n map of
			Just s@(S g sigs) -> (s, sigs)
			Just s@(V v sigs) -> (s, sigs)
			Just s@(D a sigs) -> (s, sigs)

valProp''' :: Signalable a => Sig a -> Explicit a -> Sig a
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
			ys = filter (nots . highS') $ filter (nots . lowS') xs

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
			ys = filter (nots . highS') $ filter (nots . lowS') xs

valProp''' h@(S g _) xs = h
