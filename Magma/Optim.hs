{-# LANGUAGE MultiWayIf #-}

module Magma.Optim  where 

import Data.List
import Data.Maybe
import Magma.Base
import Magma.Signal
import Magma.Explicit
import Magma.Signalable

type Optimizer a = Explicit a -> Int -> Explicit a

allOptims :: Signalable a => [Optimizer a]
allOptims = [valuePropagate, gateCombine, notElim]

highS, lowS :: Signalable a => Sig a
highS = D high []
lowS  = D low []

highS', lowS' :: Signalable a => SRef a -> Bool
highS' = ((== highS) . snd)
lowS'  = ((== lowS) . snd)

optimize :: (Signalable a, Show a) => [Optimizer a] -> Signal a -> IO (Signal a)
optimize opts sig = do
	graph <- toExplicit sig
	return $ toSignal $ runOptimizer opts graph

runOptimizer :: Signalable a => [Optimizer a] -> Explicit a -> Explicit a
runOptimizer opts m = runOptimizer' opts m [] [1]

runOptimizer' :: Signalable a => 
	[Optimizer a] -> Explicit a -> [Int] -> [Int] -> Explicit a
runOptimizer' opts m vs []     = m
runOptimizer' opts m vs (n:ns)
	| n `elem` vs = runOptimizer' opts m vs ns
	| otherwise   = foldr ($) m' (map (\o -> (\g -> o g n)) opts)   --opts m' n
 		where 
			m' = runOptimizer' opts m (n:vs) (ns' ++ ns)
			ns' = case lookup n m of
				Just (S g sigs) -> sigs
				Just (V v sigs) -> sigs
				Just (D a sigs) -> sigs

gateCombine :: Signalable a => Explicit a -> Int -> Explicit a
gateCombine m n = (\r -> replaceWith (\(x,_) -> x==n) r m) $
	case lookup' n m of
		V v    vs  -> (n, V v vs)
		D d    ds  -> (n, D d ds)
		S And xs   -> (n, S And  (zs xs And))
		S Nand xs  -> (n, S Nand (zs xs And))
		S Or xs    -> (n, S Or   (zs xs Or))
		S Nor xs   -> (n, S Nor  (zs xs Or))
		S Xor xs   -> (n, S Xor  (zs xs Xor))
		S Xnor xs  -> (n, S Xnor (zs xs Xnor))
		s          -> (n, s)
		where
			ys xs g = filter (\n -> (gateEq g) $ lookup' n m) xs
			ws xs g = filter (\n -> (not . gateEq g) $ lookup' n m) xs
			zs xs g = concat (map getDeps $ lookupM (ys xs g) m) ++ ws xs g

notElim :: Signalable a => Explicit a -> Int -> Explicit a
notElim m n = (\r -> replaceWith (\(x,_) -> x==n) r m) $
	case lookup' n m of
		V v    vs  -> (n, V v vs)
		D d    ds  -> (n, D d ds)
		
		S Not  [x] -> case lookup' x m of
			S Not [y] -> (n, lookup' y m)
			S And ys  -> (n, S Nand ys)
			S Nand ys -> (n, S And ys)
			S Or ys   -> (n, S Nor ys)
			S Nor ys  -> (n, S Or ys)
			S Xor ys  -> (n, S Xnor ys)
			S Xnor ys -> (n, S Xor ys)
			_         -> (n, S Not [x])

		S And xs   -> let ys = lookupM xs m in if
			| all (gateEq Not) ys -> (n, S Nand $ concat $ map getDeps ys)
			| otherwise           -> (n, S And xs)
		
		S Or  xs   -> let ys = lookupM xs m in if
			| all (gateEq Not) ys -> (n, S Nand $ concat $ map getDeps ys)
			| otherwise           -> (n, S Or xs)

		S Xor xs   -> let ys = lookupM xs m in if
			| all (gateEq Not) ys -> (n, S Xor $ concat $ map getDeps ys)
			| otherwise           -> (n, S Xor xs)

		S Xnor xs  -> let ys = lookupM xs m in if
			| all (gateEq Not) ys -> (n, S Xnor $ concat $ map getDeps ys)
			| otherwise           -> (n, S Xnor xs)
		
		s          -> (n, s) 

valuePropagate :: Signalable a => Explicit a -> Int -> Explicit a
valuePropagate map n = replaceWith (\(x,_) -> x==n) (n, this') map
	where
		this' = valuePropagate' this sigs'
		sigs' = zip sigs $ lookupM sigs map
		(this, sigs) = case lookup n map of
			Just s@(S g sigs) -> (s, sigs)
			Just s@(V v sigs) -> (s, sigs)
			Just s@(D a sigs) -> (s, sigs)

valuePropagate' :: Signalable a => Sig a -> Explicit a -> Sig a
valuePropagate' h@(V v _) xs = h
valuePropagate' h@(D d _) xs = h

valuePropagate' (S Not _) [(i, D d [])]
	| d == low  = highS
	| otherwise = lowS
	
valuePropagate' (S And _) xs
	| any lowS' xs = lowS
	| otherwise    = case filter (not . highS') xs of
		[]  -> highS
		[y] -> snd y
		ys  -> S And $ map fst ys

valuePropagate' (S Nand _) xs
	| any lowS' xs = highS
	| otherwise    = case filter (not . highS') xs of
		[]  -> lowS
		[y] -> S Not [fst y]
		ys  -> S Nand $ map fst ys

valuePropagate' (S Or _) xs
	| any highS' xs = highS
	| otherwise     = case filter (not . lowS') xs of
		[]  -> lowS
		[y] -> snd y
		ys  -> S Or $ map fst ys

valuePropagate' (S Nor _) xs
	| any highS' xs = lowS
	| otherwise     = case filter (not . lowS') xs of
		[]  -> highS
		[y] -> S Not [fst y]
		ys  -> S Nor $ map fst ys

valuePropagate' (S Xor _) xs
	| odd $ length ts = case ys of
		[]  -> highS
		[y] -> S Not [fst y]
		ys  -> S Xnor $ map fst ys
	| otherwise       = case ys of
		[]  -> lowS
		[y] -> snd y
		ys  -> S Xor $ map fst ys
		where 
			ts = filter (not . highS') xs
			ys = filter (not . highS') $ filter (not . lowS') xs

valuePropagate' (S Xnor _) xs
	| odd $ length ts = case ys of
		[]  -> lowS
		[y] -> snd y
		ys  -> S Xor $ map fst ys
	| otherwise       = case ys of
		[]  -> highS
		[y] -> S Not [fst y]
		ys  -> S Xnor $ map fst ys
		where
			ts = filter (not . highS') xs
			ys = filter (not . highS') $ filter (not . lowS') xs

valuePropagate' h@(S g _) xs = h
