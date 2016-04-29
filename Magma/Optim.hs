{-# LANGUAGE MultiWayIf #-}

module Magma.Optim  where 

import Data.List
import Data.Maybe
import Magma.Base
import Magma.Signal
import Magma.Pattern
import Magma.Explicit
import Magma.Signalable

type Optimizer a = Explicit a -> Int -> Explicit a

allOptims :: Signalable a => [Optimizer a]
allOptims = [gateCombine, eqElim, eqGate, notElim, compEqElim, valuePropagate]

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

replaceOptimizer m n r = replaceWith (\(x,_) -> x==n) (n, r) m

-- gateCombine reduces multiple nodes with the same operaiton to one node.
gateCombine :: Signalable a => Optimizer a
gateCombine m n = replaceOptimizer m n $
	case lookup' n m of
		S And xs   -> S And  (zs xs And)
		S Nand xs  -> S Nand (zs xs And)
		S Or xs    -> S Or   (zs xs Or)
		S Nor xs   -> S Nor  (zs xs Or)
		S Xor xs   -> S Xor  (zs xs Xor)
		S Xnor xs  -> S Xnor (zs xs Xnor)
		s          -> s
		where
			ys xs g = filter (\n -> (gateEq g) $ lookup' n m) xs
			ws xs g = filter (\n -> (not . gateEq g) $ lookup' n m) xs
			zs xs g = concat (map deps $ lookupM (ys xs g) m) ++ ws xs g

-- notElim removes double negations.
notElim :: Signalable a => Optimizer a
notElim m n = replaceOptimizer m n $
	case lookup' n m of
		S Not  [x] -> case lookup' x m of
			S Not [y] -> lookup' y m
			S And ys  -> S Nand ys
			S Nand ys -> S And ys
			S Or ys   -> S Nor ys
			S Nor ys  -> S Or ys
			S Xor ys  -> S Xnor ys
			S Xnor ys -> S Xor ys
			_         -> S Not [x]

		S And xs   -> let ys = lookupM xs m in if
			| all (gateEq Not) ys -> S Nand $ concat $ map deps ys
			| otherwise           -> S And xs
		
		S Nand xs  -> let ys = lookupM xs m in if
			| all (gateEq Not) ys -> S And  $ concat $ map deps ys
			| otherwise           -> S Nand xs
		
		S Or  xs   -> let ys = lookupM xs m in if
			| all (gateEq Not) ys -> S Nand $ concat $ map deps ys
			| otherwise           -> S Or xs
		
		S Nor xs   -> let ys = lookupM xs m in if
			| all (gateEq Not) ys -> S Or   $ concat $ map deps ys
			| otherwise           -> S Nor xs
		
		S Xor xs   -> let ys = lookupM xs m in if
			| all (gateEq Not) ys -> S Xor  $ concat $ map deps ys
			| otherwise           -> S Xor xs

		S Xnor xs  -> let ys = lookupM xs m in if
			| all (gateEq Not) ys -> S Xnor $ concat $ map deps ys
			| otherwise           -> S Xnor xs
		
		s          -> s

-- eqGate removes redundant inputs from the node.
eqGate :: Signalable a => Optimizer a
eqGate m n = replaceOptimizer m n $
	case lookup' n m of
		S And xs  -> case nub xs of
			[y] -> lookup' y m
			ys  -> S And ys
		
		S Nand xs -> case nub xs of
			[y] -> S Not [y]
			ys  -> S Nand ys
		
		S Or xs   -> case nub xs of
			[y] -> lookup' y m
			ys  -> S Or ys
		
		S Nor xs  -> case nub xs of
			[y] -> S Not [y]
			ys  -> S Nor ys
		
		S Xor xs  -> case undup $ sort xs of
			[]  -> lowS
			[y] -> lookup' y m
			ys  -> S Xor ys
		
		S Xnor xs -> case undup $ sort xs of
			[]  -> highS
			[y] -> S Not [y]
			ys  -> S Xnor ys
			
		s         -> s

-- eqElim removes references to all other nodes that implement the same function
-- as this node (and replaces them with references to this node).
eqElim :: Signalable a => Optimizer a
eqElim m n = let e = lookup' n m in
	map (\(i,s) ->
		(,) i $
		replaceDeps s $
		foldr ($) (deps s) $
		map (\f -> replaceWith f n) $
		map (==) $
		map fst $
		filter (\(i,s) -> s == e && i /= n) m
	) m

-- compEqElim replaces the node with a constant if the value of the function it
-- implements can be predetermined.
compEqElim :: Signalable a => Optimizer a
compEqElim m n = replaceOptimizer m n $
	let e = lookup' n m in if
		| (n, m) `matches` ands  [pat "a", nots $ pat "a"] -> lowS
		| (n, m) `matches` nands [pat "a", nots $ pat "a"] -> highS
		| (n, m) `matches` ors   [pat "a", nots $ pat "a"] -> highS
		| (n, m) `matches` nors  [pat "a", nots $ pat "a"] -> lowS
		| (n, m) `matches` xors  [pat "a", nots $ pat "a"] -> highS
		| (n, m) `matches` xnors [pat "a", nots $ pat "a"] -> lowS
		| otherwise                                        -> e

-- valuePropagate simplifies the node if any of its inputs are constants.
valuePropagate :: Signalable a => Explicit a -> Int -> Explicit a
valuePropagate m n = replaceWith (\(x,_) -> x==n) (n, this') m
	where
		this' = valuePropagate' this sigs'
		sigs' = zip sigs $ lookupM sigs m
		(this, sigs) = case lookup n m of
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
