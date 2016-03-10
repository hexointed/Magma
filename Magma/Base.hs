module Magma.Base where

import Data.Maybe

mapHead :: (a -> a) -> [a] -> [a]
mapHead f [] = []
mapHead f (x:xs) = f x : xs

filll :: Int -> a -> [a] -> [a]
filll n y ys = replicate (n - length ys) y ++ ys

fillr :: [a] -> Int -> a -> [a]
fillr []     n x = replicate n x
fillr (y:ys) n x = y : fillr ys (n - 1) x

filterPos :: (Num t, Enum t) => (t -> Bool) -> [a] -> [a]
filterPos f = map snd . filter (f . fst) . zip [1..]

even' :: [a] -> [a]
even' = filterPos (even :: Int -> Bool)

odd' :: [a] -> [a]
odd' = filterPos (odd :: Int -> Bool)

tail' :: [a] -> [a] -> [a]
tail' [] bs = bs
tail' as [] = as
tail' (a:as) (b:bs) = tail' as bs

group :: Int -> [a] -> [[a]]
group n [] = []
group n xs = take n xs : group n (drop n xs)

groupFold :: Int -> (a -> a -> a) -> a -> [a] -> a
groupFold n f b []  = b
groupFold n f b [a] = f a b
groupFold n f b as = groupFold n f b $ map (foldr f b) (group n as)

group2 :: [a] -> [(a, a)]
group2 as = zip (odd' as) (even' as)

group2Fold :: (a -> a -> a) -> a -> [a] -> a
group2Fold f b []  = b
group2Fold f b [a] = f a b
group2Fold f b as 
	| even $ length as = group2Fold f b $ map (\(x, y) -> f x y) $ group2 as
	| otherwise        = group2Fold f b ((group2Fold f b $ init as) : [last as])

lookupM :: Eq a => [a] -> [(a, b)] -> [b]
lookupM keys m' = map (\n -> fromJust $ lookup n m') keys

replaceWith :: (a -> Bool) -> a -> [a] -> [a]
replaceWith f a []     = []
replaceWith f a (x:xs)
	| f x       = a : replaceWith f a xs
	| otherwise = x : replaceWith f a xs
