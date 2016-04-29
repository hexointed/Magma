module Magma.Base where

import Data.Maybe
import qualified Data.List as List

mapHead :: (a -> a) -> [a] -> [a]
mapHead f [] = []
mapHead f (x:xs) = f x : xs

map' :: (a -> a) -> [a] -> [a]
map' f []     = []
map' f [x]    = [x]
map' f (x:xs) = f x : map' f xs

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

group' :: Ord a => [a] -> [[a]]
group' = List.group . List.sort

groupWith :: (a -> a -> Bool) -> [a] -> [[a]]
groupWith f []     = []
groupWith f (x:xs) = (x : takeWhile (f x) xs) : groupWith f (dropWhile (f x) xs)

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
lookupM keys m' = map (\n -> lookup' n m') keys

lookup' :: Eq a => a -> [(a, b)] -> b
lookup' key map = fromJust $ lookup key map

replaceWith :: (a -> Bool) -> a -> [a] -> [a]
replaceWith f a []     = []
replaceWith f a (x:xs)
	| f x       = a : replaceWith f a xs
	| otherwise = x : replaceWith f a xs

undup :: Eq a => [a] -> [a]
undup (x:y:xs)
	| x == y    = undup xs
	| otherwise = x : undup (y:xs)
undup xs        = xs
