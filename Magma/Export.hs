{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module Magma.Export where

import Magma.Signalable
import Magma.Explicit
import Magma.Signal
import Magma.Optim
import Magma.Base
import qualified Data.List as List
import Data.Foldable
import Data.Traversable
import Data.Char

data Target
	= Vhdl

outputs = Multiple 0 "outputs"

write :: (Signalable a, Show a, Foldable c) => 
	Target ->
	String ->
	c Variable -> 
	c (Signal a) 
	-> IO ()
write Vhdl name outs f = do
	graph <- toExplicit $ 
		Var outputs $ zipWith (\x y -> Var x [y]) 
			(toList outs) (toList f)
	let o = output name $ tail graph
	putStrLn o

output :: Signalable a => String -> Explicit a -> String
output name graph = unlines $
	"library ieee;" :
	"use ieee.std_logic_1164.all;" :
	"" :
	("entity " ++ name ++ " is port(") :
	unlines (map ("\t"++) (map' (++";") ios)) :
	"\t);" :
	("end " ++ name ++ ";") :
	"" :
	("architecture behav of " ++ name ++ " is") :
	unlines (map
		(("\tsignal w"++) . (++" : std_logic;") . show) 
		(map fst $ 
			filter 
			(\(n, s) -> case s of
				V v vs -> null vs
				_      -> True
			) 
			graph)) :
	"begin" :
	(unlines $ map ("\t"++ ) $ map (gateTranslate Vhdl) graph) :
	("end " ++ name ++ ";") :
	[]
		where
			ios  = ins ++ outs
			ins  =
				map (\xs -> (getName $ head xs) ++ " : in " ++ getType xs) $
				group' $
				map (\(V v vs) -> v) $
				filter (\(V v vs) -> null vs) vars
			outs = 
				map (\xs -> (getName $ head xs) ++ " : out " ++ getType xs) $
				group' $
				map (\(V v vs) -> v) $
				filter (\(V v vs) -> not $ null vs) vars
			vars = map snd $ filter isVar graph
			vectorize 1 = "?"
			vectorize n = "_vector (" ++ show (n-1) ++ " downto 0)"

getType :: [Variable] -> String
getType (x:[]) = "std_logic"
getType xs     = "std_logic_vector (" ++ show (length xs - 1) ++ " downto 0)"

isVar :: SRef a -> Bool
isVar (n, V _ _) = True
isVar (n, _)     = False

gateTranslate :: Signalable a => Target -> SRef a -> String
gateTranslate Vhdl (n, V v vs) = case vs of
	[] ->
		"w" ++
		show n ++
		" <= " ++
		getName v ++
		case v of
			(Single _) -> ""
			(Multiple r _) -> "(" ++ show r ++ ")"
		++
		";"
	vs -> 
		getName v ++
		case v of
			(Single _) -> ""
			(Multiple r _) -> "(" ++ show r ++ ")"
		++
		" <= " ++
		"w" ++
		show (head vs) ++
		";"
gateTranslate Vhdl (n, D a ns) =
	"w" ++
	show n ++
	" <= " ++
	if | a == high -> "1"
	   | a == low  -> "0"
gateTranslate Vhdl (n, S g ns) = 
	"w" ++ 
	show n ++ 
	" <= " ++ 
	map toLower (show g) ++ 
	" ( " ++
	List.concat ( map' (++" & ") $ map (\n -> "w" ++ show n) ns) ++
	" );"

var :: String -> Signal Bool
var s = Var (Single s) []

vector :: Int -> String -> [Signal Bool]
vector n s = reverse $ map (\x -> Var (Multiple x s) []) [0 .. n - 1]

