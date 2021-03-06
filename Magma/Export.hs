{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module Magma.Export (Target(..), write, write', var, vector) where

import Magma.Signalable
import Magma.Listable
import Magma.Explicit
import Magma.Signal
import Magma.Target
import Magma.Optim
import Magma.Base
import Data.List 
import Data.Char

var :: String -> Signal Bool
var s = Var (Single s) []

vector :: Int -> String -> [Signal Bool]
vector n s = reverse $ map (\x -> Var (Multiple x s) []) [0 .. n - 1]

write' lang name outs f = write lang allOptims name outs f

write :: Listable c => Target -> [Optimizer Bool] -> String -> c -> c -> IO ()
write lang opts name outs f = do
	if not (listsEq outs f) 
		then error "outputs don't match" 
		else (return ())
	let sig = combine $ zipWith (\x y -> setDep x y) (list outs) (list f)
	sig' <- optimize opts sig
	graph <- toExplicit sig'
	putStrLn $ show graph
	let o = output lang name $ tail graph
	putStrLn o
		where
			combine = Var (Single "out")

output :: Target -> String -> Explicit Bool -> String
output Vhdl = outputVhdl

outputVhdl :: String -> Explicit Bool -> String
outputVhdl name graph = unlines $
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
	"end behav;" :
	[]
		where
			ios  = ins ++ outs
			ins  =
				map (\xs -> (getName $ head xs) ++ " : in " ++ getType xs) $
				groupWith (\x y -> getName x == getName y) . sort $
				map (\(V v vs) -> v) $
				filter (\(V v vs) -> null vs) vars
			outs = 
				map (\xs -> (getName $ head xs) ++ " : out " ++ getType xs) $
				groupWith (\x y -> getName x == getName y) . sort $
				map (\(V v vs) -> v) $
				filter (\(V v vs) -> not $ null vs) vars
			vars = map snd $ filter isVar graph

setDep :: Signal Bool -> Signal Bool -> Signal Bool
setDep (Var v vs) s = Var v [s]

getType :: [Variable] -> String
getType [Single _] = "std_logic"
getType xs         = "std_logic_vector(" ++ show (length xs - 1) ++ " downto 0)"

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
	if | a == high -> "\'1\'"
	   | a == low  -> "\'0\'"
gateTranslate Vhdl (n, S g ns) = 
	"w" ++ 
	show n ++ 
	" <= " ++ 
	" ( " ++
	concat ( map' (++" "++ map toLower (show g)++" ") $ map (\n -> "w" ++ show n) ns) ++
	" );"
gateTranslate Vhdl (n, F Mux [s,a,b]) =
	"with w" ++
	show s ++
	" select w" ++
	show n ++
	" <= w" ++
	show a ++
	" when '0', w" ++
	show b ++
	" when '1';"
