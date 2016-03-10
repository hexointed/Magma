{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Magma.Export where

import Magma.Signalable
import Magma.Explicit
import Magma.Signal
import Magma.Optim
import qualified Data.List as List
import Data.Foldable

data Target
	= Vhdl

outputs = Multiple 0 "outputs"

write :: (Show a, Functor b, Foldable c) => 
	Target ->
	String ->
	b Variable -> 
	c Variable -> 
	(b (Signal a) -> c (Signal a)) 
	-> IO ()
write Vhdl name inps outs f = do
	graph <- toExplicit $ 
		Var outputs $ zipWith (\x y -> Var x [y]) 
			(toList outs) 
			((toList . f) $ fmap (\x -> Var x []) inps)
	let output =
		"library ieee;" :
		"use ieee.std_logic_1164.all;" :
		"" :
		("entity " ++ name ++ " is port(") :
		unlines (map ("\t"++) (map' (++";") ios)) :
		("end " ++ name ++ ";") :
		"" :
		("architecture behav of " ++ name ++ " is") :
		unlines (map (("\tsignal w"++) . (++" : std_logic;") . show) [2 ..]) :
		"begin" :
		unlines (map (gateTranslate Vhdl) graph) :
		("end " ++ name ++ ";") :
		[]
	putStrLn $ show graph
		where
			ios = undefined
			map' = undefined

gateTranslate :: Target -> SRef a -> String
gateTranslate = undefined
