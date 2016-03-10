module Magma.Export where

import Magma.Signalable
import Magma.Explicit
import Magma.Signal
import Magma.Optim

class Exportable a where
	put :: a -> IO String

data Target
	= Vhdl

outputs = Multiple 0 "outputs"

write :: (Signalable a, Show a) => 
	Target -> [Variable] -> [Variable] -> ([Signal a] -> [Signal a]) -> IO ()
write Vhdl inps outs f = do
	graph <- toExplicit $ 
		Var outputs $ 
		zipWith (\x y -> Var x [y]) (outs) (f $ map (\x -> Var x []) inps)
	putStrLn $ show graph
