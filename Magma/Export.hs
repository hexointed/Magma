module Magma.Export where

import Magma.Signal
import Magma.Optim

data Target
	= Vhdl

write :: Target -> Signal a -> IO ()
write Vhdl = undefined
	
