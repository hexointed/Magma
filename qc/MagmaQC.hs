{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Magma

instance Arbitrary Gate where
	arbitrary = do
		n <- choose (0, 6) :: Gen Int
		return $ case n of
			0 -> Not
			1 -> And
			2 -> Nand
			3 -> Or
			4 -> Nor
			5 -> Xor
			6 -> Xnor

instance Arbitrary (Signal Bool) where
	arbitrary = do
		n <- choose (0, 10) :: Gen Int
		gen' n
			where
				gen' 0 = do
					n <- arbitrary
					return $ Var n
				gen' n = do
					a <- gen' $ n-1
					b <- gen' $ n-1
					c <- arbitrary
					return $ Sig c [a, b]

prop_reify_retie :: Signal Bool -> Bool
prop_reify_retie s = (reif s) == (reif $ reTie $ reif s)
	where
		reif = snd . unsafeReify
