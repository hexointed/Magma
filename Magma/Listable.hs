{-# LANGUAGE FlexibleInstances #-}

module Magma.Listable where

import Magma.Signal

class Show a => Listable a where
	list :: a -> [Signal Bool]
	listsEq :: a -> a -> Bool

instance Listable a => Listable [a] where
	list = concat . map list
	listsEq a b = length a == length b && and (zipWith listsEq a b)

instance (Listable a, Listable b) => Listable (a, b) where
	list (a, b) = list a ++ list b
	listsEq (a, b) (c, d) = listsEq a c && listsEq b d

instance (Listable a, Listable b, Listable c) => Listable (a, b, c) where
	list (a, b, c) = list a ++ list b ++ list c
	listsEq (a, b, c) (d, e, f) =
		listsEq a d &&
		listsEq b e &&
		listsEq c f

instance 
	(Listable a,
	Listable b,
	Listable c,
	Listable d) => 
	Listable (a, b, c, d) where
		list (a, b, c, d) = list a ++ list b ++ list c ++ list d
		listsEq (a, b, c, d) (e, f, g, h) =
			listsEq a e &&
			listsEq b f &&
			listsEq c g &&
			listsEq d h

instance 
	(Listable a, 
	Listable b, 
	Listable c, 
	Listable d, 
	Listable e) =>
	Listable (a, b, c, d, e) where
		list (a, b, c, d, e) =
			list a ++
			list b ++
			list c ++
			list d ++
			list e 
		listsEq (a, b, c, d, e) (f, g, h, i, j) =
			listsEq a f &&
			listsEq b g &&
			listsEq c h &&
			listsEq d i &&
			listsEq e j

instance
	(Listable a,
	Listable b,
	Listable c,
	Listable d,
	Listable e,
	Listable f) =>
	Listable (a, b, c, d, e, f) where
		list (a, b, c, d, e, f) =
			list a ++
			list b ++
			list c ++
			list d ++
			list e ++
			list f
		listsEq (a, b, c, d, e, f) (g, h, i, j, k, l) =
			listsEq a g &&
			listsEq b h &&
			listsEq c i &&
			listsEq d j &&
			listsEq e k &&
			listsEq f l

instance
	(Listable a,
	Listable b,
	Listable c,
	Listable d,
	Listable e,
	Listable f,
	Listable g) => 
	Listable (a, b, c, d, e, f, g) where
		list (a, b, c, d, e, f, g) =
			list a ++
			list b ++
			list c ++
			list d ++
			list e ++
			list f ++
			list g
		listsEq (a, b, c, d, e, f, g) (h, i, j, k, l, m, n) =
			listsEq a h &&
			listsEq b i &&
			listsEq c j &&
			listsEq d k &&
			listsEq e l &&
			listsEq f m &&
			listsEq g n

instance 
	(Listable a,
	Listable b,
	Listable c,
	Listable d,
	Listable e,
	Listable f,
	Listable g,
	Listable h) => 
	Listable (a, b, c, d, e, f, g, h) where
		list (a, b, c, d, e, f, g, h) = 
			list a ++
			list b ++
			list c ++
			list d ++
			list e ++
			list f ++
			list g ++
			list h
		listsEq (a, b, c, d, e, f, g, h) (i, j, k, l, m, n, o, p) =
			listsEq a i &&
			listsEq b j &&
			listsEq c k &&
			listsEq d l &&
			listsEq e m &&
			listsEq f n &&
			listsEq g o &&
			listsEq h p

instance 
	(Listable a,
	Listable b,
	Listable c,
	Listable d,
	Listable e,
	Listable f,
	Listable g,
	Listable h,
	Listable i) =>
	Listable (a, b, c, d, e, f, g, h, i) where
		list (a, b, c, d, e, f, g, h, i) =
			list a ++
			list b ++
			list c ++
			list d ++
			list e ++
			list f ++
			list g ++
			list h ++
			list i
		listsEq (a, b, c, d, e, f, g, h, i) (j, k, l, m, n, o, p, q, r) =
			listsEq a j &&
			listsEq b k &&
			listsEq c l &&
			listsEq d m &&
			listsEq e n &&
			listsEq f o &&
			listsEq g p &&
			listsEq h q &&
			listsEq i r

instance 
	(Listable a,
	Listable b,
	Listable c,
	Listable d,
	Listable e,
	Listable f,
	Listable g,
	Listable h,
	Listable i,
	Listable j) =>
	Listable (a, b, c, d, e, f, g, h, i, j) where
		list (a, b, c, d, e, f, g, h, i, j) =
			list a ++
			list b ++
			list c ++
			list d ++
			list e ++
			list f ++
			list g ++
			list h ++
			list i ++
			list j
		listsEq (a, b, c, d, e, f, g, h, i, j) (k, l, m, n, o, p, q, r, s, t) =
			listsEq a k &&
			listsEq b l &&
			listsEq c m &&
			listsEq d n &&
			listsEq e o &&
			listsEq f p &&
			listsEq g q &&
			listsEq h r &&
			listsEq i s &&
			listsEq j t

instance 
	(Listable a,
	Listable b,
	Listable c,
	Listable d,
	Listable e,
	Listable f,
	Listable g,
	Listable h,
	Listable i,
	Listable j,
	Listable k) =>
	Listable (a, b, c, d, e, f, g, h, i, j, k) where
		list (a, b, c, d, e, f, g, h, i, j, k) =
			list a ++
			list b ++
			list c ++
			list d ++
			list e ++
			list f ++
			list g ++
			list h ++
			list i ++
			list j ++
			list k
		listsEq (a, b, c, d, e, f, g, h, i, j, k)
		        (l, m, n, o, p, q, r, s, t, u, v) =
			listsEq a l &&
			listsEq b m &&
			listsEq c n &&
			listsEq d o &&
			listsEq e p &&
			listsEq f q &&
			listsEq g r &&
			listsEq h s &&
			listsEq i t &&
			listsEq j u &&
			listsEq k v

instance 
	(Listable a,
	Listable b,
	Listable c,
	Listable d,
	Listable e,
	Listable f,
	Listable g,
	Listable h,
	Listable i,
	Listable j,
	Listable k,
	Listable l) =>
	Listable (a, b, c, d, e, f, g, h, i, j, k, l) where
		list (a, b, c, d, e, f, g, h, i, j, k, l) =
			list a ++
			list b ++
			list c ++
			list d ++
			list e ++
			list f ++
			list g ++
			list h ++
			list i ++
			list j ++
			list k ++
			list l
		listsEq (a, b, c, d, e, f, g, h, i, j, k, l)
		        (m, n, o, p, q, r, s, t, u, v, w, x) =
			listsEq a m &&
			listsEq b n &&
			listsEq c o &&
			listsEq d p &&
			listsEq e q &&
			listsEq f r &&
			listsEq g s &&
			listsEq h t &&
			listsEq i u &&
			listsEq j v &&
			listsEq k w &&
			listsEq l x

instance 
	(Listable a,
	Listable b,
	Listable c,
	Listable d,
	Listable e,
	Listable f,
	Listable g,
	Listable h,
	Listable i,
	Listable j,
	Listable k,
	Listable l,
	Listable m) =>
	Listable (a, b, c, d, e, f, g, h, i, j, k, l, m) where
		list (a, b, c, d, e, f, g, h, i, j, k, l, m) =
			list a ++
			list b ++
			list c ++
			list d ++
			list e ++
			list f ++
			list g ++
			list h ++
			list i ++
			list j ++
			list k ++
			list l ++
			list m
		listsEq (a, b, c, d, e, f, g, h, i, j, k, l, m)
		        (n, o, p, q, r, s, t, u, v, w, x, y, z) =
			listsEq a n &&
			listsEq b o &&
			listsEq c p &&
			listsEq d q &&
			listsEq e r &&
			listsEq f s &&
			listsEq g t &&
			listsEq h u &&
			listsEq i v &&
			listsEq j w &&
			listsEq k x &&
			listsEq l y &&
			listsEq m z

instance Listable (Signal Bool) where
	list = (:[])
	listsEq a b = True

