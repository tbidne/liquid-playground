{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.IncList.Internal
  ( IncList (..),
    IncListNE,
    Show (..),
    elems,
    len,
    incAddElem,
    incElems,
    incLen,
    toList,
  )
where

import qualified Data.Set as Set

{-@ data IncList a =
    Nil
  | (:<:) (_x :: a) (_xs :: IncList {y:a | _x <= y}) @-}
data IncList a
  = Nil
  | (:<:) a (IncList a)
  deriving (Eq, Ord)

instance Show a => Show (IncList a) where
  show = show . toList

infixr 5 :<:

toList :: IncList a -> [a]
toList Nil = []
toList (x :<: xs) = x : toList xs

---------------------------------------------
-- Liquid measures/functions for IncList a --
---------------------------------------------

{-@ measure incLen @-}
{-@ incLen :: IncList a -> Nat @-}
incLen :: IncList a -> Int
incLen Nil = 0
incLen (_ :<: xs) = 1 + incLen xs

{-@ measure incElems @-}
incElems :: Ord a => IncList a -> Set.Set a
incElems Nil = Set.empty
incElems (x :<: xs) = Set.union (Set.singleton x) (incElems xs)

{-@ inline incAddElem @-}
incAddElem :: Ord a => a -> IncList a -> Set.Set a
incAddElem x xs = Set.union (Set.singleton x) (incElems xs)

{-@ type IncListNE a N S = { xs:IncList a | N = incLen xs && incElems xs = S } @-}
type IncListNE a = IncList a

---------------------------------------
-- Liquid measures/functions for [a] --
---------------------------------------

{-@ measure len @-}
{-@ len :: [a] -> Nat @-}
len :: [a] -> Int
len [] = 0
len (_ : xs) = 1 + len xs

{-@ measure elems @-}
elems :: Ord a => [a] -> Set.Set a
elems [] = Set.empty
elems (x : xs) = Set.union (Set.singleton x) (elems xs)

{-@ inline addElem @-}
addElem :: Ord a => a -> [a] -> Set.Set a
addElem x xs = Set.union (Set.singleton x) (elems xs)