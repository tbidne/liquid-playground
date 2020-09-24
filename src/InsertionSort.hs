module InsertionSort
  ( elems,
    sort,
    module Data.IncList,
  )
where

import Data.IncList
import Data.IncList.Internal

{-@ sort :: Ord a => xs:[a] -> ys:IncListNE a {len xs} {elems xs} @-}
sort :: Ord a => [a] -> IncList a
sort [] = Nil
sort (x : xs) = insert x (sort xs)

{-@ insert :: Ord a => x:a -> xs:_ -> IncListNE a {1 + incLen xs} {incAddElem x xs} @-}
insert :: Ord a => a -> IncList a -> IncList a
insert y Nil = y :<: Nil
insert y (x :<: xs)
  | y <= x = y :<: x :<: xs
  | otherwise = x :<: insert y xs