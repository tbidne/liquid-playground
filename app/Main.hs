{-# LANGUAGE TypeApplications #-}

module Main (main) where

import InsertionSort

main :: IO ()
main = print $ sort @Int [4, 1, 6, 0, 2, 6, 7, 2, -5, 6, 1]
