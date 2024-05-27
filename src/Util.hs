{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Util (
  printElementCounts
  , symmetricDifference
  , assoc
  ) where

import Data.List (nub, (\\), sortBy)
import Data.Ord (comparing)

-- Function to count occurrences of each element in a list
countOccurrences :: (Eq a) => [a] -> a -> Int
countOccurrences list x = length (filter (== x) list)

-- Function to print each distinct element followed by its count in descending order of occurrence
printElementCounts :: (Show a, Eq a) => [a] -> IO ()
printElementCounts list = do
    let distinctElements = nub list
    let counts = map (\x -> (x, countOccurrences list x)) distinctElements
    let sortedCounts = sortBy (comparing (negate . snd)) counts  -- Sorting in descending order of count
    mapM_ (\(x, count) -> putStrLn $ show x ++ ": " ++ show count) sortedCounts


-- Define a function to compute the symmetric difference of two lists
symmetricDifference :: Eq a => [a] -> [a] -> [a]
symmetricDifference xs ys = (uniqueXs \\ uniqueYs) ++ (uniqueYs \\ uniqueXs)
  where
    uniqueXs = nub xs  -- Remove duplicates from the first list
    uniqueYs = nub ys  -- Remove duplicates from the second list

-- Define a function to restructure a tuple
assoc :: (a, (b, c)) -> ((a, b), c)
assoc (x, (y, z)) = ((x, y), z)

-- Example usage
-- main :: IO ()
-- main = do
--     let myList = ["apple", "banana", "apple", "orange", "banana", "apple"]
--     printElementCounts myList
