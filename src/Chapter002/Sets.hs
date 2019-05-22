-- | Working with sets, chapter 2.2

module Chapter002.Sets where

import Data.Set (Set)
import qualified Data.Set as S

import System.Random

-- Useful functions -------------------------------------------------------------
(./) :: (Integral a, Integral b, Fractional c) => a -> b -> c
n ./ m = fromIntegral n / fromIntegral m


-- Basic set operations ---------------------------------------------------------
setA :: Set Int
setA = S.fromList [2,7,2,3]

setB :: Set Int
setB = S.fromList [1..6]

unionAB :: Set Int
unionAB = setA `S.union` setB

intersectAB :: Set Int
intersectAB = setA `S.intersection` setB

diffAB :: Set Int
diffAB = setA S.\\ setB

symdiffAB :: Set Int
symdiffAB = (setA `S.union` setB) S.\\ (setA `S.intersection` setB)

elemA6 :: Bool
elemA6 = 6 `S.member` setA

subsets :: (Bool, Bool)
subsets = ( symdiffAB `S.isSubsetOf` unionAB
          , intersectAB `S.isSubsetOf` unionAB)


-- The probability of a union ---------------------------------------------------
setC :: Set Char
setC = S.fromList "aeiou"

setD :: Set Char
setD = S.fromList "xyz"

omega :: String
omega = ['a'..'z']
-- AIM: repeatedly sample elements from omega and see if they are in either
--      set C or set D, doing it enough times should give us the probability

matched :: Char -> Int
matched s = if s `elem` setC || s `elem` setD then 1 else 0

probMC :: Int -> StdGen -> Double
probMC n g = go n 0 g ./ n
  where
    go :: Int -> Int -> StdGen -> Int
    go count agg gen
      | count < 0 = agg
      | otherwise =
        let (sim, ngen) = randomR (0, 25) gen
        in go (count - 1) (agg + matched (omega!!sim)) ngen

correctElem :: Int -> IO Double
correctElem n = probMC n . mkStdGen <$> randomIO
-- doing it incorrectly as per the Julia implementation is actually much
-- harder in haskell - building it as above I would have to go out of my way
-- to write 'matched' in such a way that it checks an s and t being in the two
-- different sets


-- Independent events -----------------------------------------------------------
-- we will show that randomly picking 13 from 10..25 and prob first digit is 1
-- & prob second digit is 3 are not independent of each other.

prob13 :: Int -> StdGen -> [Double]
prob13 n g = (./ n) <$> go n 0 0 0 g
  where
    go :: Int -> Int -> Int -> Int -> StdGen -> [Int]
    go sims acc13 acc1x accX3 gen
      | sims == 0 = [acc13, acc1x, accX3]
      | otherwise = let
          x :: Int
          (x, ngen) = randomR (10, 23) gen
          found13 = if x == 13 then 1 else 0
          found1x = if x `div` 10 == 1 then 1 else 0
          foundX3 = if x `rem` 10 == 3 then 1 else 0
        in
          go (sims-1)
             (acc13 + found13)
             (acc1x + found1x)
             (accX3 + foundX3)
             ngen

intersect :: Int -> [Double] -> Bool
intersect n [x,y,z] = x == (y*z / fromIntegral n)
intersect _ _ = False

independent :: Int -> IO Bool
independent n = intersect n . prob13 n . mkStdGen <$> randomIO
