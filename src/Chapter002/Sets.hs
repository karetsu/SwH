-- | Working with sets, chapter 2.2

module Chapter002.Sets where

import Control.Monad (liftM)

import Data.Set (Set)
import qualified Data.Set as S

import Data.List (permutations)

import System.Random
import System.Random.Shuffle (shuffle')

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


-- The probability of a union
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
probMC n g = fromIntegral (go n 0 g) / fromIntegral n
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


-- Secretary with envelopes
factorial :: Integer -> Integer
factorial n = product [1..n]

analytic :: Double
analytic = 1.0 / exp 1

toInt :: [Bool] -> Int
toInt xs = if True `elem` xs then 0 else 1


-- breaks when factorial k overflows Int, hence use of Integer
formulaic :: Integer -> Double
formulaic n = sum series
  where
    series = [ fromIntegral ((-1)^k :: Integer) / fromIntegral (factorial k)
             | k <- [0..n]]

brute :: Int -> Double
brute = undefined


shuffleMatched :: [Int] -> Int
shuffleMatched xs = toInt $ zipWith (==) xs [1..]

envMC' :: Int     -- number of envelopes
      -> Int     -- number of iterations
      -> StdGen  -- random number generator
      -> Double  -- probability all miss
envMC' n n' g = fromIntegral (go n n' 0 g) / fromIntegral n'
  where
    go :: Int -> Int -> Int -> StdGen -> Int
    go envs sims acc gen
      | sims == 0 = acc
      | otherwise =
        let
          xs = shuffle' [1..n] n gen
          (_,ngen) = randomR (1,n) gen
        in go envs (sims - 1) (acc + shuffleMatched xs) ngen

envMC :: Int -- number of envelopes
      -> IO Double
envMC n = envMC' n (10^6) . mkStdGen <$> randomIO
