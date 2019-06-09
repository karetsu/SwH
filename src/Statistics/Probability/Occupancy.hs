-- |

module Statistics.Probability.Occupancy where

import System.Random

import Data.List (permutations)
import System.Random.Shuffle (shuffle')

-- Useful functions -------------------------------------------------------------
(./) :: (Integral a, Integral b, Fractional c) => a -> b -> c
n ./ m = fromIntegral n / fromIntegral m


-- Secretary with envelopes -----------------------------------------------------
factorial :: Integer -> Integer
factorial n = product [1..n]

toInt :: [Bool] -> Int
toInt xs = if True `elem` xs then 0 else 1

shuffleMatched :: [Int] -> Int
shuffleMatched xs = toInt $ zipWith (==) xs [1..]

-- approach 1: by formula
formulaic :: Integer -> Double
formulaic n = sum series
  where
    series = [ ((-1)^k :: Integer) ./ factorial k
             | k <- [0..n]]

-- approach 2: by brute force
brute :: Int -> Double
brute n = numerator ./ product [1..n]
  where
    numerator = sum $ shuffleMatched <$> permutations [1..n]

-- approach 3: analytic solution
envAnalytic :: Double
envAnalytic = 1.0 / exp 1

-- approach 4: monte carlo approximation
envMC' :: Int     -- number of envelopes
       -> Int     -- number of iterations
       -> StdGen  -- random number generator
       -> Double  -- probability all miss
envMC' n n' g = go n n' 0 g ./ n'
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

-- The Occupancy Problem --------------------------------------------------------
-- Cannot use int because we will get some divisions by 0
occAnalytic :: Integer -> Integer -> Double
occAnalytic n r = sum [ term1 k * fromIntegral (term2 n k) * term3 n r k
                      | k <- [0..n] ]
  where
    term1 k = (-1)^k
    term2 x y = product [1+x-y..n] `div` product [1..y]
    term3 x y k = (1.0 - k ./ x) ** fromIntegral y

throw :: Eq a => a -> [a] -> [a]
throw _ [] = []
throw x xs = filter (/=x) xs

throwRandom :: [Int] -> Int -> StdGen -> [Int]
throwRandom [] _ _ = []
throwRandom xs r g = go xs r g
  where
    ln = length xs
    go :: [Int] -> Int -> StdGen -> [Int]
    go env cards gen
      | cards == 0 = env
      | otherwise = let
          (x, nGen) = randomR (1, ln) gen
        in go (throw x env) (cards-1) nGen

allFull :: [Int] -> Int
allFull [] = 1
allFull _  = 0

occMC :: Int -> Int -> Int -> StdGen -> Double
occMC n r k g = go n r k 0 g ./ k
  where
    go :: Int -> Int -> Int -> Int -> StdGen -> Int
    go slots picks sims acc gen
      | sims == 0 = acc
      | otherwise
      = let nGen = snd (next gen)
        in go slots picks (sims-1)
              (acc + allFull (throwRandom [1..slots] picks gen)) nGen
