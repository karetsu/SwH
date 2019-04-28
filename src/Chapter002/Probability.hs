-- |

module Chapter002.Probability where

-- we will mostly be doing Monte Carlo simulations for this module
import System.Random

import Data.List (nub)
import Data.List.Split (chunksOf)


-- Dice even sum ----------------------------------------------------------------
-- roll two dice, what's the probability that the sum of the values is even?

-- theoretical solution
evenDiceT :: Int     -- number of faces
          -> Double
evenDiceT n = fromIntegral numerator / fromIntegral denominator
  where
    numerator = length $ filter even [i+j | i <- [1..n], j <- [1..n]]
    denominator = n*n

-- Monte Carlo estimate
evenDiceMC :: Int    -- number of simulations to run
           -> Int    -- number of faces
           -> IO Double
evenDiceMC n f = do
  g <- newStdGen
  let values = take (2*n) $ randomRs (1,f) g
  let numerator = length $ filter even [ uncurry (+) x
                                       | x <- zip (take n values) (drop n values)]
  let estimate = fromIntegral numerator / fromIntegral n
  return estimate

-- wrapping them up together because that's what happens in the book
rollTwo :: IO Double
rollTwo = do
  putStr "Theoretical value: "
  print $ evenDiceT 6
  putStr "Monte Carlo estimate: "
  evenDiceMC (10^6) 6


-- Partially matching passwords -------------------------------------------------
password :: String
password = "3xyZu4vN"
-- TODO


-- The birthday problem ---------------------------------------------------------
matchExists :: Int -> Float
matchExists n = 1 - product fractions
  where
    fractions = [fromIntegral k / 365 | k <- [365,364..(365-n+1)]]

matchExists' :: Int -> Float
matchExists' n = 1 - factorial
  where
    factorial = fromIntegral (product [365,364..(365-n+1)]) / (365^n)

matchedSim :: Ord a => [a] -> Int
matchedSim xs = if xs == nub xs then 0 else 1

matchExistsMC :: Int  -- number of people
              -> Int  -- number of simulations
              -> IO Double
matchExistsMC n s = do
  g <- newStdGen
  let days = chunksOf n (take (n*s) (randomRs (1,365) g)::[Int])
  let count = (sum . map matchedSim) days
  return $ fromIntegral count / fromIntegral s
