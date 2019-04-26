-- |

module Chapter002.Probability where

-- we will mostly be doing Monte Carlo simulations for this module
import System.Random

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

