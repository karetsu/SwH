-- | probabilistic stuffs
module Chapter002.Probability where

-- we will mostly be doing Monte Carlo simulations for this module
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import Data.List (nub)
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
      numerator = length $ filter even [ uncurry (+) x
                                       | x <- zip (take n values) (drop n values)]
      estimate = fromIntegral numerator / fromIntegral n
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
match :: Int -> Float
match n = 1 - product fractions
  where
    fractions = [fromIntegral k / 365 | k <- [365,364..(365-n+1)]]

match' :: Int -> Float
match' n = 1 - factorial
  where
    factorial = fromIntegral (product [365,364..(365-n+1)]) / (365^n)

matches :: Ord a => [a] -> Int
matches xs = if xs == nub xs then 0 else 1

makeDays :: Int -> StdGen -> [Int]
makeDays n = evalState (replicateM n (state $ randomR (1,365)))

matchMC :: Int -> Int -> Double
matchMC n s = fromIntegral (go n s 0) / fromIntegral s
  where
    go :: Int -> Int -> Int -> Int
    go people sims count
      | sims < 0 = count
      | otherwise =
        let days = makeDays 23 (mkStdGen sims)  -- is this ok?
        in go people (sims - 1) (count + matches days)


-- Sampling with and without replacement ----------------------------------------

-- We have 7 fish in a pond
data Fish = Gold | Silver deriving (Show, Enum, Bounded, Eq)
instance Random Fish where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
    (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)

fishPop :: [Fish]
fishPop = [Gold, Gold, Gold, Silver, Silver, Silver, Silver]

-- the aim is to have two different scenarios, one where we fish 4 with
-- replacement, the other where we fish 4 w.out replacement

pond :: Int -> Fish
pond x
  | x < 4 = Gold
  | otherwise = Silver  -- here will only ever be applied to int <= 7


-- with replacement
fishingS :: Int -> State StdGen [Fish]
fishingS n = replicateM n (pond <$> state (randomR (1,7)))

fishing :: Int -> StdGen -> [Fish]
fishing n = evalState (fishingS n)

release4 :: IO [Fish]
release4 = fishing 4 . mkStdGen <$> randomIO


-- without replacement
remFish :: Eq a => a -> [a] -> [a]
remFish _ [] = []
remFish f (x:xs) | f == x    = xs
                 | otherwise = x : remFish f xs

fishing' :: Int -> [Fish] -> StdGen -> [Fish]
fishing' n fs = go n fs []
  where
    go :: Int -> [Fish] -> [Fish] -> StdGen -> [Fish]
    go catch fishes fished gen
      | catch > length fishes = fishes
      | catch < 0 = fished
      | otherwise =
        let
          (fish, nextGen) = randomR (1, length fishes) gen
          caught = pond fish
        in go (catch-1) (remFish caught fishes) (caught : fished) nextGen

capture4 :: IO [Fish]
capture4 = fishing' 3 fishPop . mkStdGen <$> randomIO
