-- | probabilistic stuffs
module Statistics.Probability.Probability where

-- we will mostly be doing Monte Carlo simulations for this module
import Control.Monad
import Control.Monad.Trans.State
import Data.List (nub)
import Data.Maybe (catMaybes)
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
  putStrLn $ "Theoretical: " ++ show (evenDiceT 6)
  putStr "Estimate: "
  evenDiceMC (10^6) 6


-- Partially matching passwords -------------------------------------------------
password :: String
password = "3xyZu4vN"
-- TODO
-- M-g M-g 53

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


-- Lattice paths ----------------------------------------------------------------
-- the aim is to count the number of possible moves (0,0) -> (5,5) which stay
-- above the diagonal

-- total number of possible paths 2n `choose``
paths :: Integer -> Integer
paths n = product [(n+1) .. (2*n)] `div` product [2..n]

-- tail recursive n choose k
choose :: Integer -> Integer -> Integer
choose = go 1 1
  where
    go an ak _ 0 = an `div` ak
    go _  _  0 _ = 0
    go an ak n k = go (an*n) (ak*k) (n-1) (k-1)

-- nth Catalan number
catalan :: Integer -> Integer
catalan n = (2*n) `choose` n - (2*n) `choose` (n-1)

uppers :: Integer -> Double
uppers n = numerator / denominator
  where
    numerator = fromIntegral $ catalan n
    denominator = fromIntegral $ paths n

-- ok, so now the theoretical version is calculated, we want to run all
-- possible simulations and count the number for which they stay above
-- the diagonal, this will not really even be that large for reasonably
-- large n
data Step = U | R deriving (Show, Eq)
-- using U and R for up and right to avoid clashes with Either

-- award for funnest way to generate all combinations of n values from a list
allcom :: Int -> Int -> [[Step]]
allcom n m = [n+m] >>= (`replicateM` [U,R])

numOccur :: Eq a => a -> [a] -> Int
numOccur x = length . filter (==x)

genPaths :: Int -> Int -> [[Step]] -> [[Step]]
genPaths n m = filter (\xs -> numOccur U xs == n && numOccur R xs == m)

stepToInt :: Step -> Int
stepToInt U = 1
stepToInt R = -1

isUpper :: [Int] -> Maybe [Int]
isUpper xs = if all (>=0) $ scanl (+) (head xs) (tail xs) then Just xs else Nothing

latticePaths :: Int -> Int -> [[Int]]
latticePaths n m = (map . map) stepToInt (genPaths n m (allcom n m))

uppers' :: Int -> Double
uppers' n = numerator / denominator
  where
    numerator = fromIntegral $ length . catMaybes $ map isUpper (latticePaths n n)
    denominator = fromIntegral $ length (latticePaths n n)
-- TODO: this is horrible once you get beyond about 9x9 lattices and this is
--       the point after which we cross 1GB RAM usage (although it only takes 1s on
--       my PC, change this into an accumulator which counts the uppers vs all and
--       then does the division)

-- finally a more monte carlo approach which simply counts whether you hit the
-- north or each boundary first, the aim being to improve on the overhead of the
-- above simulation which enumerates every case and we can repeat this new approach
-- a large number of times inexpensively

-- generate a step, see if it takes the path to the boundary, add one to the
-- upper accumulator if it never went below horizontal otherwise add to count
-- TODO: plot out the steps on paper to implement
