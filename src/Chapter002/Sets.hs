-- | Working with sets, chapter 2.2

module Chapter002.Sets where

import Data.Set (Set)
import qualified Data.Set as S

import Data.List (permutations)
import Data.Monoid (mconcat)

import System.Random
import System.Random.Shuffle (shuffle')

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


-- Conditional probability ------------------------------------------------------
(@/) :: (Integral a, Integral b, Fractional c) => (a,b) -> c
(@/) = uncurry (./)

zeta :: Int  -- truncate
     -> Int  -- s
     -> Double
zeta t s = sum $ uncurry (./) <$> take t [(1, n^s) | n <- [1..]]

-- for our purposes we will truncate at n=2000
zeta' :: Int -> Double
zeta' = zeta 2000

-- well manufactured example M-g M-g 78
{-
A manufactoring process is sensitive to the number of dust particles in the
environment. Assume:

P(A|B_k) = 1 - 1/(k+1)
where
B_k is the probability of k dust particles in the room.

P(B_k) = 6 / (pi^2 (k+1)^2)

After a bit of faffing (Basel problem included) we end up at P(A) being the
limit as n-> infty of sum_k P(A|B_k)P(B_k) which neatly reduces to:

(pi^2 - 6 zeta(3))/pi^2

Technically an application of the Law of Total Probability
-}
manFailureAn :: Double
manFailureAn = (pi**2 - 6* zeta' 3) / pi**2


-- Bayes' Rule ------------------------------------------------------------------

-- false 1 (true 0)
ep0 :: Double
ep0 = 0.1

-- false 0 (true 1)
ep1 :: Double
ep1 = 0.05

bayesAn :: Double
bayesAn = 0.7 * (1-ep1) / (0.7*(1-ep1) + 0.3*ep0)

flipBit :: Int -> Int
flipBit 0 = 1
flipBit _ = 0

genStream :: StdGen -> [Int]
genStream g = let (bit, ngen) = randomR (0,10) g in go bit : genStream ngen
  where
    go :: Int -> Int
    go x = if x < 7 then 1 else 0

tx :: Int -> StdGen -> [Int]
tx n g = take n $ genStream g

rx :: [Int] -> StdGen -> [Int]
rx []     _ = []
rx (x:xs) g =
  let (prob, ngen) = randomR (0.0, 1.0) g  in go x prob : rx xs ngen
  where
    go :: Int -> Double -> Int
    go bit chance
      | bit == 0 && chance < ep0 = 1
      | bit == 1 && chance < ep1 = 0
      | otherwise                = x

cleanRate :: Int -> StdGen -> Double
cleanRate n g = numerator ./ denominator
  where
    ts = tx n g
    rs = rx ts g
    numerator = length $ filter (== True) (zipWith (==) ts rs)
    denominator = length rs

txrxBayes :: Int -> IO Double
txrxBayes n = cleanRate n . mkStdGen <$> randomIO


-- Monty Hall -------------------------------------------------------------------
reveal :: Int -> Int -> StdGen -> Int
reveal guess prize gen
  | guess == prize = undefined -- get random element from the set with prize removed
  | otherwise      = undefined -- pick the door which is not the prize
