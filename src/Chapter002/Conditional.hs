-- | Conditional probability

module Chapter002.Conditional where


import Data.Set (Set)
import qualified Data.Set as S

import System.Random

-- Useful functions -------------------------------------------------------------
(./) :: (Integral a, Integral b, Fractional c) => a -> b -> c
n ./ m = fromIntegral n / fromIntegral m

(@/) :: (Integral a, Integral b, Fractional c) => (a,b) -> c
(@/) = uncurry (./)


-- Conditional probability ------------------------------------------------------
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
data Door = One | Two | Three deriving (Eq, Bounded, Enum, Ord, Show)
instance Random Door where
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
    (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)

game :: Set Door
game = S.fromList [One, Two, Three]

genPrize :: StdGen -> Door
genPrize g = S.elemAt r game
  where
    (r,_) = randomR (0,2) g


reveal :: Door -> Door -> StdGen -> Door
reveal guess prize gen
  | guess == prize = S.elemAt r $ game S.\\ S.singleton prize
  | otherwise      = S.elemAt 0 $ game S.\\ S.fromList [guess, prize]
  where
    (r,_) = randomR (0,1) gen

{- get 3 doors >> pick one as the prize >> player makes guess
               >> door revealed         >> player makes choice
               >> prize revealed -}
