-- | Chapter 3.1: M-g M-g 85

module Statistics.Distributions.RandomVariables where

import Data.List (group, sort)
import System.Random

data Names = Mary | Mel | David | John | Kayley | Anderson
  deriving (Show, Eq, Enum, Bounded)

instance Random Names where
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
    (x, g') -> (toEnum x, g')
  random = randomR (minBound, maxBound)

-- Useful functions -------------------------------------------------------------
(./) :: (Integral a, Integral b, Fractional c) => a -> b -> c
n ./ m = fromIntegral n / fromIntegral m


-- A simple random variable -----------------------------------------------------
-- M-g M-g 86
gen :: Int -> StdGen -> [Names]
gen n g = take n (randoms g :: [Names])

acc :: [[Int]] -> [(Int, Int)]
acc [] = []
acc (x:xs) = (head x, length x) : acc xs

names :: Int -> StdGen -> [(Int,Double)]
names n g = density . acc . split' $ nameLength <$> gen n g
  where
    nameLength = length <$> show
    split'     = group . sort
    density :: [(Int, Int)] -> [(Int, Double)]
    density    = map (\(x,y) -> (x, y ./ n))

-- M-g M-g 87
