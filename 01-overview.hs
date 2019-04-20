{-# LANGUAGE FlexibleContexts #-}
{-
NOTE: Throughout this entire codebase I will be making use of Dante's REPLoid
      functionality. This is the emacs plugin I use under hlissner's doom emacs
      and you can find out more about it at: https://github.com/jyp/dante.

      All code which looks like: `-- >>> [snippet]` is a REPLoid, this line is
      executed and its output is placed in a new comment below it. Doing it this
      way lets me avoid all kinds of mess around IO
-}
module Chapter001 where


-- this is all pretty meh, figure out minimum needed later


-- all modules required by the below
import Polynomial.Roots               -- dsp
import Data.Complex                   -- base
import qualified Data.List as L
import Data.Random
import Numeric.LinearAlgebra.Data     -- hmatrix
import Numeric.LinearAlgebra.HMatrix as M
import System.Random
import Test.QuickCheck (frequency, Gen, listOf)


-- bubble sort ------------------------------------------------------------------

-- first create a function which knows how to sort through the list once
singleSort :: Ord a => [a] -> [a]
singleSort (x:x':xs)
  | x' < x    = x':singleSort (x :xs)
  | otherwise = x :singleSort (x':xs)
singleSort x = x  -- done second for patterns not matching the first type

-- now we can track how many times this has been done
bubbleSort' :: Ord a => [a] -> Int -> [a]
bubbleSort' xs i
  | i == length xs = xs
  | otherwise      = bubbleSort' (singleSort xs) (i+1)

-- finally our bubble sorting algorithm is the above initiated at 0
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = bubbleSort' xs 0
{- this might look a lot more convoluted than implementing the same kind of
   sort in other languages but we are being particularly verbose to make sure
   that this type of pattern becomes second nature to you as we use it a lot -}
-- >>> bubbleSort [1,10,5,3,9,1,1,1,0,15]
-- [0,1,1,1,1,3,5,9,10,15]


-- roots of a polynomial --------------------------------------------------------
-- no dealing with the complex problem of calculating roots, we will use
-- the Polynomial.Roots module of the `dsp` package, complex roots will look a
-- bit 'odd' to many as the output format is x :+ y for x + iy
zeros :: RealFloat a => [Complex a]
zeros = roots 1e-16 1000 [1,3,-10]
-- all the hard work here is done for us, and all we need to do is then use
-- zeros as we see fit :)
-- note that this uses Laguerre's method by default
-- >>> zeros
-- [0.5 :+ 0.0,(-0.2) :+ 0.0]


-- steady state of Markov chain -------------------------------------------------
-- we will write 4 approaches
-- 1. raise the state matrix to a high power, the limiting distribution is the
--    steady state
-- 2. solve the system of eqns: pi P = pi and sum_i=1^3 pi_i = 1
-- 3. using Perron-Frobenius theorem that eigenvector for the largest eval is
--    proportional to pi, so we find it and normalise it
-- TODO
-- 4. running a Monte Carlo simulation
-- TODO

-- The setup:
transition :: Matrix R
transition = (3><3)[0.5, 0.4, 0.1,
                    0.3, 0.2, 0.5,
                    0.5, 0.3, 0.2]

data Weather = Fine | Cloudy | Rain deriving (Eq, Show)

-- route 1:
steadyState1 :: Int -> Matrix R -> Matrix R
steadyState1 n t = foldl (M.<>) t [t | _ <- [1..n]]
-- >>> disp 4 $ takeRows 1 (steadyState1 (100) transition)
-- 1x3
-- 0.4375  0.3125  0.2500

-- route 2:
system :: Matrix R
system = takeRows 2 (tr (transition - ident 3)) === matrix 3 [1,1,1]

target :: Matrix R
target = (3><1) [0,0,1 :: R]

steadyState2 :: Maybe (Matrix R)
steadyState2 = linearSolve system target
-- >>> linearSolve system target
-- Just (3><1)
--  [ 0.4375
--  , 0.3125
--  ,   0.25 ]

-- route 3:
-- TODO

-- route 4:
-- this is absolutely not the best way to do it at all, but its an intuitive
-- approach for those with little statistical training

-- replicate each rain type as many times it needs to be weighted as given,
-- this is in order to sample with the correct probability
genpop :: Matrix R -> Int -> [Weather]
genpop t s = replicate (round (10*t!s!0) :: Int) Fine ++
             replicate (round (10*t!s!1) :: Int) Cloudy ++
             replicate (round (10*t!s!2) :: Int) Rain

-- increment a count of each weather occurrence
increment :: [Int] -> Weather -> [Int]
increment [x,y,z] w = case w of
                        Fine   -> [x+1,   y,   z]
                        Cloudy -> [  x, y+1,   z]
                        Rain   -> [  x,   y, z+1]

wToInt :: Weather -> Int
wToInt Fine   = 0
wToInt Cloudy = 1
wToInt Rain   = 2


getNext :: Weather -> Int -> Weather
getNext w n
  | n == 0    = head $ (genpop transition .wToInt) w
  | otherwise = head $ drop (n-1) (take n $ (genpop transition . wToInt) w)

-- our pattern now becomes like:
-- getNext 3 (getNext 1 (getNext 4 (getNext 8 Fine)))

-- because we are now dealing with random numbers we have to move to IO town
steadyState4 :: Int -> Weather -> IO [Int]
steadyState4 n start = do
  g <- newStdGen
  let xs = take n $ randomRs (0, 9) g
  let states = init $ scanl getNext start xs
  let scores = foldl increment [0,0,0] states
  return scores
-- incorrect, result is about [0.54, 0.32, 0.14]
-- FIXME
