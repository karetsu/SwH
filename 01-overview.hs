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
import Data.Random.Extras             -- random-extras
import Data.Random.Shuffle.Weighted   -- random-extras
import Data.Random.Source.Std
import Data.Random
import Numeric.LinearAlgebra.Data     -- hmatrix
import Numeric.LinearAlgebra.HMatrix ( linearSolve
                                     , eig
                                     , Element)
import System.Random

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
tvals :: [R]
tvals = [0.5, 0.4, 0.1,
         0.3, 0.2, 0.5,
         0.5, 0.3, 0.2]

transition :: Matrix R
transition = matrix 3 tvals

-- route 1:
steadyState1 :: Int -> Matrix R -> Matrix R
steadyState1 n t = foldl (<>) t [t | _ <- [1..n]]
-- >>> disp 4 $ takeRows 1 (steadyState1 (100) transition)
-- 1x3
-- 0.4375  0.3125  0.2500

-- route 2:
system :: Matrix R
system = takeRows 2 (tr (transition - ident 3)) === matrix 3 [1,1,1]

target :: Matrix R
target = (3><1) [0,0,1 :: R]
-- >>> linearSolve system target
-- Just (3><1)
--  [ 0.4375
--  , 0.3125
--  ,   0.25 ]

-- route 3:
-- TODO

-- route 4:
-- some kind of segue needed on random number generation
getRow :: Element t => Int -> Matrix t -> Matrix t
getRow n m = dropRows (n-1) $ takeRows n m
-- this is done considering matrices as 1 indexed

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

genState :: (MonadRandom m, Num a) => [(Double, a)] -> m [a]
genState wp = runRVar (weightedSample 1 wp) StdRandom

steadyState4 :: [Double]
steadyState4 = undefined
-- >>> weightedPop = zip [0.5, 0.4, 0.1] [1..3]
-- >>> runRVar (weightedSample 1 weightedPop) StdRandom
-- this part here does the actual sampling of the numbers based on their
-- weights, we need to go from that into the application on page 31 of the book
