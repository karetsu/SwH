-- | Markov chain weather change simulations
module Chapter001.MarkovChain ( steadyState1
                              , steadyState2
                              , steadyState3
                              , steadyState4
                              ) where

-- all modules required by the below
import Data.Complex                   -- base
import qualified Data.List as L
import Data.Ord
import Numeric.LinearAlgebra.Data     -- hmatrix
import qualified Numeric.LinearAlgebra.HMatrix as M
import System.Random

-- steady state of Markov chain -------------------------------------------------
-- we will write 4 approaches
-- 1. raise the state matrix to a high power, the limiting distribution is the
--    steady state
-- 2. solve the system of eqns: pi P = pi and sum_i=1^3 pi_i = 1
-- 3. using Perron-Frobenius theorem that eigenvector for the largest eval is
--    proportional to pi, so we find it and normalise it
-- 4. running a Monte Carlo simulation

-- The setup:
transition :: Matrix R
transition = (3><3)[0.5, 0.4, 0.1,
                    0.3, 0.2, 0.5,
                    0.5, 0.3, 0.2]

data Weather = Fine | Cloudy | Rain deriving (Eq, Show)

-- route 1:
steadyState1 :: Matrix R
steadyState1 = M.takeRows 1 $ foldl (M.<>) t [t | _ <- [1..n]]
  where
    n = 10^6
    t = transition
-- >>> disp 4 steadyState1
-- 1x3
-- 0.4375  0.3125  0.2500


-- route 2:
system :: Matrix R
system = takeRows 2 (tr (transition - ident 3)) === matrix 3 [1,1,1]

target :: Matrix R
target = (3><1) [0,0,1 :: R]

steadyState2 :: Maybe (Matrix R)
steadyState2 = M.linearSolve system target
-- >>> steadyState2
-- Just (3><1)
--  [ 0.4375
--  , 0.3125
--  ,   0.25 ]


-- route 3:
evals :: Vector (Complex Double)
evecs :: Matrix (Complex Double)
(evals, evecs) = M.eig (M.tr transition)

sizes :: [R]
sizes = map M.magnitude (toList evals)

maxid :: (Ord a, Num a, Enum a) => [a] -> (a, Int)
maxid xs = L.maximumBy (comparing fst) (zip xs [0..])

getEvec :: Matrix (Complex Double)
getEvec = M.dropColumns m $ M.takeColumns (m+1) evecs
  where
    m = snd $ maxid sizes

steadyState3 :: Matrix (Complex Double)
steadyState3 = getEvec / scalar (M.sumElements getEvec)
-- >>> disp 4 steadyState3
-- (3><1)
--  [ 0.43749999999999994 :+ 0.0
--  , 0.31250000000000006 :+ 0.0
--  , 0.24999999999999997 :+ 0.0 ]


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
increment xs _ = error $ "Needed a [x, y, z], got: " ++ show xs


wToInt :: Weather -> Int
wToInt Fine   = 0
wToInt Cloudy = 1
wToInt Rain   = 2

getNext :: Weather -> Int -> Weather
getNext w n = head . drop n . genpop transition . wToInt $ w

-- our pattern now becomes like:
-- getNext 3 (getNext 1 (getNext 4 (getNext 8 Fine)))

-- because we are now dealing with random numbers we have to move to IO town
steadyState4' :: Int -> IO [Double]
steadyState4' n = do
  g <- newStdGen
  let scores' = [fromIntegral x / fromIntegral n | x <- scores]
        where
          scores = L.foldl' increment [0,0,0] states
          states = init (L.scanl' getNext Fine steps)
          steps = take n $ randomRs (0, 9) g
  return scores'

steadyState4 :: IO [Double]
steadyState4 = steadyState4' (10^6)
