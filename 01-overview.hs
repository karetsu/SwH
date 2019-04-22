{-
NOTE: Throughout this entire codebase I will be making use of Dante's REPLoid
      functionality. This is the emacs plugin I use under hlissner's doom emacs
      and you can find out more about it at: https://github.com/jyp/dante.

      All code which looks like: `-- >>> [snippet]` is a REPLoid, this line is
      executed and its output is placed in a new comment below it. Doing it this
      way lets me avoid all kinds of mess around IO
-}
module Chapter001 where

-- all modules required by the below
import Polynomial.Roots               -- dsp
import Data.Complex                   -- base
import qualified Data.List as L
import Data.Ord
import Numeric.LinearAlgebra.Data     -- hmatrix
import qualified Numeric.LinearAlgebra.HMatrix as M
import System.Random

-- web interface and json
import Network.Wreq
import Control.Lens

import Data.Aeson
import Data.Aeson.Lens

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Internal as B
-- TODO: tidy this up, will be using Aeson for parsing json later :)


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
-- note that this uses Laguerre's method
-- >>> zeros
-- [0.5 :+ 0.0,(-0.2) :+ 0.0]


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
steadyState2 = linearSolve system target
-- >>> steadyState2
-- Just (3><1)
--  [ 0.4375
--  , 0.3125
--  ,   0.25 ]


-- route 3:
evals :: Vector (Complex Double)
evecs :: Matrix (Complex Double)
(evals, evecs) = eig (M.tr transition)

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

wToInt :: Weather -> Int
wToInt Fine   = 0
wToInt Cloudy = 1
wToInt Rain   = 2

getNext :: Weather -> Int -> Weather
getNext w n
  | n == 0    = head $ (genpop transition . wToInt) w
  | otherwise = head $ drop (n-1) (take n $ (genpop transition . wToInt) w)

-- our pattern now becomes like:
-- getNext 3 (getNext 1 (getNext 4 (getNext 8 Fine)))

-- because we are now dealing with random numbers we have to move to IO town
steadyState4 :: Int -> Weather -> IO [Double]
steadyState4 n start = do
  g <- newStdGen
  let xs = take n $ randomRs (0, 9) g
  let states = init $ scanl getNext start xs
  let scores = foldl increment [0,0,0] states
  let scores' = [(fromIntegral x)/(fromIntegral n) | x <- scores]
  return scores'
-- incorrect, result is about [0.54, 0.32, 0.14] as of now
-- FIXME: come back to it later
-- >>> steadyState4 1000000 Cloudy
-- [0.535009,0.321368,0.143623]
-- notice how we only need to go into IO when we are using the random numbers
-- and that the rest of this can be written in terms of pure functions. The
-- implementation here is not even close to optimal, we'll worry about making
-- things working in a better way later on


-- web interface, json and string parsing ---------------------------------------
url :: String
url = "https://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt"

query :: IO (Maybe B.ByteString)
query = do
    r <- getWith defaults url
    return $ r ^? responseBody

