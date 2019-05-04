{-
NOTE: Throughout this entire codebase I will be making use of Dante's REPLoid
      functionality. This is the emacs plugin I use under hlissner's doom emacs
      and you can find out more about it at: https://github.com/jyp/dante.

      All code which looks like: `-- >>> [snippet]` is a REPLoid, this line is
      executed and its output is placed in a new comment below it. Doing it this
      way lets me avoid all kinds of mess around IO
-}
module Main where

import Chapter001.BubbleSort
import Chapter001.Roots
import Chapter001.MarkovChain

lineBreak :: String
lineBreak = concat ["#" | _ <- [1..80]]


-- | Introduction to haskell
chapter001 :: IO ()
chapter001 = do
  putStrLn lineBreak
  putStr "Bubble sorting [1,7,34,6,5,3,67,8,4,10]: "
  print $ bubbleSort ([1,7,34,6,5,3,67,8,4,10]::[Int])
  putStr "Roots of 1 + 3x - 10x^2: "
  print zeros
  putStrLn lineBreak
  putStrLn "Markov chain:"
  putStr   "  -- approach 1:"
  print steadyState1
  putStr   "  -- approach 2:"
  print steadyState2
  putStr   "  -- approach 3:"
  print steadyState3
  putStrLn   "  -- approach 4:"
  s <- steadyState4
  print s
  putStrLn lineBreak

-- TODO: finish this section


--------------------------------------------------------------------------------
main :: IO ()
main = chapter001
