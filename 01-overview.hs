{-
NOTE: Throughout this entire codebase I will be making use of Dante's REPLoid
      functionality. This is the emacs plugin I use under hlissner's doom emacs
      and you can find out more about it at: https://github.com/jyp/dante.

      All code which looks like: `-- >>> [snippet]` is a REPLoid, this line is
      executed and its output is placed in a new comment below it. Doing it this
      way lets me avoid all kinds of mess around IO
-}
module Introduction where

import Chapter001.BubbleSort
import Chapter001.Roots
import Chapter001.MarkovChain

intro :: IO ()
intro = undefined
