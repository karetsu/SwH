-- |

module Chapter001.Roots (zeros) where

import Data.Complex
import Polynomial.Roots

-- roots of a polynomial --------------------------------------------------------
-- no dealing with the complex problem of calculating roots, we will use
-- the Polynomial.Roots module of the `dsp` package, complex roots will look a
-- bit 'odd' to many as the output format is x :+ y for x + iy
zeros :: RealFloat a => [Complex a]
zeros = roots 1e-16 1000 [1,3,-10]
-- note that this uses Laguerre's method
-- >>> zeros
-- [0.5 :+ 0.0,(-0.2) :+ 0.0]
