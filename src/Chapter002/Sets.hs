-- | Working with sets, chapter 2.2

module Chapter002.Sets where

import Data.Set (Set)
import qualified Data.Set as Set

setA :: Set Int
setA = Set.fromList [2,7,2,3]

setB :: Set Int
setB = Set.fromList [1..6]

unionAB :: Set Int
unionAB = setA `Set.union` setB

intersectAB :: Set Int
intersectAB = setA `Set.intersection` setB

diffAB :: Set Int
diffAB = setA Set.\\ setB

symdiffAB :: Set Int
symdiffAB = (setA `Set.union` setB) Set.\\ (setA `Set.intersection` setB)

elemA6 :: Bool
elemA6 = 6 `Set.member` setA

subsets :: (Bool, Bool)
subsets = (symdiffAB `Set.isSubsetOf` unionAB, intersectAB `Set.isSubsetOf` unionAB)


-- TODO: build MCestimates M-g M-g 69
