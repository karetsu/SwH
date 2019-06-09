-- | classic bubblesort but in haskell
-- | funSort is a version which doesn't require calculating the length (for fun)

module List.BubbleSort (bubbleSort, funSort) where

-- first create a function which knows how to sort through the list once
singleSort :: Ord a => [a] -> [a]
singleSort (x:x':xs)
  | x' < x    = x':singleSort (x :xs)
  | otherwise = x :singleSort (x':xs)
singleSort x = x

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

noLength :: (Ord a, Eq b) => [a] -> [b] -> [a]
noLength xs []     = xs
noLength xs (_:is) = noLength (singleSort xs) is

funSort :: (Ord a, Eq a) => [a] -> [a]
funSort xs = noLength xs xs
