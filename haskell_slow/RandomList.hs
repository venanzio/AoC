{- Various ways of generating random lists
   Used for testing
-}

module RandomList where

import System.Random
import Control.Monad

-- random list of non-negative numbers
--  n = length of list,  mx = maximum
randL :: Int -> Int -> IO [Int]
randL n mx = replicateM n (randomRIO (0,mx))
