-- | In this module we put all the functions that use unsafe math functions like `div` and `mod`,
-- but we use them in ways we know are safe, or safe enough for our needs.
module SafeMath where

import Data.Int (Int32)

-- | Safe because `Int`s in Haskell are at least 32 bits
-- Technically you can compile with any Int size, but we'll likely compile for 64 bit anyway
int32ToInt :: Int32 -> Int
int32ToInt = fromIntegral

monthsToYears :: Integral a => a -> a
monthsToYears m = m `div` 12

roundToNearest :: Int -> Int -> Int
roundToNearest 0 x = x
roundToNearest n x = (x + n `div` 2) `div` n * n
