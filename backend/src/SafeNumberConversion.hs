module SafeNumberConversion where

import Data.Int (Int32)

-- | Safe because `Int`s in Haskell are at least 32 bits
-- Technically you can compile with any Int size, but we'll likely compile for 64 bit anyway
int32ToInt :: Int32 -> Int
int32ToInt = fromIntegral

monthsToYears :: Integral a => a -> a
monthsToYears m = m `div` 12
