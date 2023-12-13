module Main where

import Control.Monad (forM_)
import DB (initConnectionPool, runSessionWith)
import Faker (generateNonDeterministic)
import Faker.Combinators (listOf)
import GHC.Float (int2Float)
import Hasql.Pool qualified
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
import User (createNewUser)
import User.Fake (fakeNewUser)

main :: IO ()
main = do
  putStrLn "Generating users..."
  let count = 10_000
  fakeUsers <- generateNonDeterministic $ listOf count fakeNewUser
  putStrLn "Done generating."
  putStrLn "Adding them to the DB..."
  let dbConnStr = "host=127.0.0.1"
  pool <- initConnectionPool dbConnStr
  forWithProgress fakeUsers $ runHasql pool . createNewUser

runHasql :: Hasql.Pool.Pool -> Transaction result -> IO result
runHasql pool sql =
  runSessionWith pool $ transaction ReadCommitted Write sql

forWithProgress :: [a] -> (a -> IO b) -> IO ()
forWithProgress xs f = forM_ (zip [1 :: Int ..] xs) work
  where
    total = length xs
    work (i, x) = do
      _ <- f x
      let percentage = round (int2Float i / int2Float total * 100) :: Int
      putStr $ "\r" <> show i <> "/" <> show total <> " (" <> show percentage <> "%)"
