module Main ( main ) where

-- This executable runs some heavy numerical computation and showcases
-- the screenie monitoring library.

import Control.Concurrent.Async
import Control.Exception ( evaluate )
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Traversable

import Screenie

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = go 1 1 (n-2)
 where
  go p x 0 = p+x
  go p x n = go x (p+x) (n-1)

computeFibonacci :: IO ()
computeFibonacci = withMonitoring "Computing fibonacci series" $ do
  _s <- fmap sum $ for [1..10000] $ \idx -> do
    value <- evaluate $ fib idx
    updateMonitoringProgress ("Computing fibonacci series: " <> show idx) (fromIntegral idx/10000)
    return value
  return ()

main :: IO ()
main = do
  fibonacci_calculators <- replicateM 20 $ async computeFibonacci
  traverse_ wait fibonacci_calculators

