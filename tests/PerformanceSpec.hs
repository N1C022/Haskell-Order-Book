module Main where

import           Data.Time.Clock
import           OrderBook.Types
import           OrderBook.Book
import           OrderBook.Matching
import           Text.Printf
import           Control.DeepSeq (deepseq) -- Might not be available? Defaults to simple seq if needed.

mkOrder :: Int -> LimitOrder
{- xtx: This generator creates orders that oscillate bid/ask at virtually the same price.
This keeps the book size very small (size ~1).
It doesn't benchmark the cost of inserting into a deep book (Log N map insertions).
-}
mkOrder i = LimitOrder (OrderId i)
                       (if even i then Bid else Ask)
                       (Price (100 + (if even i then 0 else 1))) 
                       (Quantity 10)

main :: IO ()
main = do
  let n = 500000
  let orders = [mkOrder i | i <- [1..n]]
  
  putStrLn $ "Processing " ++ show n ++ " orders..."
  
  start <- getCurrentTime
  let (finalBook, _) = foldl (\(b, _) o -> submitLimitOrder o b) (emptyBook, []) orders
  
  -- Force evaluation by printing result BEFORE capturing end time
  let v = totalVolume Bid finalBook
  -- We rely on print to force evaluation of v. 
  -- Note: totalVolume traverses the map so it forces the structure of the map. 
  -- It might not force the thunks inside the map if they are lazy, but book insert is strict in key? 
  -- uses Data.Map.Strict so valid.
  print v
  
  end <- getCurrentTime
  
  let diff = diffUTCTime end start
  let seconds = realToFrac diff :: Double
  let ops = fromIntegral n / seconds
  let latency = 1000000 / ops -- micros per op
  
  printf "Time: %.4fs\n" seconds
  printf "OPS: %.0f\n" ops
  printf "Latency: %.2f us\n" latency
