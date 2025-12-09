module Main (main) where

import           Criterion.Main
import           System.Random

import           OrderBook.Types
import           OrderBook.Book
import           OrderBook.Matching

main :: IO ()
main = do
  -- Prepare a deterministic random generator for reproducibility.
  let gen = mkStdGen 42
      orders = take 10000 (randomOrders gen)
  defaultMain
    [ bench "insert 10k resting orders" $
        nf (foldr insertLimitOrder emptyBook) orders
    , bench "submit 1k orders into prefilled book" $
        let book = foldr insertLimitOrder emptyBook (take 5000 orders)
            incoming = take 1000 (drop 5000 orders)
        in nf (runSubmissions incoming) book
    ]

runSubmissions :: [LimitOrder] -> OrderBook -> OrderBook
runSubmissions os ob0 =
  fst $ foldl (\(ob, _) o -> submitLimitOrder o ob) (ob0, []) os

randomOrders :: StdGen -> [LimitOrder]
randomOrders gen =
  [ LimitOrder
      (OrderId i)
      (if b then Bid else Ask)
      (Price (1 + p `mod` 100))
      (Quantity (1 + q `mod` 1000))
  | (i, (b, (p, q))) <- zip [1..] (triples gen)
  ]

triples :: StdGen -> [(Bool, (Int, Int))]
triples g =
  let (b, g1)  = random g
      (p, g2)  = random g1
      (q, g3)  = random g2
  in (b, (p, q)) : triples g3
