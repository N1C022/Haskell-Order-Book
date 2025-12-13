module Main where
import OrderBook.Types
import OrderBook.Book
import OrderBook.Matching
import System.Exit
import Control.Monad (when)

assert :: String -> Bool -> IO ()
assert name True = putStrLn $ "PASS: " ++ name
assert name False = do
  putStrLn $ "FAIL: " ++ name
  exitFailure

main :: IO ()
main = do
  putStrLn "Running Minimal Verifications..."

  -- 1. Partial Fill Verification
  let sell = LimitOrder (OrderId 1) Ask (Price 100) (Quantity 20)
  let buy  = LimitOrder (OrderId 2) Bid (Price 100) (Quantity 10)
  let (b1, evs1) = submitLimitOrder buy (insertLimitOrder sell emptyBook)
  let qAsk = totalVolume Ask b1
  assert "Partial Fill Volume Correct" (unQuantity qAsk == 10)
  -- Events: Trade (10), FullyFilled (Buy)
  assert "Partial Fill Events Volume" (length evs1 == 2) 

  -- 2. Multi Level Match
  let s1 = LimitOrder (OrderId 3) Ask (Price 99) (Quantity 10)
  let s2 = LimitOrder (OrderId 4) Ask (Price 100) (Quantity 10)
  let b_sweep = LimitOrder (OrderId 5) Bid (Price 100) (Quantity 20)
  let book_setup = insertLimitOrder s2 (insertLimitOrder s1 emptyBook)
  let (b2, evs2) = submitLimitOrder b_sweep book_setup
  assert "Multi Level Sweep Volume" (unQuantity (totalVolume Ask b2) == 0)
  assert "Multi Level Sweep Events" (length [() | Trade {} <- evs2] == 2)


  -- 3. Deterministic Replay (Light)
  let orders = [sell, buy, s1, s2, b_sweep]
  let replay1 = foldl (\(b,_) o -> submitLimitOrder o b) (emptyBook, []) orders
  let replay2 = foldl (\(b,_) o -> submitLimitOrder o b) (emptyBook, []) orders
  assert "Deterministic Replay" (fst replay1 == fst replay2)
  
  -- 4. Invariant Check (Crossed Book)
  -- After sweep, book should be empty
  assert "Book Empty After Sweep" (bestBid b2 == Nothing && bestAsk b2 == Nothing)


  putStrLn "All checks passed."
