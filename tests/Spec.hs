module Main (main) where

import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

import           OrderBook.Types
import           OrderBook.Book
import           OrderBook.Matching
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "OrderBook"
  [ QC.testProperty "inserting orders preserves non-negative volume" prop_nonNegativeVolume
  , QC.testProperty "no crossed book after random inserts"          prop_noCrossedBook
  , QC.testProperty "deterministic replay"                          prop_deterministic
  , QC.testProperty "unit: partial fill"                            prop_unit_partialFill
  , QC.testProperty "unit: multi-level match"                       prop_unit_multiLevelMatch
  ]

instance Arbitrary Side where
  arbitrary = elements [Bid, Ask]

instance Arbitrary Price where
  arbitrary = Price <$> arbitrarySizedNatural

instance Arbitrary Quantity where
  arbitrary = Quantity . (+1) <$> arbitrarySizedNatural  -- strictly positive

instance Arbitrary OrderId where
  arbitrary = OrderId <$> arbitrarySizedNatural

instance Arbitrary LimitOrder where
  arbitrary = LimitOrder <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_nonNegativeVolume :: [LimitOrder] -> Bool
prop_nonNegativeVolume os =
  let finalBook = foldr insertLimitOrder emptyBook os
      Quantity vBid = totalVolume Bid finalBook
      Quantity vAsk = totalVolume Ask finalBook
  in vBid >= 0 && vAsk >= 0

prop_noCrossedBook :: [LimitOrder] -> Bool
-- xtx: This only tests `insertLimitOrder`, not `submitLimitOrder` (the matching engine).
-- A broken matching engine could leave crossed orders that this test would miss because it only inserts passively.
-- This property should use `foldl match ...` to be meaningful.
prop_noCrossedBook os =
  let finalBook = foldr insertLimitOrder emptyBook os
  in case (bestBid finalBook, bestAsk finalBook) of
       (Just b, Just a) -> b < a
       _                -> True

prop_deterministic :: [LimitOrder] -> Bool
prop_deterministic os =
  let b1 = foldr insertLimitOrder emptyBook os
      b2 = foldr insertLimitOrder emptyBook os
  in b1 == b2

-- Unit test: Buy 10 @ 100 vs Sell 20 @ 100 -> Partial fill of Sell, Buy fully filled
prop_unit_partialFill :: Bool
prop_unit_partialFill =
  let sellOrder = LimitOrder (OrderId 1) Ask (Price 100) (Quantity 20)
      buyOrder  = LimitOrder (OrderId 2) Bid (Price 100) (Quantity 10)
      book0     = insertLimitOrder sellOrder emptyBook
      (book1, events) = submitLimitOrder buyOrder book0
      
      -- Expectation: 
      -- 1. Buy fully filled
      -- 2. Sell partially filled (remaining 10)
      -- 3. Book has Sell 10 @ 100
      
      qAsk = totalVolume Ask book1 -- Should be 10
      qBid = totalVolume Bid book1 -- Should be 0
      
      eventsCorrect = case events of
        [Trade _ _ _ (Quantity 10), OrderFullyFilled _] -> True
        _ -> False
        
  in unQuantity qAsk == 10 && unQuantity qBid == 0 && eventsCorrect

-- Unit test: Buy 20 @ 100 vs Sell 10 @ 99, Sell 10 @ 100
prop_unit_multiLevelMatch :: Bool
prop_unit_multiLevelMatch =
  let sell1 = LimitOrder (OrderId 1) Ask (Price 99) (Quantity 10)
      sell2 = LimitOrder (OrderId 2) Ask (Price 100) (Quantity 10)
      buy   = LimitOrder (OrderId 3) Bid (Price 100) (Quantity 20)
      
      book0 = insertLimitOrder sell2 (insertLimitOrder sell1 emptyBook)
      (book1, events) = submitLimitOrder buy book0
      
      qAsk = totalVolume Ask book1 -- Should be 0
      qBid = totalVolume Bid book1 -- Should be 0
      
      -- Events should show 2 trades
      eventCount = length [ e | Trade {} <- events ]
      
  in unQuantity qAsk == 0 && unQuantity qBid == 0 && eventCount == 2
