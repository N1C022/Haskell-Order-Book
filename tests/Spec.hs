module Main (main) where

import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

import           OrderBook.Types
import           OrderBook.Book
import           OrderBook.Matching

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "OrderBook"
  [ QC.testProperty "inserting orders preserves non-negative volume" prop_nonNegativeVolume
  , QC.testProperty "no crossed book after random inserts"          prop_noCrossedBook
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
prop_noCrossedBook os =
  let finalBook = foldr insertLimitOrder emptyBook os
  in case (bestBid finalBook, bestAsk finalBook) of
       (Just b, Just a) -> b < a
       _                -> True
