module OrderBook.Book
  ( emptyBook
  , insertLimitOrder
  , bestBid
  , bestAsk
  , totalVolume
  ) where

import           Data.Foldable    (foldl', toList)
import qualified Data.Map.Strict  as M
import qualified Data.Sequence    as S

import           OrderBook.Types

-- | An empty order book.
emptyBook :: OrderBook
emptyBook = OrderBook (BookSide M.empty) (BookSide M.empty)

-- | Insert a limit order into the book without matching.
-- This is a pure operation that preserves price-time priority.
insertLimitOrder :: LimitOrder -> OrderBook -> OrderBook
insertLimitOrder o ob =
  case loSide o of
    Bid -> ob { obBids = insertIntoSide o (obBids ob) }
    Ask -> ob { obAsks = insertIntoSide o (obAsks ob) }

insertIntoSide :: LimitOrder -> BookSide -> BookSide
insertIntoSide o (BookSide mp) =
  let p   = loPrice o
      seq = M.findWithDefault S.empty p mp
  in BookSide (M.insert p (seq S.|> o) mp)

-- | Best bid price, if any.
bestBid :: OrderBook -> Maybe Price
bestBid (OrderBook (BookSide bids) _) =
  if M.null bids then Nothing else Just (fst (M.findMax bids))

-- | Best ask price, if any.
bestAsk :: OrderBook -> Maybe Price
bestAsk (OrderBook _ (BookSide asks)) =
  if M.null asks then Nothing else Just (fst (M.findMin asks))

-- | Total volume on a given side of the book.
totalVolume :: Side -> OrderBook -> Quantity
-- xtx: iterating the whole map is O(N).
-- For an orderbook, we need this to be O(1).
-- We should cache the total volume in the OrderBook record structure and update it incrementally.
totalVolume side (OrderBook bids asks) =
  let BookSide mp = case side of
        Bid -> bids
        Ask -> asks
  in Quantity . getSum $ foldl' acc 0 mp
 where
  acc :: Int -> S.Seq LimitOrder -> Int
  acc s seq =
    s + sum [ unQuantity (loQuantity o) | o <- toList seq ]

  getSum = id
