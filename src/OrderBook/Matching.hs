module OrderBook.Matching
  ( submitLimitOrder
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Sequence   as S

import           OrderBook.Types
import           OrderBook.Book  (insertLimitOrder)

-- | Submit a limit order: it will try to match against the opposite side,
-- and return the new book and a list of match events.
submitLimitOrder :: LimitOrder -> OrderBook -> (OrderBook, [MatchEvent])
submitLimitOrder o ob =
  case loSide o of
    Bid -> matchAgainstAsks o ob
    Ask -> matchAgainstBids o ob

-- Naive matching: walk through best prices until price constraint fails
-- or the order is fully filled.

matchAgainstAsks :: LimitOrder -> OrderBook -> (OrderBook, [MatchEvent])
matchAgainstAsks incoming ob@(OrderBook bids asks) =
  case bestAskPriceLevel asks of
    Nothing -> (insertLimitOrder incoming ob, [])
    Just (bestP, queue)
      | bestP > loPrice incoming ->
          -- Can't cross the spread; insert as resting bid.
          (insertLimitOrder incoming ob, [])
      | otherwise ->
          -- Match against best ask level
          let (remaining, events, newQueue) =
                consumeLevel incoming queue []
          in case remaining of
               Nothing ->
                 -- Incoming fully filled; remove / update price level and return
                 let newAsks = updateAsks bestP newQueue asks
                 in (OrderBook bids newAsks, events)
               Just remOrder ->
                 -- Price level exhausted, continue with next ask
                 let newAsks = updateAsks bestP newQueue asks
                     (finalBook, moreEvents) =
                       matchAgainstAsks remOrder (OrderBook bids newAsks)
                 in (finalBook, events ++ moreEvents)

matchAgainstBids :: LimitOrder -> OrderBook -> (OrderBook, [MatchEvent])
matchAgainstBids incoming ob@(OrderBook bids asks) =
  case bestBidPriceLevel bids of
    Nothing -> (insertLimitOrder incoming ob, [])
    Just (bestP, queue)
      | bestP < loPrice incoming ->
          (insertLimitOrder incoming ob, [])
      | otherwise ->
          let (remaining, events, newQueue) =
                consumeLevel incoming queue []
          in case remaining of
               Nothing ->
                 let newBids = updateBids bestP newQueue bids
                 in (OrderBook newBids asks, events)
               Just remOrder ->
                 let newBids = updateBids bestP newQueue bids
                     (finalBook, moreEvents) =
                       matchAgainstBids remOrder (OrderBook newBids asks)
                 in (finalBook, events ++ moreEvents)

-- Util: get best price level and queue

bestAskPriceLevel :: BookSide -> Maybe (Price, S.Seq LimitOrder)
bestAskPriceLevel (BookSide m) =
  fmap (\(p, q) -> (p, q)) (M.lookupMin m)

bestBidPriceLevel :: BookSide -> Maybe (Price, S.Seq LimitOrder)
bestBidPriceLevel (BookSide m) =
  fmap (\(p, q) -> (p, q)) (M.lookupMax m)

-- Consume a single price level queue with an incoming order.
-- Returns: (maybe remaining incoming order, events, new queue)
consumeLevel
  :: LimitOrder
  -> S.Seq LimitOrder
  -> [MatchEvent]
  -> (Maybe LimitOrder, [MatchEvent], S.Seq LimitOrder)
consumeLevel incoming queue accEvents =
  case S.viewl queue of
    S.EmptyL -> (Just incoming, accEvents, S.empty)
    resting S.:< rest ->
      let Quantity qIn  = loQuantity incoming
          Quantity qRes = loQuantity resting
      in if qIn < qRes
           then
             -- Partial fill of resting order
             let tradeQty   = Quantity qIn
                 remainingR = resting { loQuantity = Quantity (qRes - qIn) }
                 ev         = Trade (loId incoming) (loId resting)
                                     (loPrice resting) tradeQty
             in ( Nothing
                , accEvents ++ [ev, OrderFullyFilled (loId incoming)]
                , remainingR S.<| rest
                )
           else if qIn == qRes
           then
             let tradeQty = Quantity qIn
                 ev       = Trade (loId incoming) (loId resting)
                                   (loPrice resting) tradeQty
             in ( Nothing
                , accEvents
                    ++ [ ev
                       , OrderFullyFilled (loId incoming)
                       , OrderFullyFilled (loId resting)
                       ]
                , rest
                )
           else
             -- qIn > qRes: resting fully filled, continue with new incoming
             let tradeQty    = Quantity qRes
                 remainingIn = incoming { loQuantity = Quantity (qIn - qRes) }
                 ev          = Trade (loId incoming) (loId resting)
                                      (loPrice resting) tradeQty
             in consumeLevel remainingIn rest
                   (accEvents
                      ++ [ ev
                         , OrderFullyFilled (loId resting)
                         ])

-- Update maps after matching
updateAsks :: Price -> S.Seq LimitOrder -> BookSide -> BookSide
updateAsks p q (BookSide m) =
  BookSide $
    if S.null q then M.delete p m else M.insert p q m

updateBids :: Price -> S.Seq LimitOrder -> BookSide -> BookSide
updateBids p q (BookSide m) =
  BookSide $
    if S.null q then M.delete p m else M.insert p q m
