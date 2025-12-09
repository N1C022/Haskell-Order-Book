module OrderBook.Types
  ( Side(..)
  , Price(..)
  , Quantity(..)
  , OrderId(..)
  , LimitOrder(..)
  , OrderBook(..)
  , BookSide(..)
  , MatchEvent(..)
  ) where

import           Data.Map.Strict (Map)
import           Data.Sequence   (Seq)

-- | Side of the book: bid (buy) or ask (sell).
data Side = Bid | Ask
  deriving (Eq, Ord, Show)

-- | Price in integer ticks (e.g. cents or ticks).
newtype Price = Price { unPrice :: Int }
  deriving (Eq, Ord, Show)

-- | Quantity in units (e.g. shares).
newtype Quantity = Quantity { unQuantity :: Int }
  deriving (Eq, Ord, Show)

-- | Unique identifier for an order.
newtype OrderId = OrderId { unOrderId :: Int }
  deriving (Eq, Ord, Show)

-- | A simple limit order.
data LimitOrder = LimitOrder
  { loId        :: !OrderId
  , loSide      :: !Side
  , loPrice     :: !Price
  , loQuantity  :: !Quantity
  } deriving (Eq, Show)

-- | A book side: mapping from price levels to FIFO queues of orders.
-- Invariants:
--   * For bids: highest price is best
--   * For asks: lowest price is best
newtype BookSide = BookSide
  { bsLevels :: Map Price (Seq LimitOrder)
  } deriving (Eq, Show)

-- | Full order book with bids and asks.
data OrderBook = OrderBook
  { obBids :: !BookSide
  , obAsks :: !BookSide
  } deriving (Eq, Show)

-- | Events generated during matching.
data MatchEvent
  = Trade
      { meAggressor :: !OrderId   -- ^ Incoming order
      , meResting   :: !OrderId   -- ^ Existing book order
      , mePrice     :: !Price
      , meQuantity  :: !Quantity
      }
  | OrderFullyFilled !OrderId
  | OrderPartiallyFilled !OrderId !Quantity  -- ^ Remaining
  deriving (Eq, Show)
