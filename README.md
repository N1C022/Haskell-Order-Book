# haskell orderbook

A type safe limit order book implementation in Haskell.

The goal of this project is to model a central limit order book with:

- Strong types for prices, quantities, and order sides
- Invariants enforced by the type system where possible
- Pure functional core with property-based tests
- Benchmarks for matching performance

This kind of system is similar (in spirit) to the matching engines used by modern exchanges and trading firms.

## Features

- Typed `Price`, `Quantity`, `OrderId`
- Bid / Ask sides with separate books
- Price-time priority matching
- Insertion of new limit orders
- Market-style matching via order submission
- Property-based tests (QuickCheck) checking:
  - No crossed book invariant (best bid < best ask)
  - Order quantities are preserved during insertion
- Benchmarks using Criterion

## Building

### Using `cabal`

```bash
cabal build