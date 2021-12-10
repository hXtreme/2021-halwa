{-
A Module for a Relational Algebra Machine's Fact type.
-}

module RAM.Fact (Fact (..)) where

import RAM.Constant (Constant)

data Fact = Fact
  { predicate :: String,
    args :: [Constant]
  }
  deriving (Eq, Show)
