{-
A Module for a Relational Algebra Machine's Disjunction type.
-}

module RAM.Disjunction where

import RAM.Fact (Fact)

data Disjunction = Disjunction
  { id :: String,
    facts :: [Fact]
  }
  deriving (Eq, Show)