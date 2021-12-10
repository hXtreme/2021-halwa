{-
A Module for a Relational Algebra Machine Program.
-}

module RAM.Program (Program (..)) where

import RAM.Disjunction (Disjunction)
import RAM.Fact (Fact)
import RAM.Update (Update)
import RAM.Variable (Variable)

data Program = Program
  { variables :: [Variable],
    facts :: [Fact],
    disjuctions :: [Disjunction],
    updates :: [Update]
  }
  deriving (Eq, Show)
