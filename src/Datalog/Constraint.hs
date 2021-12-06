module Datalog.Constraint where

import Common (BinOP, UnOP)
import Control.Applicative
import Datalog.Argument (Argument)
import Parseable (Parseable (parser))

data Constraint
  = BinaryConstraint Argument BinOP Argument
  | UnaryConstraint UnOP Argument
  deriving (Eq, Show)

instance Parseable Constraint where
  parser =
    BinaryConstraint <$> parser <*> parser <*> parser
      <|> UnaryConstraint <$> parser <*> parser