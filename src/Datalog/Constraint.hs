module Datalog.Constraint where

import Common (BinOP, UnOP)
import Datalog.Argument (Argument)

data Constraint
  = BinaryConstraint BinOP Argument Argument
  | UnaryConstraint UnOP Argument
  deriving (Eq, Show)
