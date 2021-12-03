module Datalog.Constraint where

import Common (BinOP, UnOP)
import Datalog.Argument (Argument)
import PrettyPrinter

data Constraint
  = BinaryConstraint BinOP Argument Argument
  | UnaryConstraint UnOP Argument
  deriving (Eq, Show)

instance Pretty Constraint where
  prettyPrint (BinaryConstraint bo a1 a2) = (show bo) ++ "(" ++ (prettyPrint a1) ++ ", " ++ (prettyPrint a2) ++ ")"
  prettyPrint (UnaryConstraint uo a) = (show uo) ++ "(" ++ prettyPrint a ++ ")"
