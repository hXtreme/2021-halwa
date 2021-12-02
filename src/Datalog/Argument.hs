module Datalog.Argument where

import Datalog.Constant (Constant)
import Datalog.Variable (Variable)
import PrettyPrinter

data Argument
    =  Wildcard
    | Constant Constant
    | Variable Variable
    deriving (Eq, Show)


instance Pretty Argument where
  prettyPrint d = "test"