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
    -- Change "test" as it's a temporary approach
  prettyPrint Wildcard = "test"