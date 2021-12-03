module Datalog.Constant where

import PrettyPrinter

data Constant
    = Symbol String
    | Boolean Bool
    | Integer Int
    | SymbolId Int
    deriving (Eq, Show)

instance Pretty Constant where
  prettyPrint (Symbol s) = s
  prettyPrint (Boolean b) = show b
  prettyPrint (Integer i) = show i
  prettyPrint (SymbolId i) = show i