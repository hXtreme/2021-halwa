module Datalog.Constant where

data Constant
    = Symbol String
    | Boolean Bool
    | Integer Int
    | SymbolId Int
    deriving (Eq, Show)