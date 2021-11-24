module Common (Type, BinOP, UnOP) where

data Type
  = Symbol
  | Integer
  | Boolean
  | String String
  deriving (Eq, Show)

data BinOP
  = Eq
  | Ne
  | Lt
  | Leq
  | Gt
  | Geq
  | And
  | Or
  | Add
  | Sub
  | Mult
  | Div
  deriving (Eq, Show)

data UnOP
  = Not
  | Pos
  | Neg
  deriving (Eq, Show)