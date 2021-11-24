{-
A Module for a Relational Algebra Machine Constants
-}

module RAM.Constant where

data Constant
  = Symbol String
  | Integer Int
  | Boolean Bool
  | String String
  deriving (Eq, Show)