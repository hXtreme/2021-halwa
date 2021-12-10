{-
A Module for a Relational Algebra Machine Constants
-}

module RAM.Constant where

data Constant
  = Symbol Int
  | Integer Int
  | Boolean Bool
  | String String
  deriving (Eq, Show)