{-
A Module for a Relational Algebra Machine's Variables.
-}

module RAM.Variable where

import Common (Type)

data Variable = Variable
  { name :: String,
    -- isTemp :: Bool,
    varType :: VarType
  }
  deriving (Eq, Show)

data VarType
  = Empty
  | Simple Type
  | Complex [VarType]
  deriving (Eq, Show)