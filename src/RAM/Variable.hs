{-
A Module for a Relational Algebra Machine's Variables.
-}

module RAM.Variable
  ( Variable (..),
    VarType (..),
  )
where

import Common (Type)

data Variable = Variable
  { name :: String,
    isTemp :: Bool,
    varType :: VarType
  }
  deriving (Eq, Show)

data VarType
  = Empty
  | Base Type
  | Tuple [VarType]
  deriving (Eq, Show)