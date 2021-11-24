{-
A Module for a Relational Algebra Machine's Program Flow.
-}

module RAM.Flow (Flow) where

import qualified RAM.Constant as RC

data Argument
  = Element [String]
  | Tuple [Argument]
  | Constant RC.Constant
  deriving (Eq, Show)

data Flow
  = Product Flow Flow
  | Intersect Flow Flow
  | Join Flow Flow
  | Filter Flow Argument
  | Project Flow Argument
  | Find Flow RC.Constant
  | Chain Flow [RC.Constant] Flow
  | Variable String
  deriving (Eq, Show)
