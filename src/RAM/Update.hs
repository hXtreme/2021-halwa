{-
A Module for a Relational Algebra Machine's Update type.
-}

module RAM.Update where

import RAM.Flow (Flow)

data Update = Update
  { target :: String,
    flow :: Flow
  }
  deriving (Eq, Show)