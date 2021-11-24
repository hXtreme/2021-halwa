module Datalog.Variable where

newtype Variable = Variable { name :: String }
  deriving (Eq, Show)