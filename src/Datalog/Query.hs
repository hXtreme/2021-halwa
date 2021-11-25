module Datalog.Query where

import Datalog.Atom (Atom)

newtype Query = Query {query :: Atom} deriving (Eq, Show)