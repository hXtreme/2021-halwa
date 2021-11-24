module Datalog.Query where

import Datalog.Atom (Atom)

newtype Query = Query {atom :: Atom} deriving (Eq, Show)