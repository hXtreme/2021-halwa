module Datalog.Query where

import Datalog.Atom (Atom)
import Parseable (Parseable (parser))
import qualified Parser as P

newtype Query = Query {query :: Atom} deriving (Eq, Show)

instance Parseable Query where
  parser = Query <$> (keyWordQuery *> parser)
    where
      keyWordQuery = P.stringP "query"
