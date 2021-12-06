module Datalog.Query where

import Datalog.Atom (Atom (args))
import Parseable (Parseable (parser))
import qualified Parser as P

newtype Query = Query {query :: Atom} deriving (Eq, Show)

instance Parseable Query where
  parser = Query <$> (keyWordQuery *> relation)
    where
      relation = P.filter (null . args) parser
      keyWordQuery = P.stringP "query"
