module Datalog.Query where

import Datalog.Atom (Atom (args, predicate))
import Parseable (Parseable (parser))
import qualified Parser as P
import Pretty (Pretty (pretty))

newtype Query = Query {query :: Atom} deriving (Eq, Show)

instance Parseable Query where
  parser = Query <$> (keyWordQuery *> relation)
    where
      relation = P.filter (null . args) parser
      keyWordQuery = P.stringP "query"

instance Pretty Query where
  pretty (Query atom) = "query " ++ predicate atom
