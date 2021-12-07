module Datalog.Variable where

import qualified Datalog.Common as DL.Common
import Parseable (Parseable (parser))
import qualified Parser as P
import Pretty (Pretty (pretty))

newtype Variable = Variable {name :: String}
  deriving (Eq, Show)

instance Parseable Variable where
  parser = Variable <$> titleCaseName
    where
      titleCaseName = P.filter DL.Common.isNotKeyWord $ P.wsP P.titleCaseWord

instance Pretty Variable where
  pretty = name
