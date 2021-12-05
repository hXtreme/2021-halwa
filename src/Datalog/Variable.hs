module Datalog.Variable where

import Parseable (Parseable (parser))
import qualified Parser as P
import qualified Datalog.Common as DL.Common
newtype Variable = Variable { name :: String }
  deriving (Eq, Show)

instance Parseable Variable where
  parser = Variable <$> titleCaseName
    where
      titleCaseName = P.filter DL.Common.isNotKeyWord $ P.wsP P.titleCaseWord