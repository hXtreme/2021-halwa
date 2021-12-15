module FrogEngine.Lib
  ( prettyResults,
    exec,
  )
where

import Control.Monad.State (State)
import Data.List (intercalate)
import qualified Data.Map as Map
import Datalog.Fact (Fact)
import Datalog.Variable (Variable)
import FrogEngine.Memory (Memory)
import FrogEngine.RAM (RAM (RAM), step)
import qualified FrogEngine.RAM as RAM
import FrogEngine.Rule (Rule)
import FrogEngine.Variable (allKnownFacts)
import Pretty (pretty)

stepN :: Int -> [Rule] -> State Memory [Rule]
stepN 0 rules = return rules
stepN n rules = do
  step rules
  stepN (n - 1) rules

translateResults :: Memory -> [Fact]
translateResults = concat . Map.elems . Map.map allKnownFacts

prettyResults :: Memory -> String
prettyResults = intercalate "\n" . map (\f -> pretty f ++ ".") . translateResults

exec = RAM.exec
