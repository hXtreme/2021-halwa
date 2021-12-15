module Path where

import qualified Data.Map as Map
import qualified Datalog.Constant as DL.Constant
import FrogEngine.RAM (RAM (RAM))
import FrogEngine.Rule (Rule (Join, Proj))
import FrogEngine.Variable (Variable (name), emptyRelation, newRelation, newVariable)

[towne, levine, moore] = map DL.Constant.Symbol ["Towne", "Levine", "Moore"]

paths :: RAM
paths = RAM mem rules queries
  where
    rules = [ruleDirectPath, ruleIndirectPath]
    ruleDirectPath = Proj "path" "edge" [0, 1]
    ruleIndirectPath = Join "path" [2, 1] "path" [0] "edge" [1]
    queries = ["path"]
    mem = Map.fromList . map (\v -> (name v, v)) $ vars
    vars = [varPath, varEdge]
    varPath = newVariable "path" emptyRelation
    varEdge = newVariable "edge" relationEdge
    relationEdge =
      newRelation
        [ [towne, levine],
          [levine, moore]
        ]
