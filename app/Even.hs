module Even where

import qualified Control.Monad.State as S
import Data.List ((!!))
import Data.Map ((!))
import qualified Datalog.Constant as DL.Constant
import FrogEngine.Join (fromJoin, mergeSortJoin)
import FrogEngine.Rule (Rule (Join))
import FrogEngine.Variable (Relation (elements), Variable (recent, name), newRelation, newVariable)
import FrogEngine.Memory (Memory)
import qualified Data.Map as Map
import FrogEngine.RAM (RAM(RAM))
import qualified FrogEngine.RAM as RAM
import FrogEngine.Lib (prettyResults)
import Pretty (NewLineSeparatedList(NSL), Pretty (pretty))

{-
odd(O) :- succ(E, O), even(E).
even(E) :- succ(O, E), odd(O).

query even.
query odd.
-}


evens :: Int -> RAM
evens base = RAM mem rules queries
  where
    nums = [DL.Constant.Integer n | n <- [0 .. base - 1]]
    rules = [ruleOdd, ruleEven]
    ruleOdd = Join "odd" [1] "succ" [0] "even" [0]
    ruleEven = Join "even" [1] "succ" [0] "odd" [0]
    queries = ["even", "odd"]
    mem = Map.fromList . map (\v -> (name v, v)) $ vars
    relationNum = newRelation . map (:[]) $ nums
    relationSucc = newRelation rels
      where
        rels = flat $ zip nums $ tail nums
        flat = map (\(x, y) -> [x, y])
    relationEven = newRelation [[DL.Constant.Integer 0]]
    varNum = newVariable "num" relationNum
    varSucc = newVariable "succ" relationSucc
    varOdd = newVariable "odd" (newRelation [])
    varEven = newVariable "even" relationEven
    vars = [varNum, varSucc, varOdd, varEven]

evens5 :: RAM
evens5 = evens 5

-- >>> prettyResults . RAM.exec $ evens5
-- "even(0).\neven(2).\neven(4).\nodd(1).\nodd(3)."
