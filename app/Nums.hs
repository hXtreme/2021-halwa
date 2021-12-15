module Nums where

import qualified Control.Monad.State as S
import Data.List ((!!))
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Datalog.Constant as DL.Constant
import FrogEngine.Join (fromJoin, mergeSortJoin)
import FrogEngine.Lib (prettyResults)
import FrogEngine.Memory (Memory)
import FrogEngine.RAM (RAM (RAM))
import qualified FrogEngine.RAM as RAM
import FrogEngine.Rule (Rule (Join, Proj))
import FrogEngine.Variable (Relation (elements), Variable (name, recent), newRelation, newVariable)
import Pretty (NewLineSeparatedList (NSL), Pretty (pretty))

{-
odd(O) :- succ(E, O), even(E).
even(E) :- succ(O, E), odd(O).

nums'(N) :- even(N).
nums'(N) :- odd(N).

query even.
query odd.
-}

nums' :: Int -> RAM
nums' base = RAM mem rules queries
  where
    mem = Map.fromList . map (\v -> (name v, v)) $ [varNum, varSucc, varOdd, varEven, varNums']
    rules = [ruleOdd, ruleEven, ruleNums'1, ruleNums'2]
    queries = ["nums'"]
    nums = [DL.Constant.Integer n | n <- [0 .. base - 1]]
    ruleOdd = Join "odd" [1] "succ" [0] "even" [0]
    ruleEven = Join "even" [1] "succ" [0] "odd" [0]
    ruleNums'1 = Proj "nums'" "even" [0]
    ruleNums'2 = Proj "nums'" "odd" [0]
    relationNum = newRelation . map (: []) $ nums
    relationSucc = newRelation rels
      where
        rels = flat $ zip nums $ tail nums
        flat = map (\(x, y) -> [x, y])
    relationEven = newRelation [[DL.Constant.Integer 0]]
    varNum = newVariable "num" relationNum
    varSucc = newVariable "succ" relationSucc
    varOdd = newVariable "odd" (newRelation [])
    varEven = newVariable "even" relationEven
    varNums' = newVariable "nums'" (newRelation [])

nums5 :: RAM
nums5 = nums' 5

-- >>> prettyResults . RAM.exec $ nums5
-- "nums'(0).\nnums'(1).\nnums'(2).\nnums'(3).\nnums'(4)."
