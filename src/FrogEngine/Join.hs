module FrogEngine.Join
  ( fromJoin,
    fromAntiJoin,
    fromMap,
    mergeSortJoin,
  )
where

import FrogEngine.Variable
  ( FactLiterals,
    Relation (elements),
    Variable (recent, stable),
    insertIntoVariable,
    newRelation,
  )
import Data.Sort (sortOn)

type KeyFn v = FactLiterals -> v

fromJoin ::
  Ord v =>
  Variable ->
  (FactLiterals -> FactLiterals -> FactLiterals) ->
  KeyFn v ->
  KeyFn v ->
  Variable ->
  Variable ->
  Variable
fromJoin = joinInto

fromAntiJoin ::
  Ord v =>
  Variable ->
  (FactLiterals -> FactLiterals) ->
  KeyFn v ->
  KeyFn v ->
  Variable ->
  Relation ->
  Variable
fromAntiJoin = antiJoinInto

fromMap ::
  Variable ->
  (FactLiterals -> FactLiterals) ->
  Variable ->
  Variable
fromMap = mapInto

-- -- p(X, Y) = q(X, Y), r(X, Y).
-- -- Test: Monotonic steps.
joinInto ::
  Ord v =>
  Variable -> -- p
  (FactLiterals -> FactLiterals -> FactLiterals) ->
  KeyFn v ->
  KeyFn v ->
  Variable -> -- q
  Variable -> -- r
  Variable -- p'
joinInto target logic k1 k2 input1 input2 = insertIntoVariable target . newRelation $ results
  where
    recent1 = elements . recent $ input1
    recent2 = elements . recent $ input2
    stable1 = elements . stable $ input1
    stable2 = elements . stable $ input2
    fromStable2 = mergeSortJoin logic k1 k2 recent1 stable2
    fromStable1 = mergeSortJoin logic k1 k2 stable1 recent2
    fromRecent = mergeSortJoin logic k1 k2 recent1 recent2
    results = fromStable2 <> fromStable1 <> fromRecent

antiJoinInto ::
  Ord v =>
  Variable ->
  (FactLiterals -> FactLiterals) ->
  KeyFn v ->
  KeyFn v ->
  Variable ->
  Relation ->
  Variable
antiJoinInto target logic k1 k2 input1 input2 = insertIntoVariable target . newRelation $ results
  where
    r [] _ = []
    r xs [] = xs
    r (x : xs) (y : ys)
      | k1 x < k2 y = x : r xs (y : ys)
      | k1 x == k2 y = r xs (y : ys)
      | k1 x > k2 y = r (x : xs) ys
    results = map logic $ r (elements . recent $ input1) (elements input2)

-- | Merge - Sort - Join (Try SortedList)
mergeSortJoin ::
  (Ord v) =>
  (FactLiterals -> FactLiterals -> FactLiterals) ->
  KeyFn v ->
  KeyFn v ->
  [FactLiterals] ->
  [FactLiterals] ->
  [FactLiterals]
mergeSortJoin logic k1 k2 input1 input2 = merge input1' input2'
  where
    input1' = sortOn k1 input1
    input2' = sortOn k2 input2
    merge [] ys = []
    merge xs [] = []
    merge (x : xs) (y : ys)
      | k1 x < k2 y = merge xs (y : ys)
      | k1 x == k2 y = logic x y : merge (x : xs) ys
      | k1 x > k2 y = merge (x : xs) ys

mapInto :: Variable -> (FactLiterals -> FactLiterals) -> Variable -> Variable
mapInto target logic = insertIntoVariable target . newRelation . map logic . elements . recent
