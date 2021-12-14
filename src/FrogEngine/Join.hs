module FrogEngine.Join
  ( fromJoin,
    fromAntiJoin,
    fromMap,
  )
where

import FrogEngine.Variable
  ( Relation (elements),
    Variable (recent, stable),
    insertIntoVariable,
    newRelation,
  )

fromJoin ::
  (Ord k, Ord v1, Ord v2, Ord v) =>
  Variable k v ->
  Variable k v1 ->
  Variable k v2 ->
  (k -> v1 -> v2 -> v) ->
  Variable k v
fromJoin = joinInto

fromAntiJoin ::
  (Ord k, Ord v) =>
  Variable k v ->
  Variable k v ->
  Relation k v' ->
  (k -> v -> v) ->
  Variable k v
fromAntiJoin = antiJoinInto

fromMap ::
  (Ord k, Ord v, Ord v') =>
  Variable k v ->
  Variable k v' ->
  (k -> v' -> v) ->
  Variable k v
fromMap = mapInto

-- p(X, Y) = q(X, Y), r(X, Y).
-- Test: Monotonic steps.
joinInto ::
  (Ord k, Ord v1, Ord v2, Ord v) =>
  Variable k v -> -- p
  Variable k v1 -> -- q
  Variable k v2 -> -- r
  (k -> v1 -> v2 -> v) ->
  Variable k v -- p'
joinInto target input1 input2 logic = insertIntoVariable target . newRelation $ results
  where
    recent1 = elements . recent $ input1
    recent2 = elements . recent $ input2
    fromStable2 = foldMap (mergeSortJoin logic recent1 . elements) (stable input2)
    fromStable1 = foldMap (mergeSortJoin' logic recent2 . elements) (stable input1)
    fromRecent = mergeSortJoin logic recent1 recent2
    results = fromStable2 <> fromStable1 <> fromRecent

antiJoinInto ::
  (Ord k, Ord v) =>
  Variable k v ->
  Variable k v1 ->
  Relation k v2 ->
  (k -> v1 -> v) ->
  Variable k v
antiJoinInto target input1 input2 logic = insertIntoVariable target . newRelation $ results
  where
    r [] _ = []
    r xs [] = xs
    r (x@(kx, vx) : xs) (y@(ky, _) : ys)
      | kx < ky = x : r xs (y : ys)
      | kx == ky = r xs (y : ys)
      | kx > ky = r (x : xs) ys
    results = map (uncurry mapFn) $ r (elements . recent $ input1) (elements input2)
    mapFn k v = (k, logic k v)

-- | Merge - Sort - Join (Try SortedList)
mergeSortJoin :: (Ord k) => (k -> v1 -> v2 -> v) -> [(k, v1)] -> [(k, v2)] -> [(k, v)]
mergeSortJoin logic = merge
  where
    merge [] ys = []
    merge xs [] = []
    merge (x@(kx, vx) : xs) (y@(ky, vy) : ys)
      | kx < ky = merge xs (y : ys)
      | kx == ky = (kx, logic kx vx vy) : merge (x : xs) ys
      | kx > ky = merge (x : xs) ys

mergeSortJoin' :: Ord k => (k -> v1 -> v2 -> v) -> [(k, v2)] -> [(k, v1)] -> [(k, v)]
mergeSortJoin' logic input1 input2 = mergeSortJoin logic input2 input1

mapInto :: (Ord k, Ord v) => Variable k v -> Variable k t -> (k -> t -> v) -> Variable k v
mapInto target input logic = insertIntoVariable target . newRelation $ results
  where
    results = map (uncurry mapFn) $ elements . recent $ input
    mapFn k v = (k, logic k v)
