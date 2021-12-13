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
  (k -> v -> v) ->
  Relation k v'
fromMap = mapInto

joinInto ::
  (Ord k, Ord v1, Ord v2, Ord v) =>
  Variable k v ->
  Variable k v1 ->
  Variable k v2 ->
  (k -> v1 -> v2 -> v) ->
  Variable k v
joinInto target input1 input2 logic = insertIntoVariable target . newRelation $ results
  where
    recent1 = elements . recent $ input1
    recent2 = elements . recent $ input2
    fromStable2 = foldMap (joinHelper logic recent1 . elements) (stable input2)
    fromStable1 = foldMap (joinHelper' logic recent2 . elements) (stable input1)
    fromRecent = joinHelper logic recent1 recent2
    results = fromStable2 <> fromStable1 <> fromRecent

antiJoinInto ::
  (Ord k, Ord v) =>
  Variable k v ->
  Variable k v ->
  Relation k v' ->
  (k -> v -> v) ->
  Variable k v
antiJoinInto target input1 input2 logic = insertIntoVariable target . newRelation $ results
  where
    r [] _ = []
    r xs [] = xs
    r (x : xs) (y : ys) = undefined -- TODO
    results :: [(k, v)]
    results = undefined

joinHelper :: (k -> v1 -> v2 -> v) -> [(k, v1)] -> [(k, v2)] -> [(k, v)]
joinHelper logic input1 input2 = undefined -- TODO

joinHelper' :: (k -> v1 -> v2 -> v) -> [(k, v2)] -> [(k, v1)] -> [(k, v)]
joinHelper' logic = flip (joinHelper logic)

mapInto = undefined -- TODO
