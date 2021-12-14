module FrogEngine.Variable
  ( Variable (name, distinct, stable, recent),
    newRelation,
    newVariable,
    merge,
    insertIntoVariable,
    Relation (elements),
  )
where

import Data.Foldable (Foldable (toList))
import Data.Sort (uniqueSort)
import qualified FrogEngine.Lib as Lib

-- | A sorted and distinct collection of elements (tuples).
newtype Relation k v = R {elements :: [(k, v)]} deriving (Eq, Show)

newRelation :: (Ord k, Ord v, Foldable t) => t (k, v) -> Relation k v
newRelation = R . uniqueSort . toList

merge :: (Ord k, Ord v) => Relation k v -> Relation k v -> Relation k v
merge (R xs) (R ys) = newRelation (xs ++ ys)

data Variable k v = Variable
  { name :: String,
    distinct :: Bool,
    stable :: [Relation k v],
    recent :: Relation k v,
    todo :: [Relation k v]
  }
  deriving (Eq, Show)

newVariable :: String -> Bool -> Variable k v
newVariable name distinct = Variable name distinct [] (R []) []

insertIntoVariable :: Variable k v -> Relation k v -> Variable k v
insertIntoVariable var rel = var {todo = rel : todo var}

allKnownFacts :: (Ord k, Ord v) => Variable k v -> Relation k v
allKnownFacts (Variable _ _ stable recent todo) = rel
  where
    rel = foldr merge recent (stable ++ todo)
