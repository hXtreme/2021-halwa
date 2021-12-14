module FrogEngine.Variable
  ( Variable (name, distinct, stable, recent),
    newRelation,
    newVariable,
    merge,
    insertIntoVariable,
    Relation (elements),
    FactLiterals,
  )
where

import Data.Foldable (Foldable (toList))
import Data.Sort (uniqueSort)
import qualified Datalog.Constant as DL

type FactLiterals = [DL.Constant]

-- | A sorted and distinct collection of elements (tuples).
newtype Relation = R {elements :: [FactLiterals]} deriving (Eq, Show)

newRelation :: (Foldable t) => t FactLiterals -> Relation
newRelation = R . uniqueSort . toList

merge :: Relation -> Relation -> Relation
merge (R xs) (R ys) = newRelation (xs ++ ys)

data Variable = Variable
  { name :: String,
    distinct :: Bool,
    stable :: [Relation],
    recent :: Relation,
    todo :: [Relation]
  }
  deriving (Eq, Show)

newVariable :: String -> Bool -> Variable
newVariable name distinct = Variable name distinct [] (R []) []

insertIntoVariable :: Variable -> Relation -> Variable
insertIntoVariable var rel = var {todo = rel : todo var}

allKnownFacts :: Variable -> Relation
allKnownFacts (Variable _ _ stable recent todo) = rel
  where
    rel = foldr merge recent (stable ++ todo)
