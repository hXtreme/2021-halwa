module FrogEngine.Variable
  ( Variable (name, distinct, stable, recent),
    newRelation,
    newVariable,
    merge,
    insertIntoVariable,
    stepVariable,
    allKnownFacts,
    Relation (elements),
    FactLiterals,
  )
where

import Data.Foldable (Foldable (toList))
import Data.Sort (uniqueSort)
import qualified Datalog.Argument as DL
import qualified Datalog.Argument as DL.Argument
import qualified Datalog.Atom as DL
import qualified Datalog.Constant as DL
import Datalog.Fact (Fact)
import qualified Datalog.Fact as DL

type FactLiterals = [DL.Constant]

-- | A sorted and distinct collection of elements (tuples).
newtype Relation = R {elements :: [FactLiterals]} deriving (Eq, Show, Ord)

-- | New relation from a list of Facts.
newRelation :: (Foldable t) => t FactLiterals -> Relation
newRelation = R . uniqueSort . toList

-- | Relation with no facts.
emptyRelation :: Relation
emptyRelation = R []

-- | Convert Relation representation to Datalog Fact representation.
relationToFacts :: String -> Relation -> [Fact]
relationToFacts name (R lits) = map fact lits
  where
    litToArg :: DL.Constant -> DL.Argument
    litToArg lit = DL.Argument.Constant lit
    args lits = map litToArg lits
    fact lits = DL.Fact . DL.Atom name . args $ lits

-- | Merge two relations.
merge :: Relation -> Relation -> Relation
merge (R xs) (R ys) = R $ merge' xs ys
  where
    merge' [] ys = ys
    merge' xs [] = xs
    merge' (x : xs) (y : ys)
      | x <= y = x : merge' xs (y : ys)
      | otherwise = y : merge' (x : xs) ys

-- | RAM Variable.
data Variable = Variable
  { name :: String,
    distinct :: Bool,
    stable :: Relation,
    recent :: Relation,
    todo :: [Relation]
  }
  deriving (Eq, Show)

instance Ord Variable where
  compare (Variable _ _ s1 r1 _) (Variable _ _ s2 r2 _) = compare (s1, r1) (s2, r2)

-- | New variable.
newVariable :: String -> Relation -> Variable
newVariable name rel = Variable name True emptyRelation rel []

-- | Insert a facts into a variable.
insertIntoVariable :: Variable -> Relation -> Variable
insertIntoVariable var rel = var {todo = rel : todo var}

-- | Update a variable to reflect stable and recent facts.
stepVariable :: Variable -> Variable
stepVariable (Variable n d s r t) =
  Variable n d s' r' []
  where
    s' = if null . elements $ r then s else merge s r
    r' = newRelation . concatMap elements $ t

-- | All known facts for a Variable.
allKnownFacts :: Variable -> [Fact]
allKnownFacts (Variable name _ stable recent todo) = relationToFacts name rel
  where
    rel = foldr merge (stable `merge` recent) todo