module RAM where

import qualified Data.Bifunctor as Bifunctor
import Data.List (groupBy)
import Data.Map (union)
import qualified Datalog.Argument as DL
import qualified Datalog.Atom as DL
import qualified Datalog.Atom as DL.Atom
import qualified Datalog.Declaration as DL
import qualified Datalog.Declaration as DL.Declaration
import qualified Datalog.Fact as DL
import qualified Datalog.Fact as DL.Fact
import qualified Datalog.Program as D
import qualified Datalog.Query as DL
import FrogEngine.Memory (newMemory)
import qualified FrogEngine.Memory as FR
import qualified FrogEngine.RAM as R
import FrogEngine.Variable (emptyRelation, emptyVariable, newRelation, newVariable)
import qualified FrogEngine.Variable as FR

splitFacts :: [DL.Fact] -> [(String, [DL.Fact])]
splitFacts = map (\fs -> (groupKey fs, fs)) . groups
  where
    groups = groupBy (\f1 f2 -> DL.Fact.getPredicate f1 == DL.Fact.getPredicate f2)
    groupKey fs = DL.Fact.getPredicate (head fs)

argsToConstants :: DL.Argument -> DL.Constant
argsToConstants (DL.Constant c) = c
argsToConstants _ = error "Arguments to Fact must be constants"

factToConstants :: DL.Fact -> [DL.Constant]
factToConstants (DL.Fact (DL.Atom _ args)) = map argsToConstants args

factsToRelation :: [DL.Fact] -> [(String, FR.Relation)]
factsToRelation fs = relationGroups
  where
    groups = map (Bifunctor.second (map factToConstants)) . splitFacts $ fs
    relationGroups = map (Bifunctor.second newRelation) groups

factsToMemory :: [DL.Fact] -> FR.Memory
factsToMemory fs = newMemory vars
  where
    relationGroups = factsToRelation fs
    vars = map (uncurry newVariable) relationGroups

declarationsToMemory :: [DL.Declaration] -> FR.Memory
declarationsToMemory ds = newMemory vars
  where
    vars = map (emptyVariable . DL.Declaration.predicate) ds

astToMemory :: D.Program -> FR.Memory
astToMemory (D.Program ds rs fs dis qs) = factsToMemory fs `union` declarationsToMemory ds

astQueriesToRAM :: [DL.Query] -> [String]
astQueriesToRAM = map getPredicate
  where
    getPredicate (DL.Query (DL.Atom p _)) = p

astToRAM :: D.Program -> R.RAM
astToRAM (D.Program ds rs fs dis qs) = R.RAM mem rules queries
  where
    mem = astToMemory (D.Program ds rs fs dis qs)
    rules = []
    queries = astQueriesToRAM qs
