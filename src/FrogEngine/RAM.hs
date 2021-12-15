module FrogEngine.RAM where

import Control.Monad.State (State)
import qualified Control.Monad.State as S
import Data.Map (Map, mapWithKey, (!))
import qualified Data.Map as Map
import Datalog.Variable (Variable)
import FrogEngine.Memory (Memory, memEq)
import FrogEngine.Rule (Rule, stepRules)
import FrogEngine.Variable (FactLiterals, allKnownFacts, stepVariable)

-- | Relational Algebra Machine.
data RAM = RAM
  { memory :: Memory,
    rules :: [Rule],
    queries :: [String]
  }
  deriving (Show, Eq)

-- | Update the memory and stored varaibles.
stepMemory :: State Memory ()
stepMemory = do
  mem <- S.get
  S.put $ mapWithKey (\_ v -> stepVariable v) mem
  return ()

-- | Apply all the rules once.
step :: [Rule] -> State Memory [Rule]
step rules = do
  stepRules rules
  stepMemory
  return rules

-- | Apply all the rules until no more changes are made.
stepToEnd :: [Rule] -> State Memory ()
stepToEnd rules = do
  oldMem <- S.get
  step rules
  newMem <- S.get
  if oldMem `memEq` newMem
    then return ()
    else stepToEnd rules

-- | Run the RAM.
exec' :: RAM -> Memory
exec' (RAM mem rs _) = snd . S.runState (stepToEnd rs) $ mem

-- | Run the RAM and return query results.
exec :: RAM -> Memory
exec (RAM mem rs qs) = queryResult
  where
    execResult = exec' (RAM mem rs qs)
    queryResult = Map.filterWithKey (\k _ -> k `elem` qs) execResult
