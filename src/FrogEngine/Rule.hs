module FrogEngine.Rule where

-- Project RelName RelName1 ColIdx1
-- Join RelName ProjectionIdx RelName1 ColIdx1 RelName2 ColIdx2
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import Control.Monad.State
import FrogEngine.Memory (Memory)
import FrogEngine.Variable (Variable)
import FrogEngine.Join
import Data.Map ((!))

-- | Update rule to update a RAM Variable
data Rule
  = Proj String String [Int]
  | Join String [Int] String [Int] String [Int]
  deriving (Show, Eq)

-- | Get the name of the relation this rule is for
ruleName :: Rule -> String
ruleName (Proj relName _ _) = relName
ruleName (Join relName _ _ _ _ _) = relName

-- | Helper function for projecting relations.
proj :: [Int] -> [b] -> [b]
proj idxs ls = map (ls !!) idxs

-- | Apply a rule.
stepRule :: Rule -> State Memory Variable
-- p(A, B, ...) :- q(A1, B1, ...).
stepRule (Proj relName relName1 colIdx1) = do
  ctx <- S.get
  let var' = fromMap (p ctx) logic (q ctx)
  S.put $ Map.insert relName var' ctx
  return var'
  where
    p = (! relName)
    q = (! relName1)
    logic = proj colIdx1

-- p(A, B, ...) :- q(A1, B1, ...), r(A2, B2, ...)
stepRule (Join relName projIdx relName1 colIdx1 relName2 colIdx2) = do
  ctx <- S.get
  let var' = fromJoin (p ctx) logic qKey rKey (q ctx) (r ctx)
  S.put $ Map.insert relName var' ctx
  return var'
  where
    p = (! relName)
    q = (! relName1)
    r = (! relName2)
    qKey = proj colIdx1
    rKey = proj colIdx2
    logic qs rs = proj projIdx (qs ++ rs)

-- | Apply a list of rules.
stepRules :: [Rule] -> State Memory ()
stepRules (r:rs) = do
  stepRule r
  stepRules rs
stepRules [] = return ()