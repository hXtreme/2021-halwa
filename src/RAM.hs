module RAM where

import qualified Control.Monad.State as S
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Datalog.Argument as DL.Argument
import qualified Datalog.Atom as DL.Atom
import qualified Datalog.Constant as DL.Constant
import qualified Datalog.Declaration as DL.Declaration
import qualified Datalog.Fact as DL.Fact
import qualified Datalog.Program as DL.Program
import qualified Datalog.Rule as DL.Rule
import qualified RAM.Constant
import qualified RAM.Disjunction
import qualified RAM.Fact
import qualified RAM.Program as R
import qualified RAM.Variable

identifierMap :: DL.Program.Program -> Map.Map String Int
identifierMap ast = Map.fromList $ zip symbols [0 ..]
  where
    symbols = mapMaybe isSymbol args
    args = foldl flattenRule [] (DL.Program.rules ast)
    flattenRule acc x = acc ++ (DL.Atom.args . DL.Rule.head $ x)
    isSymbol arg = case arg of
      (DL.Argument.Constant (DL.Constant.Symbol s)) -> Just s
      _ -> Nothing

ramVariables :: DL.Program.Program -> [RAM.Variable.Variable]
ramVariables ast = map declToVar (DL.Program.declarations ast)
  where
    declToVar (DL.Declaration.Declaration pred argsT) =
      RAM.Variable.Variable pred False (astArgsTypeToVarType argsT)

astArgsTypeToVarType args = case args of
  [] -> RAM.Variable.Empty
  [arg] -> RAM.Variable.Base arg
  _ -> RAM.Variable.Tuple $ map RAM.Variable.Base args

astConstToRamConst c idMap = case c of
  DL.Constant.Symbol s -> RAM.Constant.Symbol (idMap ! s)
  DL.Constant.Boolean b -> RAM.Constant.Boolean b
  DL.Constant.Integer i -> RAM.Constant.Integer i

astFactToRamFact :: Map.Map String Int -> DL.Fact.Fact -> RAM.Fact.Fact
astFactToRamFact idMap fact =
  RAM.Fact.Fact (pred fact) (consts fact)
  where
    pred fact = DL.Atom.predicate . DL.Fact.head $ fact
    args fact = DL.Atom.args . DL.Fact.head $ fact
    consts fact = mapMaybe f (args fact)
    f arg = case arg of
      (DL.Argument.Constant c) -> Just $ astConstToRamConst c idMap
      _ -> Nothing

ramFacts :: Map.Map String Int -> DL.Program.Program -> [RAM.Fact.Fact]
ramFacts idMap ast = map (astFactToRamFact idMap) (DL.Program.facts ast)

ramDisjunctions :: Map.Map String Int -> DL.Program.Program -> [RAM.Disjunction.Disjunction]
ramDisjunctions idMap dl = undefined

ramUpdates :: Map.Map String Int -> [RAM.Variable.Variable] -> [RAM.Fact.Fact] -> DL.Rule.Rule -> Int -> a
ramUpdates idMap vars facts rule idx = undefined

astToRAM :: DL.Program.Program -> R.Program
astToRAM dl = R.Program vars facts disjs updates
  where
    idMap = identifierMap dl
    vars = ramVariables dl
    facts = ramFacts idMap dl
    disjs = ramDisjunctions idMap dl
    updates = zipWith (ramUpdates idMap vars facts) (DL.Program.rules dl) [0 ..]
