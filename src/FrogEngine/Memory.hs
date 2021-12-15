module FrogEngine.Memory where

import FrogEngine.Variable
import qualified Data.Map as Map
import Data.Map (Map, (!))


type Memory = Map String Variable

-- | Are two memories equal?
memEq :: (Ord a, Ord k) => Map k a -> Map k a -> Bool
memEq mem1 mem2 = keysMatch && and valuesMatch
    where
        keysMatch = keys1 == keys2
        valuesMatch = map (\k -> (mem1 ! k) `varEq` (mem2 ! k)) keys1
        var1 `varEq` var2 = case compare var1 var2 of
            EQ -> True
            _ -> False
        keys1 = Map.keys mem1
        keys2 = Map.keys mem2