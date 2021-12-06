module Located where

import Control.Monad.Cont (Monad)

data Location
  = Location Int Int -- (line, column)
  deriving (Eq, Show)

-- | Row number of the location.
row :: Location -> Int
row (Location l _) = l

-- | Column number of the location.
column :: Location -> Int
column (Location _ c) = c

-- | The location that follows after a character.
locationAfter :: Located Char -> Location
locationAfter (L l c) = case c of
  '\n' -> Location (row l + 1) 1
  _ -> Location (row l) (column l + 1)

instance Ord Location where
  l1 `compare` l2 = case row l1 `compare` row l2 of
    EQ -> column l1 `compare` column l2
    x -> x

data Located a
  = L Location a
  deriving (Eq, Show)

loc :: Located a -> Location
loc (L l _) = l

val :: Located a -> a
val (L _ a) = a

locateAt :: Location -> a -> Located a
locateAt = L

instance Functor Located where
  fmap f l = L (loc l) (f . val $ l)

instance Applicative Located where
  pure = L (Location 1 1)

  {-
  Alternatively consider:
      l1 <*> l2 = L (loc l2, val l1 (val l2))
  -}
  l1 <*> l2 = L (loc l1) (val l1 (val l2))

instance Monad Located where
  return = pure
  L l a >>= f = f a
