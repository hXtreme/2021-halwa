module Located where

import Control.Monad.Cont (Monad)

newtype Location
  = Location (Int, Int) -- (line, column)
  deriving (Eq, Show)

row :: Location -> Int
row (Location (l, _)) = l

column :: Location -> Int
column (Location (_, c)) = c

instance Ord Location where
  l1 `compare` l2 = case row l1 `compare` row l2 of
    EQ -> column l1 `compare` column l2
    x -> x

newtype Located a
  = L (Location, a)
  deriving (Eq, Show)

loc :: Located a -> Location
loc (L (l, _)) = l

val :: Located a -> a
val (L (_, a)) = a

instance Functor Located where
  fmap f l = L (loc l, f . val $ l)

instance Applicative Located where
  pure a = L (Location (1, 1), a)

  {-
  Alternatively consider:
      l1 <*> l2 = L (loc l2, val l1 (val l2))
  -}
  l1 <*> l2 = L (loc l1, val l1 (val l2))

instance Monad Located where
  return :: a -> Located a
  return a = L (Location (1, 1), a)
  (>>=) :: Located a -> (a -> Located b) -> Located b
  (L (l, a)) >>= f = f a
