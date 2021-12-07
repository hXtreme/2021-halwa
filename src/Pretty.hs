module Pretty
  ( Pretty (pretty),
    CommaSeparatedList (..),
    NewLineSeparatedList (..),
  )
where

import Data.List
import GHC.Unicode (toLower)

class Show a => Pretty a where
  pretty :: a -> String
  pretty = show

newtype CommaSeparatedList a = CSL {csl :: [a]} deriving (Eq, Show)

newtype NewLineSeparatedList a = NSL {nsl :: [a]} deriving (Eq, Show)

instance Pretty Int where
  pretty = show

instance Pretty Bool where
  pretty = map toLower . show

instance Pretty String where
  pretty = id

instance Pretty a => Pretty (CommaSeparatedList a) where
  pretty l = intercalate ", " (map pretty (csl l))

instance Pretty a => Pretty (NewLineSeparatedList a) where
  pretty l = intercalate "\n" (map pretty (nsl l))

-- >>> pretty (CSL [1 :: Int, 2, 3])
-- "1, 2, 3"

-- >>> pretty (NSL [True, False, False])
-- "true\nfalse\nfalse"
