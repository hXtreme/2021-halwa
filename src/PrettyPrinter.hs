module PrettyPrinter where

import Data.List

class Show a => Pretty a where
    prettyPrint :: a -> String
    prettyPrint = show

newtype CommaSeparatedList a = CommaSeparatedList {csl :: [a]} deriving (Eq, Show)
newtype NewLineSeparatedList a = NewLineSeparatedList {nsl :: [a]} deriving (Eq, Show)

instance Pretty a => Pretty (CommaSeparatedList a) where
    prettyPrint l = intercalate ", " (map prettyPrint (csl l))

instance Pretty a => Pretty (NewLineSeparatedList a) where
    prettyPrint l = intercalate "\n" (map prettyPrint (nsl l))