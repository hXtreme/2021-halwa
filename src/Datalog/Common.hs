module Datalog.Common where

keyWords :: [String]
keyWords =
  [ "decl",
    "query",
    "Symbol",
    "Int",
    "Bool",
    "String",
    "true",
    "false"
  ]

isKeyWord :: String -> Bool
isKeyWord = (`elem` keyWords)

isNotKeyWord :: String -> Bool
isNotKeyWord = not . isKeyWord
