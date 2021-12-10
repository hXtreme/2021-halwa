{-# LANGUAGE InstanceSigs #-}

-- CIS 552, University of Pennsylvania

-- | A small, applicative-based parsing library
module Parser
  ( Parser,
    doParse,
    get,
    eof,
    filter,
    parse,
    parseFromFile,
    ParseError,
    satisfy,
    alpha,
    digit,
    upper,
    line,
    lower,
    space,
    char,
    string,
    word,
    snakeCaseWord,
    titleCaseWord,
    int,
    chainl1,
    chainl,
    choice,
    between,
    sepBy1,
    sepBy,
    wsP,
    constP,
    stringP,
    commaP,
    semicolonP,
    colonP,
    parensP,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Data.Char
import Data.Foldable (asum)
import Data.Functor
import Located (Located, loc, locateAt, locationAfter, val, Location (Location))
import qualified System.IO as IO
import qualified System.IO.Error as IO
import Prelude hiding (filter)

-- definition of the parser type
newtype Parser a = P {doParse :: Located String -> Either ParseError (Located a, Located String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \s -> do
    (c, cs) <- doParse p s
    return (f <$> c, cs)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \s -> Right (pure x, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = P $ \s -> do
    (f, s') <- doParse p1 s
    (x, s'') <- doParse p2 s'
    return (f <*> x, s'')

instance Alternative Parser where
  empty :: Parser a
  empty = P $ \s -> Left (locateAt (loc s) "Empty Parser matches nothing.")

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P $ \s -> doParse p1 s `firstJust` doParse p2 s

instance Monad Parser where
  return = pure
  p >>= f = P $ \s -> do
    (x, s') <- doParse p s
    let x' = f . val $ x
    doParse x' s'

-- | Combine two Maybe values together, producing the first
-- successful result
firstJust :: Either ParseError a -> Either ParseError a -> Either ParseError a
firstJust (Right x) _ = Right x
firstJust (Left _) y = y

-- | Return the next character from the input
get :: Parser Char
get = P $ \s -> case val s of
  (c : cs) -> Right (c', cs')
    where
      c' = locateAt (loc s) c
      cs' = locateAt l' cs
      l' = locationAfter c'
  [] -> Left (locateAt (loc s) "Unexpected end of file.")

-- | This parser *only* succeeds at the end of the input.
eof :: Parser ()
eof = P $ \s -> case val s of
  [] -> Right (locateAt (loc s) (), s)
  _ : _ -> Left (locateAt (loc s) "Expected the end of file.")

-- | Filter the parsing results by a predicate
filter :: (a -> Bool) -> Parser a -> Parser a
filter f p = P $ \s -> do
  (c, cs) <- doParse p s
  if f . val $ c
    then return (c, cs)
    else Left (locateAt (loc c) "Filter failed.")

---------------------------------------------------------------
---------------------------------------------------------------
---------------------------------------------------------------

type ParseError = Located String
type ParseResult a = Either ParseError (Located a)

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors, but we
-- give it a type similar to other Parsing libraries.
parse :: Parser a -> String -> Either ParseError (Located a)
parse parser str = case doParse parser str' of
  Left err -> Left err
  Right (a, _) -> Right a
  where
    str' = pure str

-- | parseFromFile p filePath runs a string parser p on the input
-- read from filePath using readFile. Returns either a
-- ParseError (Left) or a value of type a (Right).
parseFromFile :: Parser a -> String -> IO (Either ParseError (Located a))
parseFromFile parser filename = do
  IO.catchIOError
    ( do
        handle <- IO.openFile filename IO.ReadMode
        str <- IO.hGetContents handle
        pure $ parse parser str
    )
    ( pure . Left . locateAt (Location 0 0) . show
    )

-- | Return the next character if it satisfies the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = filter p get

-- | Parsers for specific sorts of characters
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace

-- | Parses and returns the specified character
-- succeeds only if the input is exactly that character
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | Parses and returns the specified string.
-- Succeeds only if the input is the given string
string :: String -> Parser String
string = foldr (\c p -> (:) <$> char c <*> p) (pure "")

-- | Parses and returns a word.
word :: Parser String
word = many (alpha <|> digit <|> char '_')

-- | Parses and returns a snake case word.
snakeCaseWord :: Parser String
snakeCaseWord = many (lower <|> digit <|> char '_')

-- | Parses and returns a titlecase word.
titleCaseWord :: Parser String
titleCaseWord = (:) <$> upper <*> word

-- | Parses and returns a single line.
line :: Parser String
line = some lineChars <* lineEnding
  where
    end = eof $> ""
    lineChars = satisfy (not . isEndOfLine)
    isEndOfLine = (== '\n')
    lineEnding = (satisfy isEndOfLine $> ()) <|> eof

-- >>> parse (many line) "hello\nworld"
-- Right (L (Location 1 1) ["hello","world"])

-- | succeed only if the input is a (positive or negative) integer
int :: Parser Int
int = read <$> ((++) <$> string "-" <*> some digit <|> some digit)

-- | Parses one or more occurrences of @p@ separated by binary operator
-- parser @pop@.  Returns a value produced by a /left/ associative application
-- of all functions returned by @pop@.
-- See the end of the `Parsers` lecture for explanation of this operator.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` pop = foldl comb <$> p <*> rest
  where
    comb x (op, y) = x `op` y
    rest = many ((,) <$> pop <*> p)

-- | @chainl p pop x@ parses zero or more occurrences of @p@, separated by @pop@.
-- If there are no occurrences of @p@, then @x@ is returned.
chainl :: Parser b -> Parser (b -> b -> b) -> b -> Parser b
chainl p pop x = chainl1 p pop <|> pure x

-- | Combine all parsers in the list (sequentially)
choice :: [Parser a] -> Parser a
choice = asum -- equivalent to: foldr (<|>) empty

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is pureed.
between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = open *> p <* close

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

---------------------------------------------

-- | Parse p and discard following spaces
wsP :: Parser a -> Parser a
wsP p = p <* many space

-- | Parse a constant value
constP :: String -> a -> Parser a
constP s x = (wsP . string) s $> x

-- | Parse a constant string
stringP :: String -> Parser ()
stringP s = constP s ()

-- | Parse a comma
commaP :: Parser ()
commaP = stringP ","

-- | Parse a semicolon
semicolonP :: Parser ()
semicolonP = stringP ";"

-- | Parse a colon
colonP :: Parser ()
colonP = stringP ":"

-- | Parse ( p )
parensP :: Parser a -> Parser a
parensP x = between (stringP "(") x (stringP ")")
