module LearnParsers where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
import Data.Monoid ((<>))
--import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

two = char '2'
three = char '3'

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

oneStr = string "1"
oneTwoStr = string "12"

oneTwoThreeStr = string "123"

parseIntegerEOF :: Parser Integer
parseIntegerEOF = do
  i <- integer
  e <- eof
  return i

type FractionalOrInteger = Either Rational Integer

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseFoI :: Parser FractionalOrInteger
parseFoI = do
  skipMany (oneOf "\n")
  v <- try (Left <$> parseFraction) <|> (Right <$> decimal)
  skipMany (oneOf "\n")
  return v
--print $ parseString parseFoI mempty "\n 123/3\n22\n"

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParseEOF :: Parser () -> IO ()
testParseEOF p = print $ parseString p mempty "123"

testParseString :: Parser String -> IO ()
testParseString p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

mainLP = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "one >> EOF:"
  testParseEOF (one >> eof)
  pNL "oneTwo >> EOF"
  testParseEOF (oneTwo >> eof)
  pNL "string"
  testParseString (choice [oneTwoThreeStr, oneTwoStr, oneStr, stop])
  pNL "chars"
  testParse (choice
    [one >> two >> three, one >> two, one, stop])

data NumberOrString = NOSS String | NOSI Integer
  deriving (Show)

instance Eq NumberOrString where
  NOSS s == NOSS s' = s == s'
  NOSI i == NOSI i' = i == i'
  _ == _ = False
instance Ord NumberOrString where
  NOSS s `compare` NOSS s' = compare s s'
  NOSI i `compare` NOSI i' = compare i i'
  NOSI _ `compare` _ = LT
  _ `compare` NOSI _ = GT

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
  deriving (Show)

instance Eq SemVer where
  SemVer ma mi pa re me == SemVer ma' mi' pa' re' me' =
    ma == ma' && mi == mi' && pa == pa' && re == re' && me == me'
instance Ord SemVer where
  SemVer ma mi pa re _ `compare` SemVer ma' mi' pa' re' _ =
    let c = compare ma ma' <> compare mi mi' <> compare pa pa'
     in case c of
          EQ -> compare re re'
          _  -> c

parseSemVer :: Parser SemVer
parseSemVer = do
  ma <- decimal
  _ <- char '.'
  mi <- decimal
  _ <- char '.'
  pa <- decimal
  rel <- try pRel
  meta <- try pMeta
  _ <- eof
  return $ SemVer ma mi pa rel meta

pRel :: Parser [NumberOrString]
pRel = (char '-' >> many pNoS' ) <|> return []

pMeta :: Parser [NumberOrString]
pMeta = (char '+' >> many pNoS' ) <|> return []

pNoS' :: Parser NumberOrString
pNoS' = do
  ns <- pNoS
  skipMany $ oneOf "."
  return ns

pNoS :: Parser NumberOrString
pNoS = (NOSI <$> decimal) <|> (NOSS <$> some letter)
