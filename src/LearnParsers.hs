module LearnParsers where

import Text.Trifecta
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
