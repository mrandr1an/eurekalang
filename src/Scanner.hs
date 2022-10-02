module Scanner where

import Control.Applicative
import Data.Char
import Data.List
import Data.String
import System.IO

-- The way the compiler works is this way:
--   1. You give it a file to compile
--   2. The Applicative CLI library used
--    	reads the file.
--   3. A parser is created for each possible value
--   	of the target-language 
--   4. You run the contents (source) as a string
--   	against the parsers and the program compiles
data EurekaValue
  = EurekaNum (Either Integer Float)
  | EurekaArgs String
  | EurekaChar Char
  | EurekaString String
  | EurekaLnBr Char
  | EurekaFun String [EurekaValue]

data EurekaFun id args
  = EurekaDef id args
  | EurekaOp (Either (EurekaFun id args) EurekaValue)

newtype Source =
  Source
    { s :: [Input]
    }
  deriving (Show)

data Input =
  Input
    { i :: String
    , l :: Maybe Int
    }
  deriving (Show)

newtype EParser a =
  EParser
    { runParser :: Input -> Maybe (a, Input)
    }

printPI :: Input -> String
printPI ip = show (i ip)

instance Functor EParser where
  fmap f (EParser p) =
    EParser $ \input -> do
      (p', input') <- p input
      return (f p', input')

instance Applicative EParser where
  pure x = EParser $ \s -> Just (x, s)
  (EParser p1) <*> (EParser p2) =
    EParser $ \source -> do
      (p3, source1) <- p1 source
      (p4, source2) <- p2 source1
      return (p3 p4, source2)

instance Alternative EParser where
  empty = EParser $ const empty
  (EParser p1) <|> (EParser p2) = EParser $ \input -> p1 input <|> p2 input

eurekaValue :: Source -> [EParser EurekaValue]
eurekaValue source =
  let inputs = s source
   in map (eurekaNum) (inputs)

------ Parsing Numbers
eurekaNum :: Input -> EParser EurekaValue
eurekaNum input = EParser p
  where
    p (input)
      | isDigit (head $ i input) =
        Just (EurekaNum $ sourceToInt (checkDigit (i input)), input)
      | otherwise = Nothing

--read for tuples
sourceToInt :: (String, String) -> Either Integer Float
sourceToInt s =
  case s2 of
    " " -> Right (read s1 :: Float)
    "Nothing" -> Right (read (s1 ++ s2) :: Float)
    _ -> Left (read s1 :: Integer)
  where
    s1 = fst s
    s2 = snd s

-- Checks if digit is float or if it has multiple digits
checkDigit :: String -> (String, String)
checkDigit a =
  case dropWhile isDigit a of
    "" -> span isDigit a
    ('.':as) ->
      case dropWhile isDigit as of
        "" -> (a, " ")
        _ -> ("Nothing", "Nothing")
    _ -> ("Nothing", "Nothing")

-- ParseChars
eurekaChar :: Input -> EParser EurekaValue
eurekaChar input = EParser p
  where
    p (input)
      | isLetter (head $ i input) && null (tail $ i input) =
        Just (EurekaChar (head $ i input), input)
      | otherwise = Nothing

eurekaLnBr :: Input -> EParser EurekaValue
eurekaLnBr input = EParser p
  where
    p (input)
      | i input == ";" = Just (EurekaLnBr '\n', input)
      | otherwise = Nothing

eurekaString :: Input -> EParser EurekaValue
eurekaString input = EParser p
  where
    p (input)
      | head (i input) == '"' && last (i input) == '"' =
        Just (EurekaString (i input), input)
      | otherwise = Nothing

parseString :: Input -> [EParser a] -> [Maybe (a, Input)]
parseString s parsers = map parse parsers
  where
    parse = \p -> runParser p s

parseSource :: Source -> [EParser a] -> IO [[Maybe (a, Input)]]
parseSource source parsers = do
  putStrLn "Parsing source..."
  return $ map parse (s source)
  where
    parse = \s -> parseString s parsers

createI :: String -> [String] -> Input
createI s ls = Input {i = s, l = index}
  where
    index = elemIndex s ls

getSource :: String -> IO [[Maybe (EurekaValue, Input)]]
getSource s
 --opens file and stores contents in context variable
 = do
  putStrLn $ "Reading file " ++ s
  handle <- openFile s ReadMode
  contents <- hGetContents handle
  putStrLn "Stored contents in string, creating source"
  let s = words contents
  let inputs = map createInput s
  let source = Source {s = inputs}
  parseSource source (eurekaValue source)
  where
    createInput = \w -> createI w [s]
