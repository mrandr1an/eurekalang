module Datatype where

data EurekaValue
  = EurekaNull
  | EurekaBool Bool
  | EurekaNum Integer
--Preparing Source for the parsing
-- getWord :: Int -> Int -> [[String]] -> String
-- getWord ln wd src = srcWord
--   where
--     srcLine = src !! ln
--     srcWord = srcLine !! wd
-- getLn :: Int -> [[String]] -> [String]
-- getLn ln src = srcLine
--   where
--     srcLine = src !! ln
-- scan :: Int -> Int -> [[String]] -> EParser a -> IO (Maybe a)
-- scan word line source parser = do
--   putStrLn $ getWord word line source
--   return (fst <$> runParser parser s)
--   where
--     s = getWord line word source
-- parseLine :: Int -> Int -> [[String]] -> EParser a -> IO (Maybe a) --Scans only one line
-- parseLine word line source parser = do
--   scan word line source parser
--   if length (getLn line source) < word + 1
--     then parseLine (word + 1) line source parser
--     else if length source <= line + 1
--            then return Nothing
--            else parseLine 0 (line + 1) source parser
--function that will read the file and get the source code
