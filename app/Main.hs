module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Scanner

-- The CLI data type accepts the filename (target that will be compiled)
-- interp is a function that will take the file source code in string format and will use scan
-- -- getSource is going to get the sourcecode of the file from the given name from the IO ()
-- scan is going to move the scanners cursor one by one until it reaches the end of the string
-- Scanner is the datatype that has two cursor values x y for chars and lines respectively.
-- The idea is this:
--     
-- 	interp:: CompilerCli -> IO() {let s = getSource}
--
--
newtype CompilerCli = CompilerCli
  {
  target :: String
  }

cli :: Parser CompilerCli
cli = CompilerCli
  <$> strOption
    (long "target"
    <> short 't'
    <> metavar "TARGET"
    <> help "Target to compile")

runCli :: CompilerCli -> IO ()
runCli (CompilerCli t) = getSource t

main :: IO ()
main = runCli =<< execParser opts
  where
    opts = 
      info (cli <**> helper)
      (fullDesc
      <> progDesc "Compiles a target to x86_64"
      <> header "t a test for a compuiler for Eureka lang"
      )     
