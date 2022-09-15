module Scanner where

import System.IO

--this is the source code (or source codes of the program stored in the file that will be compiled
data Source = Source
 {
  sourceString :: [Char],
  sourceLength :: Int
 }

globalSource::[IO Char]
globalSource = []




newtype Scanner = Scanner
 {
 cursor :: (Int, Int)
 }

--function that will read the file and get the source code
getSource:: String -> IO()
getSource s = do
 --opens file and stores contents in context variable
 putStrLn $ "Reading file " ++ s
 handle <- openFile s ReadMode
 -- contents <- hGetContents handle
 putStrLn "Stored contents in string, getting chars"
 source <- scan handle
 putStrLn "Scanned source \n Adding sourcefile(s) to global source."

--scans source char by char and stores it in a source list which will be parsed
scan:: Handle -> IO ()
scan fname =
  --if is end of file then return nothing else getChar and repeat
  do ineof <- hIsEOF fname
     if ineof
       then do
              hClose fname
              return () --returns an IO of nothing
       else do char <- hGetChar fname --else if it isnt the end of file check if the character is a whitespace
               --TO-DO REMOVE ALSO COMMENTS //
               if char == ' '
                 then scan fname
                 else do
                        putChar char
                        scan fname -- if not add it to source and repeat the function 


 
