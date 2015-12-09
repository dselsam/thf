module Main where

import System.Environment
import System.Directory
import qualified Parser as Parser
import qualified Lexer as Lexer
import Export

print_usage = putStrLn "usage: parseTHF <filename>"

get_bool :: String -> Bool
get_bool s = read s

main = do
  args <- getArgs
  case args of
    [filename] -> do
         content <- readFile (args !! 0)
         mapM_ putStrLn (export_statements . Parser.parseTHF $ map snd (Lexer.alexScanTokens content))
    _ -> print_usage
