module Main where

import System.Environment
import System.Directory
import qualified Parser as Parser
import qualified Lexer as Lexer
import Export

print_usage = putStrLn "usage: parseTHF <filename>"

main = do
  args <- getArgs
  case args of
    [] -> print_usage
    (_:_:_) -> print_usage
    _ -> do
      content <- readFile (args !! 0)
      let tokens = map snd (Lexer.alexScanTokens content) in do
        mapM_ putStrLn (export_statements . Parser.parseTHF $ tokens)
--        return . export_statements . Parser.parseTHF $ tokens
--        putStr "."
