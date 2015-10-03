module Main where

import System.Environment
import qualified Parser as Parser
import qualified Lexer as Lexer

print_usage = putStrLn "usage: parseTHF <filename>"

main = do
  args <- getArgs
  case args of
    [] -> print_usage
    (_:_:_) -> print_usage
    _ -> do
      content <- readFile (args !! 0)
      let tokens = map snd (Lexer.alexScanTokens content) in do
        print tokens
        print "--------"
        print . Parser.parseTHF $ tokens
