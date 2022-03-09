module Lam where

import Lam.Parser      (parseProgram)
import Lam.PrettyPrint (pprint)
import Lam.Semantics   (multiStep)

import System.Directory   (doesPathExist)
import System.Environment (getArgs)
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  -- Get command line args
  case args of
    [] -> putStrLn "Please supply a filename as a command line argument"
    -- If we have at least one
    (fname:_) -> do
      -- Check if this is a file
      exists <- doesPathExist fname
      if not exists
        then putStrLn $ "File `" <> fname <> "` cannot be found."
        else do
          -- Read the file, parse, and do something...
          input <- readFile fname
          case parseProgram fname input of
            Right ast -> do
              -- Show AST
              putStrLn $ "\n " <> ansi_bold <> "AST: " <> ansi_reset <> show ast

              -- Pretty print
              putStrLn $ "\n " <> ansi_bold <> "Pretty: " <> ansi_reset <> pprint ast

              -- Evaluate
              let (normalForm, count) = multiStep ast
              putStrLn $ "\n " <> ansi_bold <> "Number of steps: " <> ansi_reset <> show count
              putStrLn $ "\n " <> ansi_bold <> "Normal form: " <> ansi_reset <> pprint normalForm

            Left msg -> do
              putStrLn $ ansi_red ++ "Error: " ++ ansi_reset ++ msg
              exitFailure

ansi_red, ansi_green, ansi_reset, ansi_bold :: String
ansi_red   = "\ESC[31;1m"
ansi_green = "\ESC[32;1m"
ansi_reset = "\ESC[0m"
ansi_bold  = "\ESC[1m"