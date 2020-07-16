module Main where

import Parser
import Runner
import System.Environment (getArgs)
import System.Exit (die, exitWith, ExitCode (ExitSuccess))

parseArgs :: [String] -> IO String
parseArgs ["-h"] = usage >> exit
parseArgs [] = getContents
parseArgs ["-i", f] = readFile f
parseArgs [str] = return str
parseArgs _ = usage >> die "-1"

usage :: IO ()
usage = putStrLn "Usage: bf [-h] [-i file]"

exit :: IO a
exit = exitWith ExitSuccess

noparse :: String -> IO a
noparse s = putStrLn "No parse" >> die s

main :: IO ()
main = do
  args <- getArgs
  input <- parseArgs args

  case parse input of
    Left e -> noparse $ getError e

    Right bf -> do
      _ <- run bf

      return ()
