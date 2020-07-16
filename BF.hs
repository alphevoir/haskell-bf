module BF where

data BF
  = L
  | R
  | Inc
  | Dec
  | In
  | Out
  | Loop [BF]
  deriving Show

commands :: String
left, right, increment, decrement, input, output, loopL, loopR :: Char
commands @ (left : right : increment : decrement : input : output : loopL : loopR : []) = "<>+-,.[]"

bfShow :: BF -> String
bfShow L = [left]
bfShow R = [right]
bfShow Inc = [increment]
bfShow Dec = [decrement]
bfShow In = [input]
bfShow Out = [output]
bfShow (Loop is) = [loopL] ++ bfsShow is ++ [loopR]

bfsShow :: [BF] -> String
bfsShow = concatMap bfShow

bfPP :: [BF] -> IO ()
bfPP = putStrLn . bfsShow
