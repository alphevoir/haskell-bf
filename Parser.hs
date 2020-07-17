{-# LANGUAGE FlexibleInstances #-}
module Parser
( parse
, ParserError (..)
) where

import BF
import Control.Applicative

data Token
  = TEOF
  | TChar { getToken :: Char}
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize [] = [TEOF]
tokenize (c : cs)
  | c `elem` commands = TChar c : tokenize cs
  | otherwise = tokenize cs

newtype Parser a = Parser
  { runParser :: [Token] -> Either ParserError ([Token], a)
  }

newtype ParserError = ParserError
  { getError :: String
  } deriving (Eq, Show)

instance Functor Parser where
  fmap f (Parser p) = Parser fP
    where
      fP ts = do
        (ts', a) <- p ts
        return (ts', f a)

instance Applicative Parser where
  pure x = Parser lift
    where
      lift ts = Right (ts, x)

  Parser pf <*> Parser p = Parser apply
    where
      apply ts = do
        (ts', f) <- pf ts
        (ts'', a) <- p ts'
        return (ts'', f a)

instance Alternative (Either ParserError) where
  empty = Left $ ParserError "empty"

  Left _ <|> e = e
  e <|> _ = e

instance Alternative Parser where
  empty = Parser emptyP
    where
      emptyP = const empty

  Parser p1 <|> Parser p2 = Parser alternative
    where
      alternative ts = p1 ts <|> p2 ts

eofP :: Parser Token
eofP = Parser eof
  where
    eof [TEOF] = Right ([], TEOF)
    eof (TEOF:_:_) = Left $ ParserError "Error: EOF reached before end of input"
    eof (t:ts) = Left $ ParserError $ "Error: No parse on " ++ [getToken t]
    eof _ = Left $ ParserError "Error: Missing EOF"

charP :: Char -> Parser Token
charP c = Parser match
  where
    match (TEOF:_) = Left $ ParserError "Error: Unexpected end of input"
    match (t:ts)
      | t == TChar c = Right (ts, t)
      | otherwise = Left $ ParserError $ "Error: Expected " ++ [c] ++ " but found " ++ [getToken t]
    match _ = Left $ ParserError "Error: Missing EOF"

leftP, rightP, incrementP, decrementP, inputP, outputP :: Parser BF
leftP = L <$ charP left
rightP = R <$ charP right
incrementP = Inc <$ charP increment
decrementP = Dec <$ charP decrement
inputP = In <$ charP input
outputP = Out <$ charP output

loopP :: Parser BF
loopP = Loop <$> (charP loopL *> codeP <* charP loopR)

commandP :: Parser BF
commandP = leftP <|> rightP <|> incrementP <|> decrementP <|> inputP <|> outputP <|> loopP

codeP :: Parser [BF]
codeP = many commandP

code :: Parser [BF]
code = codeP <* eofP

parse :: String -> Either ParserError [BF]
parse = fmap snd . (runParser code) . tokenize
