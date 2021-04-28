module Parser where

import           Data.Char                      ( isAlpha )
import           Text.ParserCombinators.Parsec

data Lam = Named String Lang | Raw Lang deriving Show

data Lang = Atom String
          | Fun String Lang
          | App Lang Lang
          deriving Show

-- expr   = \ ID . expr | term
-- term   = factor expr? | factor <- don't know qaq
-- factor = ID | ( expr )

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

-- parse input
parseAll :: Parser Lam
parseAll =
    Named
        <$> (many1 (satisfy isAlpha) <* spaces <* char '=' <* spaces)
        <*> parseLambda
        <|> Raw
        <$> parseLambda

parseFactor :: Parser Lang
parseFactor =
    Atom
        <$> many1 (satisfy isAlpha)
        <|> char '('
        *>  spaces
        *>  parseLambda
        <*  spaces
        <*  char ')'

-- parse lambda expression
parseLambda :: Parser Lang
parseLambda =
    Fun
        <$> (  char '\\'
            *> spaces
            *> many1 (satisfy isAlpha)
            <* char '.'
            <* spaces
            )
        <*> parseLambda
        <|> parseTerm

parseTerm :: Parser Lang
parseTerm =
    try (App <$> parseFactor <* many1 space <*> parseLambda) <|> parseFactor  -- ! problem !
