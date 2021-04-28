module Parser where

import           Data.Char
import           Lambda                         ( Lambda(..)
                                                , Lang(..)
                                                )
import           Text.ParserCombinators.Parsec

-- expr   = \ ID . expr | term
-- term   = term factor | factor
-- factor = ID | ( expr )

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

-- parse input
parseAll :: Parser Lang
parseAll = choice
    [ try
    $   Named
    <$> (many1 identifiers <* spaces <* char '=' <* spaces)
    <*> parseLambda
    , Raw <$> parseLambda
    ]

identifiers :: Parser Char
identifiers = satisfy isAlpha <|> char '_'

-- parse lambda expression
parseLambda :: Parser Lambda
parseLambda =
    Fun
        <$> (char '\\' *> spaces *> many1 identifiers <* char '.' <* spaces)
        <*> parseLambda
        <|> parseTerm

-- ! solved assoc but new peoblem occurs QAQ
parseTerm :: Parser Lambda
parseTerm = (parseFactor <* spaces) `chainl1` pure App

parseFactor :: Parser Lambda
parseFactor =
    Atom
        <$> many1 identifiers
        <|> char '('
        *>  spaces
        *>  parseLambda
        <*  spaces
        <*  char ')'
