module Parser where

import           Data.Char                      ( isAlpha )
import           Data.Functor                   ( ($>) )
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
    <$> (identifiers <* spaces <* char '=' <* spaces)
    <*> parseExpr
    , Raw <$> parseExpr
    ]

identifiers :: Parser String
identifiers = many1 $ satisfy isAlpha <|> char '_'

inside :: Parser a -> Parser a -> Parser b -> Parser b
inside l r p = l *> spaces *> p <* spaces <* r

-- parse lambda expression
parseExpr :: Parser Lambda
parseExpr = parseLambda <|> parseTerm

parseLambda :: Parser Lambda
parseLambda =
    Fun
        <$> inside (spaces *> char '\\') (char '.' <* spaces) identifiers
        <*> parseExpr

parseTerm :: Parser Lambda
parseTerm = (parseFactor <|> parseLambda) `chainl1` (spaces $> App)

parseFactor :: Parser Lambda
parseFactor = Atom <$> identifiers <|> inside (char '(') (char ')') parseExpr
