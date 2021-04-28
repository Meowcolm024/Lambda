module Parser where


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
parseAll =
    Named
        <$> (many1 alphaNum <* spaces <* char '=' <* spaces)
        <*> parseLambda
        <|> Raw
        <$> parseLambda

-- parse lambda expression
parseLambda :: Parser Lambda
parseLambda =
    Fun
        <$> (char '\\' *> spaces *> many1 alphaNum <* char '.' <* spaces)
        <*> parseLambda
        <|> parseTerm

parseTerm :: Parser Lambda
parseTerm = (parseFactor <* spaces) `chainl1` pure App

parseFactor :: Parser Lambda
parseFactor =
    Atom
        <$> many1 alphaNum
        <|> char '('
        *>  spaces
        *>  parseLambda
        <*  spaces
        <*  char ')'
