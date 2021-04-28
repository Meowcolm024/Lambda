module Lambda where

data Lang = Named String Lambda
          | Raw Lambda
          deriving (Show, Eq)

data Lambda = Atom String
            | Fun String Lambda
            | App Lambda Lambda
            deriving (Show, Eq)
