module Lambda where

data Lambda = Atom String
            | Fun Lambda Lambda
            | App Lambda Lambda
            deriving Show
