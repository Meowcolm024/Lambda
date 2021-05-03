module Lambda where

data Lang = Named String Lambda
          | Raw Lambda
          deriving Eq

data Lambda = Atom String
            | Fun String Lambda
            | App Lambda Lambda
            deriving Eq

instance Show Lambda where
    show = showLambda

instance Show Lang where
    show = showLang

showLambda :: Lambda -> String
showLambda (Atom x               ) = x
showLambda (Fun v        b       ) = "\\" <> v <> ".(" <> showLambda b <> ")"
showLambda (App (Atom f) (Atom x)) = f <> " " <> x
showLambda (App (Atom f) x       ) = f <> " (" <> showLambda x <> ")"
showLambda (App f        (Atom x)) = "(" <> showLambda f <> ") " <> x
showLambda (App f x) = "(" <> showLambda f <> ") (" <> showLambda x <> ")"

showLang :: Lang -> String
showLang (Named n l) = n <> " := " <> showLambda l
showLang (Raw l    ) = showLambda l
