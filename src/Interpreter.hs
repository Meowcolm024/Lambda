module Interpreter where

import           Combinator
import           Data.Either                    ( fromRight )
import           Data.Foldable                  ( foldrM )
import           Lambda
import           Parser

-- primitives :: Env
primitives :: Env
primitives =
    fromRight newEnv
        .   fromRight (Left "Parse Error")
        $   foldrM addToEnv newEnv
        <$> mapM
                (regularParse parseAll)
                [ "Y = \\g.(\\x.g (x x)) \\x.g (x x)"
                , "id = \\x.x"
                , "true = \\x.\\y.x"
                , "false = \\x.\\y.y"
                , "and = \\p.\\q.p q p"
                , "or = \\p.\\q.p p q"
                , "not = \\p.\\a.\\b.p b a"
                ]
