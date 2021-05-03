module Interpreter where

import           Combinator
import           Data.Either                    ( fromRight )
import           Data.Foldable                  ( foldrM )
-- import           Lambda
import           Parser                         ( parseAll
                                                , regularParse
                                                )

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
                , "if = \\p.\\a.\\b.p a b"
                , "zero = \\f.\\x.x"
                , "one = \\f.\\x.f x"
                , "succ = \\n.\\f.\\x.f (n f x)"
                , "pred = \\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) \\u.u"
                , "iszero = \\n.n (\\x.\\x.\\y.y) \\x.\\y.x"
                ]
