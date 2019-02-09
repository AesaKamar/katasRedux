module Common where

a |> fa  = fa a
fa <| a  = fa a
a |$> fa = fa <$> a

tup a b = (,) <$> a <*> b
