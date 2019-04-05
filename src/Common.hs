module Common where

a |> fa  = fa a
fa <| a  = fa a

a |$> fa = fa <$> a
fa <$| a = fa <$> a

a $> fa = fa <$> a
fa <$ a = fa <$> a

a |*> fa = fa <*> a
fa <*| a = fa <*> a



tup a fa = (,) <$> a <*> fa
