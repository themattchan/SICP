module SICP.Utils where

infixr 8 ...
(...) :: (c -> d) -> (a -> b -> c) ->(a -> b -> d)
(...) = (.) . (.)
