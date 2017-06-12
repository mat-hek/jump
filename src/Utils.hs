module Utils where

(|>) :: a -> (a -> b) -> b
(|>) v f = f v
infixl 5 |>
