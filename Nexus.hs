module Nexus where

data Tree a = Leaf a | Node [Tree a] deriving (Show, Eq)

foldt (f, g) = u
  where
    u (Leaf x) = f x
    u (Node xs) = g (fmap u xs)

unfoldt psi = v
  where
    v x = case psi x of
      Left x -> Leaf x
      Right xs -> Node (fmap v xs)

