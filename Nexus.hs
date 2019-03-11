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

-- hylot f g = foldt f . unfoldt g

-- deforestation!
hylot f g = h
  where
    h x = case g x of
            Left x -> f (Left x)
            Right xs -> f (Right (fmap h xs))
