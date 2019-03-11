module Nexus where

data Tree a = Leaf a | Node [Tree a] deriving (Show, Eq)

{--
foldt (f, g) = u
  where
    u (Leaf x) = f x
    u (Node xs) = g (fmap u xs)
--}
foldt f g (Leaf x) = f x
foldt f g (Node ts) = g (fmap (foldt f g) ts)
{--
unfoldt psi = v
  where
    v x = case psi x of
      Left x -> Leaf x
      Right xs -> Node (fmap v xs)
--}
unfoldt p v h x = if p x then Leaf (v x)
                  else Node (fmap (unfoldt p v h) (h x))
{--
-- hylot f g = foldt f . unfoldt g

-- deforestation!
hylot f g = h
  where
    h x = case g x of
            Left x -> f (Left x)
            Right xs -> f (Right (fmap h xs))
--}

-- hylot f g p v h = foldt f g . unfoldt p v h
-- hylot f g p h x = if p x then f x else g (fmap (hylot f g p h) (h x))
hylot f g p h x | p x       = f x
                | otherwise = g (fmap (hylot f g p h) (h x))
