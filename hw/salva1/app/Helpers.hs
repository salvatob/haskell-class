module Helpers where

-- applies f  n-times
repeatF :: (b -> b) -> Int -> b -> b
repeatF f n = foldr (.) id (replicate n f)

-- add constant to a vector
addC :: Num b => (b, b) -> b -> (b, b)
(x, y) `addC` a = (x + a, y + a)

-- vector addition
add :: (Num n) => (n, n) -> (n, n) -> (n, n)
add (a, b) (x, y) = (a + x, b + y)

-- apply f on element at index i
changeAt :: (a -> a) -> Int -> [a] -> [a]
changeAt _ _ [] = []
changeAt f i l = take i l ++ (f (l !! i)) : drop (i + 1) l


rotP :: IntPoint -> IntPoint
rotP (x, y) = (-y, x)

flipP :: IntPoint -> IntPoint
flipP (x, y) = (-x, y)
