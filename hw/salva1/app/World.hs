module World where

import Graphics.Gloss
import Shapes

data World =
  World [Shape] Int

rotateSelection :: World -> World
rotateSelection (World s i) = World s ((i + 1) `mod` (length s + 1))

changeAt :: (a -> a) -> Int -> [a] -> [a]
changeAt _ _ [] = []
changeAt f i l = take i l ++ [f (l !! i)] ++ drop (i + 1) l

moveSelected :: Dir -> World -> World
moveSelected d (World s i)
  | i >= length s = World s i
  | otherwise = World newS i
  where
    newS = changeAt (move d) i s

drawWorld :: World -> Picture
drawWorld (World shapes i)
  | i >= length shapes = Pictures shape_drawings
  | otherwise = Pictures (outline : shape_drawings)
  where
    shape_drawings = map draw shapes
    outline = drawOutline (shapes !! i)
