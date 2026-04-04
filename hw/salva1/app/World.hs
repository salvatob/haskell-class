module World where

import Graphics.Gloss
import Shapes

data World =
  World [Sprite] Int Bool

-- increment the index of the selected shape
rotateSelection :: World -> World
rotateSelection (World s i False) = World s newI False
  where
    newI = (i + 1) `mod` (length s + 1)
rotateSelection (World s i p) = World s i p -- if a shape is selected, do not rotate

handlePickUp :: World -> World
handlePickUp (World s i picked) = World s i (not picked)

-- util for moving a shape by index
changeAt :: (a -> a) -> Int -> [a] -> [a]
changeAt _ _ [] = []
changeAt f i l = take i l ++ (f (l !! i)) : drop (i + 1) l

moveSelected :: Dir -> World -> World
moveSelected _ (World s i False) = World s i False -- if shape is not picked up, dont do anything
moveSelected d (World s i True)
  | i >= length s = World s i True
  | otherwise = World newS i True
  where
    newS = changeAt (move d) i s

drawWorld :: World -> Picture
drawWorld (World shapes i _)
  | i >= length shapes = Pictures shape_drawings
  | otherwise = Pictures highlight
  where
    shape_drawings = map draw shapes
    highlight = changeAt (\(Color _ s) -> Color black s) i shape_drawings
