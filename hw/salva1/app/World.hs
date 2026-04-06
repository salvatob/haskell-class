module World where

import Graphics.Gloss
import Shapes
import Helpers

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


moveSelected :: Dir -> World -> World
moveSelected _ (World s i False) = World s i False -- if shape is not picked up, dont do anything
moveSelected d (World s i True)
  | i >= length s = World s i True
  | otherwise = World newS i True
  where
    newS = changeAt (move d) i s

changeSelected :: (Sprite -> Sprite) -> World -> World
changeSelected _ (World s i False) = World s i False -- if shape is not picked up, dont do anything
changeSelected f (World sprite i True)
  | i >= length sprite = World sprite i True -- selected index in not on any shape
  | otherwise = World newS i True
  where
    newS = changeAt f i sprite

drawWorld :: World -> Picture
drawWorld (World shapes i _)
  | i >= length shapes = Pictures shape_drawings
  | otherwise = Pictures highlight
  where
    shape_drawings = map draw shapes
    highlight = changeAt (\(Color _ s) -> Color black s) i shape_drawings
