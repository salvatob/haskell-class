module World where

import Shapes
import Graphics.Gloss

data World = World [Shape] Int


rotateSelection :: World -> World
rotateSelection (World s i) = World s ((i+1) `mod` (length s + 1))

-- moveSelected :: Dir -> World -> World
-- moveSelec

-- highlightShape :: World -> Maybe Picture
-- highlightShape (World s i) = drawOutline (s !! i)

drawWorld :: World -> Picture
drawWorld (World shapes i) 
            | i >= (length shapes)  = Pictures shape_drawings
            | otherwise           = Pictures (outline : shape_drawings)
        where
          shape_drawings = map draw shapes
          outline = drawOutline (shapes !! i)
          
