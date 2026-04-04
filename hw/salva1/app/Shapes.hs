module Shapes where

import Constants
import Debug.Trace
import Graphics.Gloss

type IntPoint = (Int, Int)

data Shape
  = Square Int -- only size is specified, renders with 
  | Triangle IntPoint IntPoint -- first point is always (0,0), two more are specified
  | Parallel IntPoint Int -- first point is (0,0), second is specified, then the line is filled horizontally into a full parallelogram

data Sprite = Sprite
  { shape :: Shape
  , position :: IntPoint
  , clr :: Color
  , rotation :: Float
  }

smallSquare :: Color -> IntPoint -> Sprite
smallSquare c p = Sprite {shape = Square 1, position = p, clr = c, rotation = 0}

smallTriangle :: Color -> IntPoint -> Float -> Sprite
smallTriangle c p r =
  Sprite {shape = Triangle (0, 1) (1, 0), position = p, clr = c, rotation = r}

--TODO make parallel constructor function



-- My coordinates are in Ints,
-- (0,0) starts at top left corner instead of middle, y increasing goes down
-- and the unit is defined as a size of a small square, that is relative to the window size
translateCoordinates :: IntPoint -> Point
translateCoordinates (x, y) =
  ( fromIntegral $ (x * unitSize) - windowWidth `div` 2
  , fromIntegral $ windowHeight `div` 2 - (y * unitSize))

addC :: Num b => (b, b) -> b -> (b, b)
(x, y) `addC` a = (x + a, y + a)

drawShape :: Shape -> Picture
drawShape (Square size) =
  Polygon
    $ translateCoordinates <$> [(0, 0), (size, 0), (size, size), (0, size)]

drawShape (Triangle b c) = Polygon $ translateCoordinates <$> [(0, 0) , b, c]


drawShape (Parallel (x2, y2) len) =
  Polygon
    $ translateCoordinates <$> [(0, 0), (x2, y2), (x2 + len, y2), (len, 0)]

draw :: Sprite -> Picture
draw (Sprite s (x, y) c r) =
  Color c
    $ Translate (fromIntegral (x * unitSize)) (-fromIntegral (y * unitSize))
    $ drawShape s

data Dir
  = L
  | R
  | U
  | D

offset = 1 :: Int

moveOffset :: Dir -> (Int, Int)
moveOffset L = (-offset, 0)
moveOffset R = (offset, 0)
moveOffset U = (0, -offset)
moveOffset D = (0, offset)

add :: (Num n) => (n, n) -> (n, n) -> (n, n)
add (a, b) (x, y) = (a + x, b + y)

-- just add the offset to the position
move :: Dir -> Sprite -> Sprite
move d s = s {position = position s `add` moveOffset d}


rotateShape :: Shape -> Shape
rotateShape (Triangle (x1,y1) (x2,y2) ) = Triangle (-y1, x1) (-y2, x2)
rotateShape s = s

rotateSprite :: Sprite -> Sprite
rotateSprite s = s {shape = rotateShape (shape s)}

flipShape :: Shape -> Shape
flipShape (Triangle (x1,y1) (x2,y2) ) = Triangle (-x1,y1) (-x2,y2)
flipShape (Parallel (x,y) s) = Parallel (-x, y) (-s)
flipShape s = s

flipSprite :: Sprite -> Sprite
flipSprite s = s { shape = flipShape $ shape s }

