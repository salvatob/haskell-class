module Shapes where

import Constants
import Graphics.Gloss

type IntPoint = (Int, Int)

data Shape
  = Square Int -- only size is specified, renders with 
  | Triangle IntPoint IntPoint -- first point is always (0,0), two more are specified
  -- implicit point a is (0,0), specifying two other will result in a parallelogram.
  -- last point is calculated as a vector sum b + c
  | Parallel IntPoint IntPoint

data Sprite = Sprite
  { shape :: Shape
  , position :: IntPoint
  , clr :: Color
  }

smallSquare :: Color -> IntPoint -> Sprite
smallSquare c p = Sprite {shape = Square 1, position = p, clr = c}

smallTriangle :: Color -> IntPoint -> Sprite
smallTriangle c p =
  Sprite {shape = Triangle (0, 1) (1, 0), position = p, clr = c}

largeTriangle :: Color -> IntPoint -> Sprite
largeTriangle c p =
  move D Sprite {shape = Triangle (1, 1) (1, -1), position = p, clr = c}

parallelogram :: Color -> IntPoint -> (IntPoint, IntPoint) -> Sprite
parallelogram c pos (p1, p2) = Sprite (Parallel p1 p2) pos c

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
drawShape (Triangle b c) = Polygon $ translateCoordinates <$> [(0, 0), b, c]
drawShape (Parallel (x1, y1) (x2, y2)) =
  Polygon
    $ translateCoordinates <$> [(0, 0), (x1, y1), (x1 + x2, y1 + y2), (x2, y2)]

draw :: Sprite -> Picture
draw (Sprite s (x, y) c) =
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

rotP :: IntPoint -> IntPoint
rotP (x, y) = (-y, x)

rotateShape :: Shape -> Shape
rotateShape (Triangle b c) = Triangle (rotP b) (rotP c)
rotateShape (Parallel b c) = Parallel (rotP b) (rotP c)
rotateShape s = s

rotateSprite :: Sprite -> Sprite
rotateSprite s = s {shape = rotateShape (shape s)}

flipP :: IntPoint -> IntPoint
flipP (x, y) = (-x, y)

flipShape :: Shape -> Shape
flipShape (Triangle a b) = Triangle (flipP a) (flipP b)
flipShape (Parallel a b) = Parallel (flipP a) (flipP b)
flipShape s = s

flipSprite :: Sprite -> Sprite
flipSprite s = s {shape = flipShape $ shape s}
