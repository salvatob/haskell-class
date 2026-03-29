module Shapes where

import Constants
import Graphics.Gloss

type Position = (Int, Int)

type IntPoint = (Int, Int)

data Shape
  = Square IntPoint Int Color
  | Triangle IntPoint IntPoint IntPoint Color
  | Parallel IntPoint IntPoint Int Color

-- My coordinates are in Ints,
-- (0,0) starts at top left corner instead of middle
-- And the unit is defined as a size of a small square, that is relative to the window size
translateCoordinates :: Position -> Point
translateCoordinates (x, y) =
  ( fromIntegral $ (x * unitSize) - windowWidth `div` 2
  , fromIntegral $ windowHeight `div` 2 - (y * unitSize))

(x, y) `addC` a = (x + a, y + a)

draw :: Shape -> Picture
draw (Square (x, y) size clr) =
  Color clr
    $ Polygon
    $ translateCoordinates
        <$> [(x, y), (x + size, y), (x + size, y + size), (x, y + size)]
draw (Triangle a b c clr) =
  Color clr $ Polygon $ translateCoordinates <$> [a, b, c]
draw (Parallel (x1, y1) (x2, y2) len clr) =
  Color clr
    $ Polygon
    $ translateCoordinates
        <$> [(x1, y1), (x2, y2), (x2 + len, y2), (x1 + len, y1)]

drawOutline :: Shape -> Picture
drawOutline (Square (x, y) size _) =
  Scale 1.1 1.1
    $ Color black
    $ Line
    $ translateCoordinates
        <$> [(x, y), (x + size, y), (x + size, y + size), (x, y + size), (x, y)]
drawOutline (Triangle a b c _) =
  Scale 1.1 1.1 $ Color black $ Line $ translateCoordinates <$> [a, b, c, a]
drawOutline (Parallel {}) = blank

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

move :: Dir -> Shape -> Shape
move d (Square p size clr) = Square (p `add` moveOffset d) size clr
move d (Triangle p1 p2 p3 clr) =
  Triangle
    (p1 `add` moveOffset d)
    (p2 `add` moveOffset d)
    (p3 `add` moveOffset d)
    clr
move d (Parallel a b len clr) =
  Parallel (a `add` moveOffset d) (b `add` moveOffset d) len clr
