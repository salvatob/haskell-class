-- {-# LANGUAGE InstanceSigs #-}
module Shapes where

-- import Graphics.Gloss.Data.Picture (Point, Picture)
import Graphics.Gloss



-- class Shape a where
--   draw  :: a -> Picture

type Position = (Float, Float)
-- data Square = Square Color Float (Float, Float)

data Shape =  Square Point Float Color
            | Triangle Point Point Point Color

draw :: Shape -> Picture
draw (Square (x, y) size clr) = Color clr $ Polygon [(x,y),(x+size,y),(x+size,y+size),(x,y+size)]
draw (Triangle a b c clr) = Color clr $ Polygon [a,b,c]

data Dir = L|R
            -- |U|D

offset = 50 :: Float

move :: Dir -> Shape -> Shape
move R (Square (x, y) size clr) = Square (x+offset, y) size clr
move R (Triangle (x1,y1) (x2,y2) (x3,y3) clr) = Triangle (x1+offset,y1) (x2+offset,y2) (x3+offset,y3) clr
move L (Square (x, y) size clr) = Square (x-offset, y) size clr
move L (Triangle (x1,y1) (x2,y2) (x3,y3) clr) = Triangle (x1-offset,y1) (x2-offset,y2) (x3-offset,y3) clr

