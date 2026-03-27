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


