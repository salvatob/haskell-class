module Shapes where

import Graphics.Gloss

type Position = (Float, Float)

data Shape =  Square Point Float Color
            | Triangle Point Point Point Color

draw :: Shape -> Picture
draw (Square (x, y) size clr) = Color clr $ Polygon [(x,y),(x+size,y),(x+size,y+size),(x,y+size)]
draw (Triangle a b c clr) = Color clr $ Polygon [a,b,c]


drawOutline :: Shape -> Picture
drawOutline (Square (x, y) size _) = Scale 1.1 1.1 $ Color black $ Line [(x,y),(x+size,y),(x+size,y+size),(x,y+size),(x,y)]
drawOutline (Triangle a b c _) = Scale 1.1 1.1 $ Color black $ Line [a,b,c,a]



data Dir = L|R|U|D

offset = 50 :: Float

moveOffset :: Dir -> (Float, Float)
moveOffset L = (-offset, 0)
moveOffset R = (offset, 0)
moveOffset U = (0, offset)
moveOffset D = (0, -offset)

add :: (Num n) => (n,n) -> (n,n) -> (n,n)
add (a,b) (x,y) = (a+x,b+y)

move :: Dir -> Shape -> Shape
move d (Square p size clr) = Square (p `add` moveOffset d) size clr


move d (Triangle p1 p2 p3 clr) = Triangle (p1 `add` moveOffset d) 
                                          (p2 `add` moveOffset d)
                                          (p3 `add` moveOffset d)
                                          clr

