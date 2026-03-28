#!/usr/bin/env cabal
module Main where
{- cabal:
build-depends: base, gloss
ghc-options: -threaded
-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Shapes
import World



{- The state of the world is represented simply with a single number -}
mySquare :: Shape
mySquare = Square (-200,-120) 150 blue
myTri :: Shape
myTri = Triangle (0,0) (100,-50) (50, 100) red

initialWorld :: World
initialWorld = World [mySquare, myTri] 1

data arrowKey = KeyLeft|Keyright|KeyUp|KeyDown

handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeyTab) Down _ _) w = rotateSelection w
handleEvent (EventKey (SpecialKey arrowKey) Down _ _) w = rotateSelection w
handleEvent _ n = n -- we ignore all other events

updateWorld :: p -> a -> a
updateWorld _ = id

myWindow :: Display
myWindow = InWindow "tadyto je muj program" (800, 600) (100, 80)

-- myPicture :: Picture
-- myPicture = ThickLine (-100,-100) (200, 300) 4

main :: IO ()
main = play myWindow white 25 initialWorld drawWorld handleEvent updateWorld
-- main = display myWindow white myPicture