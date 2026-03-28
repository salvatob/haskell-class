
module Main where
{- cabal:
build-depends: base, gloss
ghc-options: -threaded
-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Shapes
import World
import Constants


initialWorld :: World
initialWorld = World [
  Square (1,1) 1 blue,
  Triangle (0,0) (1,0) (0, 1) red
  
  ] 0

handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeyTab) Down _ _) w = rotateSelection w

handleEvent (EventKey (SpecialKey KeyUp) Down _ _)    w = moveSelected U w
handleEvent (EventKey (SpecialKey KeyDown) Down _ _)  w = moveSelected D w
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _)  w = moveSelected L w
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) w = moveSelected R w

handleEvent _ n = n -- we ignore all other events

updateWorld :: p -> a -> a
updateWorld = const id

myWindow :: Display
myWindow = InWindow "tadyto je muj program" (windowWidth, windowHeight) (100, 80)

main :: IO ()
main = play myWindow white 25 initialWorld drawWorld handleEvent updateWorld
-- main = display myWindow white (Line [(0,0), (100, 200)])