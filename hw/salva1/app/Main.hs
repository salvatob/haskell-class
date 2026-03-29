module Main where

{- cabal:
build-depends: base, gloss
ghc-options: -threaded
-}
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Constants
import Shapes
import World

initialWorld :: World
initialWorld = World [
  Square (0, 2) 1 violet,
  Square (1, 1) 1 red,
  Triangle (2, 0) (3, 0) (2, 1) green,
  Triangle (3, 0) (4, 0) (4, 1) azure,
  Triangle (4, 1) (4, 3) (3, 2) orange,
  Parallel (1,3) (2,2) 1 rose 
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
