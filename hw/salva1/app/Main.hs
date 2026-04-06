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
import Helpers
import Serialization
import Debug.Trace (trace)

initialSprites :: [Sprite]
initialSprites =
-- sprites can be created either with thece nice constructors
  [ smallSquare violet (0, 2)
-- or from scratch
  , Sprite (Square 1) (1, 1) (SRColor red)
  , smallTriangle green (2, 0)
  , rotateSprite $ smallTriangle cyan (4, 0)
  , largeTriangle orange (3, 1)
  , parallelogram rose (2, 2) ((-1, 1), (1, 0))
  ]

initialWorld :: World
initialWorld = World initialSprites (length initialSprites) False


handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeyTab) Down _ _) w = rotateSelection w
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) w = handlePickUp w
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) w = changeSelected (move U) w
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) w =
  changeSelected (move D) w
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) w =
  changeSelected (move L) w
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) w =
  changeSelected (move R) w
handleEvent (EventKey (Char 'c') Down _ _) w = changeSelected rotateSprite w
handleEvent (EventKey (Char 'z') Down _ _) w =
  changeSelected (repeatF rotateSprite 3) w
handleEvent (EventKey (Char 'v') Down _ _) w = changeSelected flipSprite w
handleEvent _ n = n -- we ignore all other events

myWindow :: Display
myWindow =
  InWindow "tadyto je muj program" (windowWidth, windowHeight) (100, 80)

mySprite :: Sprite
-- mySprite = smallSquare green (2,1)
mySprite
  -- rotateSprite $
 = smallTriangle red (1, 1)

main :: IO ()
main = play myWindow white 25 initialWorld drawWorld handleEvent (const id)
-- main = display myWindow white $ Color blue $ Color green $ Line [(0,0), (390, 290)]
-- main = display myWindow white $ draw mySprite
