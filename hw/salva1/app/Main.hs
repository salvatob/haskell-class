module Main where

{- cabal:
build-depends: base, gloss
ghc-options: -threaded
-}
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

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



saveFile :: FilePath
saveFile = "./save.txt"


handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeyTab) Down _ _) w = rotateSelection w
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) w = handlePickUp w

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) w = changeSelected (move U) w
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) w = changeSelected (move D) w
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) w = changeSelected (move L) w
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) w = changeSelected (move R) w

-- clockwise rotation
handleEvent (EventKey (Char 'c') Down _ _) w = changeSelected rotateSprite w
-- counter clockwise rotation
handleEvent (EventKey (Char 'z') Down _ _) w = changeSelected (repeatF rotateSprite 3) w
handleEvent (EventKey (Char 'v') Down _ _) w = changeSelected flipSprite w


handleEvent (EventKey (Char 'p') Down _ _) w = trace (show $ (read $ show w :: World)) w
handleEvent _ w = w -- we ignore all other events

handleEventIO :: Event -> World -> IO World
handleEventIO (EventKey (Char 's') Down _ _) w = do
    _ <- saveWorld w saveFile
    return w

handleEventIO (EventKey (Char 'l') Down _ _) _ = loadWorld saveFile

handleEventIO e w = pure $ handleEvent e w

eventIO :: Event -> World -> IO World
eventIO _ = return


myWindow :: Display
myWindow =
  InWindow "tadyto je muj program" (windowWidth, windowHeight) (100, 80)

mySprite :: Sprite
-- mySprite = smallSquare green (2,1)
mySprite
  -- rotateSprite $
 = smallTriangle red (1, 1)



main :: IO ()
-- main = play myWindow white 25 initialWorld drawWorld handleEvent (const id)
main = playIO myWindow white 25 initialWorld drawWorldIO handleEventIO (const pure)
-- main = display myWindow white $ Color blue $ Color green $ Line [(0,0), (390, 290)]
-- main = display myWindow white $ draw mySprite
