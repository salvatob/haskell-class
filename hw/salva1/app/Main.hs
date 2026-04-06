module Main where

{- cabal:
build-depends: base, gloss
ghc-options: -threaded
-}
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.IO.Interact

import Debug.Trace (trace)
import System.Exit (exitSuccess)

import Serialization
import Constants
import Helpers
import Shapes
import World

initialSprites :: [Sprite]
initialSprites =
  [
-- sprites can be created either with these nice constructors
     smallSquare violet (0, 2)
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
-- events that move selected sprite
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) w = changeSelected (move U) w
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) w = changeSelected (move D) w
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) w = changeSelected (move L) w
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) w = changeSelected (move R) w

-- picking up and putting down of selected sprite
handleEvent (EventKey (SpecialKey KeyTab) Down _ _) w = rotateSelection w
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) w = handlePickUp w


-- clockwise rotation
handleEvent (EventKey (Char 'c') Down _ _) w = changeSelected rotateSprite w
-- counter clockwise rotation
handleEvent (EventKey (Char 'z') Down _ _) w = changeSelected (repeatF rotateSprite 3) w

handleEvent (EventKey (Char 'v') Down _ _) w = changeSelected flipSprite w

-- prints the current state into the console. Mostly for debugging
handleEvent (EventKey (Char 'p') Down _ _) w = trace (show w) w

handleEvent _ w = w -- we ignore all other events


saveFile :: FilePath
saveFile = "./save.txt"


-- saves world into the selected file
handleEventIO :: Event -> World -> IO World
handleEventIO (EventKey (Char 's') Down _ _) w = do
    _ <- saveWorld w saveFile
    return w

-- loads the world in a selected file
handleEventIO (EventKey (Char 'l') Down _ _) _ = loadWorld saveFile

-- exit on pressing ESCAPE
handleEventIO (EventKey (SpecialKey KeyEsc) Down _ _) _ = do
  exitSuccess
-- wrap normal events into IO
handleEventIO e w = pure $ handleEvent e w


myWindow :: Display
myWindow =
  InWindow "tadyto je muj program" (windowWidth, windowHeight) (100, 80)


main :: IO ()
main = playIO myWindow white 25 initialWorld drawWorldIO handleEventIO (const pure)
