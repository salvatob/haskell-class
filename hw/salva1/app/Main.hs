#!/usr/bin/env cabal
module Main where
{- cabal:
build-depends: base, gloss
ghc-options: -threaded
-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Shapes

{- The state of the world is represented simply with a single number -}
initialWorld :: Integer
initialWorld = 5

{- This function draws the world (integer `n`) as a Gloss `Picture` type.
 - (see the documentation for the Picture type on Hoogle.) -}
drawWorld :: Integer -> Picture
drawWorld n =
  Color black
    $ Pictures
    $ flip map [1 .. n] $ \i ->
    Translate (100 * fromInteger i - 550) 0 $ Pictures [ThickCircle 50 10]

{- This function changes the world (integer `n`) based on an incoming event, in
 - our case arrow keys being pressed.a -}
handleEvent :: (Ord a, Num a) => Event -> a -> a
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) n = max 0 $ n - 1
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) n = min 10 $ n + 1
handleEvent _ n = n -- we ignore all other events

updateWorld :: p -> a -> a
updateWorld _ = id

{- Function `play` from gloss connects the functions for managing and drawing
 - the world state and runs them on the initial state, with a selected
 - background color and framerate. All other things are handled by the Gloss
 - library. -}

myWindow :: Display
myWindow = InWindow "tadyto je muj program" (800, 600) (100, 80)

main :: IO ()
-- main = play myWindow white 25 initialWorld drawWorld handleEvent updateWorld
main = display myWindow white myPicture