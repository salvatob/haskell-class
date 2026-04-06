module World where

import Graphics.Gloss
import Helpers
import Serialization
import Shapes

data World = World
  { shapes :: [Sprite]
  , index :: Int -- note that index will overflow by one, representing state, where no sprite is selected
  , pickedUp :: Bool -- if the 
  }
  -- World [Sprite] Int Bool
 deriving (Show, Read)

-- increment the index of the selected shape
rotateSelection :: World -> World
rotateSelection (World s i False) = World s newI False
  where
    newI = (i + 1) `mod` (length s + 1)
rotateSelection w = w -- if a shape is selected, do not rotate


handlePickUp :: World -> World
handlePickUp w
   -- if index is not on any sprite, do not pickup
  | index w >= length (shapes w) = w {pickedUp = False}
  | otherwise = w {pickedUp = not (pickedUp w)}


changeSelected :: (Sprite -> Sprite) -> World -> World
changeSelected _ (World s i False) = World s i False -- if shape is not picked up, dont do anything
changeSelected f (World sprite i True)
  | i >= length sprite = World sprite i True -- do nothing if index is out of bounds
  | otherwise = World newS i True
  where
    newS = changeAt f i sprite


-- highlight sprite so we know what will be moved by player input
-- Sadly I just color the shape black in all cases, doesn't matter if it's picked up
-- but i decided to focus on other things than nice rendering  
highlightSelected :: World -> World
highlightSelected w =
  changeSelected (\s -> s {clr = SRColor black}) (w {pickedUp = True})

drawWorld :: World -> Picture
drawWorld = Pictures . map draw . shapes . highlightSelected

drawWorldIO :: World -> IO Picture
drawWorldIO = pure . drawWorld

saveWorld :: World -> FilePath -> IO ()
saveWorld w path = writeFile path (show w)

loadWorld :: FilePath -> IO World
loadWorld path = do
  content <- readFile path
  return $ read content
