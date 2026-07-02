module GameLoop where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.IO
-- import Graphics.Gloss.Internals.Interface.Game
-- import Graphics.Gloss.Internals.Interface.Backend


import Editor
import Channel
import Control.Concurrent



updIO :: SharedWorldRef -> Float -> World -> IO World
updIO shared _ world = do
    mCoverage <- tryReadMVar shared
    pure $
        case mCoverage of
            Nothing ->
                world
            Just s ->
                world { server = s }


-- myPlay  display backColor simResolution
--         worldStart worldToPicture worldHandleEvent worldAdvance

--  = playWithBackendIO defaultBackendState
--         display backColor simResolution
--         worldStart worldToPicture worldHandleEvent worldAdvance
--         False

runGame :: LocalInfoRef -> SharedWorldRef -> (Int, Int, Int, Int) -> IO()
runGame locatStateRef sharedServerRef (w,h,x,y) = do
    print (w,h,x,y)
    playIO
        -- FullScreen
        (InWindow "logo-webapp" (w, h) (x, y))
        white 
        20
        initialWorld 
        (pure . render (w, h)) 
        (eventWrapper locatStateRef) 
        (updIO sharedServerRef)

