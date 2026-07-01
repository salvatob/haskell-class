module GameLoop where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.IO


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

runGame :: LocalInfoRef -> SharedWorldRef -> IO()
runGame locatStateRef sharedServerRef = playIO
        FullScreen
        white 
        20
        initialWorld 
        (pure . render) 
        (eventWrapper locatStateRef) 
        (updIO sharedServerRef)

