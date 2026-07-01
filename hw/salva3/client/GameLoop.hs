module GameLoop where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game


import Editor
import Channel


-- doesnt even have to be IO since I just used trace xddd
updIO :: SharedWorld -> Float -> World -> IO World
-- updIO shared _ world = do 
--   newServerState <- updateFromServer

--   case newServerState of
--     Nothing -> pure world
--     Just s  -> pure (world {server = parseServerWorld s})

updIO shared _ world = do
    mCoverage <- updatesFromServer shared

    pure $
        case mCoverage of
            Nothing ->
                world

            Just s ->
                world { server = s }

runGame :: SharedWorld -> IO()
runGame shared = playIO FullScreen white 20 initialWorld (pure . render) eventIO (updIO shared)


