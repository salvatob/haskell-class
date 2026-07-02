module Channel where
{- cabal:
    build-depends: base, network, containers
-}

import Control.Concurrent
import Network.Socket
import System.IO
import Control.Monad
import Graphics.Gloss.Interface.Pure.Game



-- import Data.Time.Clock.POSIX (getPOSIXTime)

import Editor
import Protocol
import qualified Data.Set as Set

type SharedWorldRef = MVar ServerCoverage


-- blocks the thread, when recieving new info from server, it passes it into the shared MVar
readerThread :: Handle -> SharedWorldRef -> IO ()
readerThread h shared = forever $ do
    line <- hGetLine h
    -- for debug reasons i wanna see it
    putStrLn $ "s: " ++ line
    case parseServerCoverage line of
        Nothing -> pure ()
        Just w  -> do
            _ <- tryTakeMVar shared
            putMVar shared w


type LocalInfoRef = MVar ClientCoverage


getTiles :: St -> [Tile]
getTiles (Selecting t ts) = t:ts
getTiles (Dragging t ts) = t:ts

toClientCoverage :: St -> ClientCoverage
toClientCoverage st =
  let
    tiles = getTiles st
  in
    Set.fromList
      [ (tx + dx, ty + dy, shard)
      | Tile (tx, ty) subs <- tiles
      , ((dx, dy), shards) <- subs
      , shard <- shards
      ]



-- wrap the event handling so I can do things to teh world after each update
eventWrapper :: LocalInfoRef -> Event -> World -> IO World
eventWrapper cRef e w = do
    newWorld <- eventIO e w
    let coverage = toClientCoverage (local newWorld)

    -- replaces the value with the new one
    -- maybe isnt the best way to do it, but it is non-blocking, 
    -- and even if there was a concern of a data race, it would not be a critical bug 
    _ <- tryTakeMVar cRef
    _ <- tryPutMVar cRef coverage
    
    return newWorld


-- randomInt :: Int -> IO Int
-- randomInt n = do
--     t <- getPOSIXTime
--     return (floor (t * 1000000) `mod` n)

-- oneInTwentyTrue :: IO Bool
-- oneInTwentyTrue = do
--     i <- randomInt 20
--     return $ i == 0

-- this thread is gonna be blocked until the coverage statistic is filled
writerThread :: Handle -> LocalInfoRef -> IO ()
writerThread h info = forever $ do
    coverage <- takeMVar info

    -- debugging you know...
    putStrLn $ "C: " ++ showClientCoverage coverage 

    hPutStrLn h (showClientCoverage coverage)

