module Channel where
{- cabal:
    build-depends: base, network, containers
-}

import Control.Concurrent
import Network.Socket
import System.IO
import Control.Monad
import Graphics.Gloss.Interface.Pure.Game



import Data.Time.Clock.POSIX (getPOSIXTime)

import Editor
import Protocol

type SharedWorldRef = MVar ServerInfo


parseServerWorld :: String -> Maybe ServerInfo
parseServerWorld = read


-- blocks the thread, when recieving new info from server, it passes it into the shared MVar
readerThread :: Handle -> SharedWorldRef -> IO ()
readerThread h shared = forever $ do
    line <- hGetLine h
    case parseServerWorld line of
        Nothing -> pure ()
        Just w  -> do
            _ <- tryTakeMVar shared
            putMVar shared w


-- updatesFromServer :: SharedWorldRef -> IO ServerInfo
-- updatesFromServer shared = 
--     readMVar shared


type LocalInfoRef = MVar ClientCoverage


-- wrap the event handling so I can do things to teh world after each update
eventWrapper :: LocalInfoRef -> Event -> World -> IO World
eventWrapper cRef e w = do
    newWorld <- eventIO e w
    let coverage = toClientCoverage (local newWorld)
    putMVar cRef coverage
    return newWorld


randomInt :: Int -> IO Int
randomInt n = do
    t <- getPOSIXTime
    return (floor (t * 1000000) `mod` n)

oneInTwentyTrue :: IO Bool
oneInTwentyTrue = do
    i <- randomInt 20
    return $ i == 0

-- this thread is gonna be blocke duntil the coverage statistic is filled
writerThread :: Handle -> LocalInfoRef -> IO ()
writerThread h info = do
    -- shouldPrint <- oneInTwentyTrue
    coverage <- takeMVar info
    hPrint h coverage
    -- when shouldPrint $ hPrint h coverage

