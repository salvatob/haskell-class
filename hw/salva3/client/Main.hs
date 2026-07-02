{-# LANGUAGE ScopedTypeVariables #-}
module Main where
{- cabal:
build-depends: base, network
-}

import Control.Concurrent
import Network.Socket
import System.IO
import Control.Exception

import Channel
import GameLoop
import ArgOpts

-- main =
--   withSocketsDo $ do
--     sock <- socket AF_INET Stream 0
--     addr <-
--       addrAddress . head
--         <$> getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "10042")
--     connect sock addr
--     h <- socketToHandle sock ReadWriteMode
--     hSetBuffering h NoBuffering
--     hPutStrLn h "Cover [(0,0,E), (0,1,W)]"
--     response <- hGetLine h
--     putStrLn $ "Received a line: " ++ response
--     hPutStrLn h "Poll"
--     threadDelay 1000000
--     response <- hGetLine h
--     putStrLn $ "Received another line: " ++ response
--     hPutStrLn h "Quit"
--     threadDelay 100000
--     hClose h

main :: IO ()
main = withSocketsDo $ do
    args <- parseCmdArgs

    sock <- socket AF_INET Stream 0

    addr <- addrAddress . head
        <$> getAddrInfo (Just defaultHints)
                        (Just (address args))
                        (Just (show $ port args))

    connect sock addr
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering

    sharedServerInfo <- newEmptyMVar
    sharedLocalInfo <- newEmptyMVar

    _ <- forkIO $ readerThread h sharedServerInfo
    
    _ <- forkIO $ writerThread h sharedLocalInfo
    
    let myWindow = ( windowWidth args, windowHeight args, windowX args, windowY args)
    -- let window = (800,600,200,200)

    finally
        (runGame sharedLocalInfo sharedServerInfo myWindow) 
        (do
            hPutStrLn h "Quit"
            putStrLn "C: ending connection."
            )
    -- runGame  sharedLocalInfo sharedServerInfo `catch` \(e :: SomeException) -> do
    --     putStrLn $ "Caught: " ++ displayException e
    --     throwIO e