module Main where
{- cabal:
build-depends: base, network
-}

import Control.Concurrent
import Network.Socket
import System.IO

import Channel
import GameLoop

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

main = withSocketsDo $ do
    sock <- socket AF_INET Stream 0

    addr <- addrAddress . head
        <$> getAddrInfo (Just defaultHints)
                        (Just "127.0.0.1")
                        (Just "10042")

    connect sock addr
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering

    shared <- newMVar Nothing

    _ <- forkIO $ readerThread h shared

    
    runGame shared