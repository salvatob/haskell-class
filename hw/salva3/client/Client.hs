
{- cabal:
build-depends: base, network
-}
module Main where

import Control.Concurrent
import Network.Socket
import System.IO

main =
  withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    addr <-
      addrAddress . head
        <$> getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "10042")
    connect sock addr
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering
    hPutStrLn h "Cover [(0,0,E), (0,1,W)]"
    response <- hGetLine h
    putStrLn $ "Received a line: " ++ response
    hPutStrLn h "Poll"
    threadDelay 1000000
    response <- hGetLine h
    putStrLn $ "Received another line: " ++ response
    hPutStrLn h "Quit"
    threadDelay 100000
    hClose h
