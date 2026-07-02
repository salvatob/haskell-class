{-# LANGUAGE ScopedTypeVariables #-}

module Main where

{- cabal:
build-depends: base, network
-}
import Control.Concurrent
import Control.Exception
import Network.Socket
import System.IO

import ArgOpts
import Channel
import GameLoop

main :: IO ()
main =
  withSocketsDo $ do
    args <- parseCmdArgs
    sock <- socket AF_INET Stream 0
    addr <-
      addrAddress . head
        <$> getAddrInfo
              (Just defaultHints)
              (Just (address args))
              (Just (show $ port args))
    connect sock addr
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering
    sharedServerInfo <- newEmptyMVar
    sharedLocalInfo <- newEmptyMVar
    _ <- forkIO $ readerThread h sharedServerInfo
    _ <- forkIO $ writerThread h sharedLocalInfo
    let myWindow =
          (windowWidth args, windowHeight args, windowX args, windowY args)
    runGame sharedLocalInfo sharedServerInfo myWindow
