{- cabal:
    build-depends: base, network, containers
-}
module Channel where

import Control.Concurrent
import Network.Socket
import System.IO
import Control.Monad

import Editor

type SharedWorld = MVar (Maybe ServerInfo)


parseServerWorld :: String -> ServerInfo
parseServerWorld = read



readerThread :: Handle -> SharedWorld -> IO ()
readerThread h shared = return ()
-- readerThread h shared = forever $ do
--     line <- hGetLine h
--     case parseServerWorld line of
--         Nothing -> pure ()
--         Just w  -> do
--             _ <- tryTakeMVar shared
--             putMVar shared (Just w)


updatesFromServer :: SharedWorld -> IO (Maybe ServerInfo)
updatesFromServer shared = do
    modifyMVar shared $ \m -> do
        pure (Nothing, m)

