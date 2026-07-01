module Server where
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import Control.Monad
import Data.Foldable
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Set.Internal as SI
import Network.Socket
import System.IO
import Text.Read

data Shard
  = S
  | E
  | N
  | W
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

type State = S.Set (Int, (Int, Int, Shard))

data InMsg
  = Poll
  | Cover [(Int, Int, Shard)]
  | Quit
  deriving (Show, Read)

data OutMsg
  = Stats [(Int, Int, Shard, Bool)]
  | Error
  deriving (Show, Read)

data ServerCom = ServerCom
  { inChan :: Chan (Int, InMsg)
  , outChan :: TChan OutMsg
  , clientIdCtr :: MVar Int
  }

newServerCom = ServerCom <$> newChan <*> newBroadcastTChanIO <*> newMVar 0

splitAround i s =
  let (l, (_, r)) =
        S.spanAntitone ((<= i) . fst) <$> S.spanAntitone ((< i) . fst) s
   in (l, r)

makeStats st =
  map (\((x, y, s), n) -> (x, y, s, n >= threshold)) $ M.assocs counts
  where
    counts = foldl' cnt M.empty . map snd $ toList st
    cnt m x = M.alter ((<|> Just 0) . fmap succ) x m
    threshold = maximum counts `div` 2

workerThread :: ServerCom -> State -> IO ()
workerThread com state = do
  let broadcast x = atomically $ writeTChan (outChan com) x
      continue s = broadcast (Stats $ makeStats s) >> workerThread com s
  msg <- readChan (inChan com)
  case msg of
    (_, Poll) -> continue state
    (i, Cover cs) ->
      let (l, r) = splitAround i state
       in continue $ l `SI.merge` S.fromList (map (i, ) cs) `SI.merge` r
    (i, Quit) ->
      let (l, r) = splitAround i state
       in continue $ l `SI.merge` r

main =
  withSocketsDo $ do
    com <- newServerCom
    worker <- forkIO $ workerThread com S.empty
    E.bracket open close $ mainLoop com
  where
    open = do
      sock <- socket AF_INET Stream 0
      setSocketOption sock ReuseAddr 1
      bind sock $ SockAddrInet 10042 0
      listen sock 10
      return sock

mainLoop com sock =
  forever $ do
    (c, _) <- accept sock
    forkIO $ E.bracket (setupConn c) hClose $ runConn com

setupConn :: Socket -> IO Handle
setupConn c = do
  h <- socketToHandle c ReadWriteMode
  hSetBuffering h NoBuffering
  return h

runConn :: ServerCom -> Handle -> IO ()
runConn com h = do
  clientId <- takeMVar $ clientIdCtr com
  clientIdCtr com `putMVar` succ clientId
  myChan <- atomically $ dupTChan (outChan com)
  sender <- forkIO . forever $ do
    msg <- atomically (readTChan myChan)
    putStrLn $ "--> (" ++ show clientId ++ ") " ++ show msg
    hPrint h msg
  let loop = do
        -- the filter here removes the \r (and other ugly stuff) typically sent
        -- by telnet and other manual neworkish tools
        cmd <- filter (>= ' ') <$> hGetLine h
        putStrLn $ "<-- (" ++ show clientId ++ ") " ++ show cmd
        case readMaybe cmd of
          Just Quit -> pure ()
          Just x -> do
            writeChan (inChan com) (clientId, x)
            loop
          Nothing
            | null cmd -> loop
          _ -> do
            hPrint h Error
            loop
  E.finally loop $ do
    killThread sender
    writeChan (inChan com) (clientId, Quit)
