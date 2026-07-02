module Editor where
{- cabal:
    build-depends: base, gloss, containers
-}

import Control.Monad
import Data.List
import System.Exit (exitSuccess)
import Debug.Trace (trace)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Protocol
import Control.Exception

-- data Shard
--   = S
--   | E
--   | N
--   | W
--   deriving (Show, Read, Eq, Bounded, Enum, Ord)

type Pos = (Int, Int)

type TileData = [(Pos, [Shard])]

data Tile =
  Tile Pos TileData
  deriving (Show)

data St
  = Selecting Tile [Tile]
  | Dragging Tile [Tile]
  deriving (Show)

type ServerInfo = Int

data World = World {
  local :: St,
  server :: ServerCoverage
}

addCx :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addCx (a, b) (c, d) = (a + c, b + d)

succBnd :: (Eq a, Bounded a, Enum a) => a -> a
succBnd c
  | c == maxBound = minBound
  | otherwise = succ c

flipVShard :: Shard -> Shard
flipVShard N = S
flipVShard S = N
flipVShard x = x

rotL = map $ \((x, y), ss) -> ((negate y, x), map succBnd ss)

rotR = rotL . rotL . rotL

flipV = map $ \((x, y), ss) -> ((x, negate y), map flipVShard ss)

flipH = rotL . flipV . rotR

startingTiles :: [Tile]
startingTiles =
  [ Tile (0, 0) [((0, 0), [S, E, N, W])]
  , Tile (1, 0) [((0, 0), [N, W])]
  , Tile (1, 0) [((0, 0), [S, E]), ((1, 0), [N, W])]
  , Tile (2, 0) [((0, 0), [S, E]), ((1, 0), [S, W])]
  , Tile (3, 0) [((0, 0), [E, N])]
  , Tile (4, 0) [((0, 0), [S, E, N, W])]
  ]

cplx :: (Integral a1, Integral a2, Num t1, Num t2) => (t1 -> t2 -> t3) -> (a1, a2) -> t3
cplx f (a, b) = f (fromIntegral a) (fromIntegral b)

unSq :: p -> p -> p -> p -> Shard -> p
unSq s _ _ _ S = s
unSq _ e _ _ E = e
unSq _ _ n _ N = n
unSq _ _ _ w W = w

drawSq :: [Shard] -> Picture
drawSq =
  Pictures
    . map
        (unSq
           (Polygon [(0, 0), (1, 0), (0.5, 0.5)])
           (Polygon [(1, 0), (1, 1), (0.5, 0.5)])
           (Polygon [(1, 1), (0, 1), (0.5, 0.5)])
           (Polygon [(0, 1), (0, 0), (0.5, 0.5)]))

renderTile :: Tile -> Picture
renderTile (Tile pos subs) =
  cplx Translate pos
    $ Pictures [cplx Translate spos $ drawSq sq | (spos, sq) <- subs]

renderTiles :: [Tile] -> Picture
renderTiles = Pictures . map renderTile

initialWorld :: World
initialWorld = World local server
  where
    (t:ts) = startingTiles
    local = Selecting t ts
    server = []

render :: (Int, Int) -> World -> Picture
render (w, h) (World l s) =
  let
    tileSize = fromIntegral (min w h) / 10
    scene =
      Pictures
        [ renderServer s
        , renderLocal l
        ]
  in
    Scale tileSize tileSize scene


renderLocal :: St -> Picture
renderLocal = go
  where
    go (Selecting t ts) =
      Pictures [renderTiles ts, Color (greyN 0.2) $ renderTile t]
    go (Dragging t ts) =
      Pictures [renderTiles ts, Color red $ renderTile t]

renderServer :: ServerCoverage -> Picture
renderServer =
  Pictures . map renderShard
  where
    majorityColor = makeColorI 230 210 120 255
    minorityColor = light (greyN 0.7)

    renderShard (x, y, shard, isMajority) =
      Color (if isMajority then majorityColor else minorityColor) $
        Translate (fromIntegral x) (fromIntegral y) $
          drawSq [shard]

localEvent :: Event -> St -> St
localEvent (EventKey (SpecialKey k) Down _ _) st = skEvent k st
localEvent (EventKey (Char c) Down _ _) st = ltrEvent c st
localEvent _ tiles = tiles

skEvent :: SpecialKey -> St -> St
skEvent KeyTab (Selecting t ts) =
  let (t':ts') = ts ++ [t]
   in Selecting t' ts'
skEvent KeySpace (Selecting t ts) = Dragging t ts
skEvent KeySpace (Dragging t ts) = Selecting t ts
skEvent arr (Dragging (Tile p ss) ts)
  | KeyRight <- arr = Dragging (Tile (addCx p (1, 0)) ss) ts
  | KeyLeft <- arr = Dragging (Tile (addCx p (-1, 0)) ss) ts
  | KeyUp <- arr = Dragging (Tile (addCx p (0, 1)) ss) ts
  | KeyDown <- arr = Dragging (Tile (addCx p (0, -1)) ss) ts
skEvent _ st = st

ltrEvent :: Char -> St -> St
ltrEvent k (Dragging (Tile p ss) ts)
  | k == 'z' = Dragging (Tile p $ rotL ss) ts
  | k == 'c' = Dragging (Tile p $ rotR ss) ts
  | k == 'h' = Dragging (Tile p $ flipH ss) ts
  | k == 'v' = Dragging (Tile p $ flipV ss) ts
ltrEvent _ st = st




eventIO :: Event -> World -> IO World
-- eventIO (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
eventIO (EventKey (SpecialKey KeyEsc) Down _ _) _ = throwIO (userError "quit")
eventIO (EventKey (Char 'i') Down _ _) w = pure $ w {server = []}
eventIO e s = pure $ s { local = localEvent e (local s) }
