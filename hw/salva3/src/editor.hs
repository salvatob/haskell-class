
import Control.Monad
import Data.List
import System.Exit (exitSuccess)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Shard
  = S
  | E
  | N
  | W
  deriving (Show, Read, Eq, Bounded, Enum)

type Pos = (Int, Int)

type TileData = [(Pos, [Shard])]

data Tile =
  Tile Pos TileData
  deriving (Show)

data St
  = Selecting Tile [Tile]
  | Dragging Tile [Tile]
  deriving (Show)

addCx (a, b) (c, d) = (a + c, b + d)

succBnd c
  | c == maxBound = minBound
  | otherwise = succ c

flipVShard N = S
flipVShard S = N
flipVShard x = x

rotL = map $ \((x, y), ss) -> ((negate y, x), map succBnd ss)

rotR = rotL . rotL . rotL

flipV = map $ \((x, y), ss) -> ((x, negate y), map flipVShard ss)

flipH = rotL . flipV . rotR

startingTiles =
  [ Tile (0, 0) [((0, 0), [S, E, N, W])]
  , Tile (1, 0) [((0, 0), [N, W])]
  , Tile (1, 0) [((0, 0), [S, E]), ((1, 0), [N, W])]
  , Tile (2, 0) [((0, 0), [S, E]), ((1, 0), [S, W])]
  , Tile (3, 0) [((0, 0), [E, N])]
  , Tile (4, 0) [((0, 0), [S, E, N, W])]
  ]

cplx f (a, b) = f (fromIntegral a) (fromIntegral b)

unSq s _ _ _ S = s
unSq _ e _ _ E = e
unSq _ _ n _ N = n
unSq _ _ _ w W = w

drawSq =
  Pictures
    . map
        (unSq
           (Polygon [(0, 0), (1, 0), (0.5, 0.5)])
           (Polygon [(1, 0), (1, 1), (0.5, 0.5)])
           (Polygon [(1, 1), (0, 1), (0.5, 0.5)])
           (Polygon [(0, 1), (0, 0), (0.5, 0.5)]))

renderTile (Tile pos subs) =
  cplx Translate pos
    $ Pictures [cplx Translate spos $ drawSq sq | (spos, sq) <- subs]

renderTiles = Pictures . map renderTile

initialState = Selecting t ts
  where
    (t:ts) = startingTiles

render = Scale 100 100 . go
  where
    go (Selecting t ts) =
      Pictures [renderTiles ts, Color (greyN 0.2) $ renderTile t]
    go (Dragging t ts) = Pictures [renderTiles ts, Color red $ renderTile t]

event (EventKey (SpecialKey k) Down _ _) st = skEvent k st
event (EventKey (Char c) Down _ _) st = ltrEvent c st
event _ tiles = tiles

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

ltrEvent k (Dragging (Tile p ss) ts)
  | k == 'z' = Dragging (Tile p $ rotL ss) ts
  | k == 'c' = Dragging (Tile p $ rotR ss) ts
  | k == 'h' = Dragging (Tile p $ flipH ss) ts
  | k == 'v' = Dragging (Tile p $ flipV ss) ts
ltrEvent _ st = st

upd _ tiles = tiles
updIO _ tiles = pure tiles

type SubTile = (Int, Int, Shard)

type ClientCoverage = Set.Set SubTile

type ServerCoverage = Map.Map SubTile Int


insertTile :: Tile -> ClientCoverage
insertTile (Tile (x,y), shards) = 
  let
    ((dx,dy), ss) = head shards
    set = Set.empty
    set1 = insert ((x+dx, y+dy, head ss)) set

    filled = insert (x,y,head tileData) s

-- toClientCoverage :: [Tile] -> ClientCoverage
-- toClientCoverage (t:_) = 
--   let
--     ((x, y), tileData) = t
--     first
--     s = Set.empty

--     filled = insert (x,y,head tileData) s

-- main = play FullScreen white 20 initialState render event upd
eventIO :: Event -> St -> IO St
eventIO (EventKey (SpecialKey KeyEsc) Down _ _) _ = do exitSuccess
eventIO e s = pure $ event e s

main = playIO FullScreen white 20 initialState (pure . render) eventIO updIO
