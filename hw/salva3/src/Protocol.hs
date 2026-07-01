module Protocol where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map



data Shard
  = S
  | E
  | N
  | W
  deriving (Show, Read, Eq, Bounded, Enum, Ord)


type SubTile = (Int, Int, Shard)

type ClientCoverage = Set.Set SubTile

type ServerCoverage = Map.Map SubTile Int
