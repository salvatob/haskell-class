module Protocol where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (stripPrefix)


data Shard
  = S
  | E
  | N
  | W
  deriving (Show, Read, Eq, Bounded, Enum, Ord)


type SubTile = (Int, Int, Shard)

type ClientCoverage = Set.Set SubTile

showClientCoverage :: ClientCoverage -> String
showClientCoverage c = "Cover " ++ show (Set.toList c)

type ServerCoverage = [(Int, Int, Shard, Bool)]

parseServerCoverage :: String -> Maybe ServerCoverage
parseServerCoverage str = do
    rest <- stripPrefix "Stats " str
    case reads rest of
        [(coverage, "")] -> Just coverage
        _                -> Nothing
