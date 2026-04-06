{-# LANGUAGE InstanceSigs #-}

module Serialization where

import Graphics.Gloss.Data.Color

newtype SRColor =
  SRColor Color

instance Show SRColor where
  show :: SRColor -> String
  show (SRColor c) = show $ rgbaOfColor c


-- this code was significanly assisted by chatgpt
instance Read SRColor where
    readsPrec :: Int -> ReadS SRColor
    readsPrec _ s =
        [ (SRColor $ makeColor r g b a, rest)
        | ((r,g,b,a), rest) <- reads s
        ]
