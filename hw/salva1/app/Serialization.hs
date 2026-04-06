module Serialization where

import Graphics.Gloss.Data.Color

newtype SRColor = SRColor Color
    -- deriving (Show)

instance Show SRColor where
  -- show :: SRColor -> String
  show (SRColor c) = show $ rgbaOfColor c

instance Read SRColor where
    -- readPrec = do
    --     (r,g,b,a) <- readPrec
    --     return $ SRColor (makeColor r g b a)
    readsPrec _ s =
        [ (SRColor $ makeColor r g b a, rest)
        | ((r,g,b,a), rest) <- reads s
        ]

  -- read :: String -> SRColor
  -- read s = makeColor r g b a
  --   where 
  --     (r,g,b,a) = (read s :: (Float -> Float -> Float -> Float))
     