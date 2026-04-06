module Constants where

-- defines a size of a unit in pixels
-- program works perfectly well with different window size or more "cells/units" for the shapes 
unitSize = 200 :: Int

unitWidth = 4 :: Int

unitHeight = 3 :: Int

-- defines the window size in units
windowWidth = unitWidth * unitSize :: Int

windowHeight = unitHeight * unitSize :: Int

type IntPoint = (Int, Int)
