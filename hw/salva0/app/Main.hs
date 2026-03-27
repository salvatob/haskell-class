module Main where
import Data.Char (toLower, isAlpha)

countVowels :: String -> Int
countVowels = length . filter ( `elem` "aeiouy")

computeVowelPercentage :: String -> Int
computeVowelPercentage w = 100 * countVowels w `div` length w

filterLongWords :: String -> [String]
filterLongWords = filter ((>= 3) . length) . words

normalizeWords :: [String] -> String
normalizeWords =  filter isAlpha . fmap toLower . concat


main :: IO ()
main = interact $ (++"%") . show . computeVowelPercentage . normalizeWords . filterLongWords