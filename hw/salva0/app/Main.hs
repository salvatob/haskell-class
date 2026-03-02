module Main where
import Data.Char (toLower, isAlpha)

vowels :: String
vowels = "aeiouy"

isVowel :: Char -> Bool
isVowel c = c `elem` vowels

countVowels :: String -> Int
countVowels = length . filter isVowel

computeVowelPercentage :: String -> Int
computeVowelPercentage w = (countVowels w * 100) `div` length w

collectLetters :: String -> String
collectLetters str = concat $ ( filter ((>= 3) . length) . words)
        $ filter isAlpha $ fmap toLower str


main :: IO ()
main = interact $ (++"%") . show . computeVowelPercentage . collectLetters