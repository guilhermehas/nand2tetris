module Main where

import Data.List.Split
import qualified Data.Map as Map
import Data.Char
import Data.List

removeSpacesComments :: String -> String
removeSpacesComments = (filter (/=' ')) . head . (splitOn "//")

splitCodeClean :: String -> [String]
splitCodeClean = (map removeSpacesComments) . (splitOn ['\n'])

data Line = Local String | ACommand String | DCommand String String

parseLine :: String -> Line
parseLine s = 
    case head s of
      '(' -> Local $ init $ tail s
      '@' -> ACommand $ tail s
      otherwise -> let dParts = splitOn ";" s in DCommand (dParts !! 0) (dParts !! 1)

type TableSymbols = ([String], [Int])

tableValueList :: TableSymbols
tableValueList = unzip $ [("R" ++ show n, n) | n <- [0..15]] ++
    [("SCREEN", 16384), ("KBD", 24576), ("SP", 0)] ++
    [("LCL", 1), ("ARG", 2), ("THIS", 3), ("THAT", 4)]

type CountTableSymbol = (Int, [String], [Int])

nextStepLabel :: Line -> CountTableSymbol -> CountTableSymbol
nextStepLabel (Local s) (count, listS, listI) = (count, [s] ++ listS, [count] ++ listI )
nextStepLabel _ (count, listS, listI) = (count + 1, listS, listI)

nextStepSymbol :: Line -> CountTableSymbol -> CountTableSymbol
nextStepSymbol (ACommand s) (count, listS, listI) = 
    if not (isDigit $ head s) && not (s `elem` listS)
       then (count+1, [s] ++ listS, [count] ++ listI)
       else (count, listS, listI) 

toTableValue :: [Line] -> TableSymbols
toTableValue table = (newLabels2, newValues2)
    where (labels, values) = tableValueList
          (_, newLabels, newValues) = foldr nextStepLabel (0, labels, values) table
          (_, newLabels2, newValues2) = foldr nextStepSymbol (16, newLabels2, newValues2) table

data IntegerLine = ACommandI Int | DCommandI String String

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

convertLine :: TableSymbols -> Line -> Maybe IntegerLine
convertLine _ (Local _) = Nothing
convertLine (symbols, values) (ACommand s) = 
    if isDigit $ head s 
        then Just $ ACommandI $ read s 
        else Just $ ACommandI $ values !! (fromJust $ elemIndex s symbols)
convertLine _ (DCommand d1 d2) = Just $ DCommandI d1 d2

main :: IO ()
main = putStrLn "Hello, Haskell!"
