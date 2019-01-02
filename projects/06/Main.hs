module Main where

import Data.List.Split
import qualified Data.Map as Map
import Data.Char
import Data.List
import qualified Data.HashMap.Strict as M

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

type TableSymbols = M.HashMap String Int

tableValueList :: TableSymbols
tableValueList = M.fromList $ [("R" ++ show n, n) | n <- [0..15]] ++
    [("SCREEN", 16384), ("KBD", 24576), ("SP", 0)] ++
    [("LCL", 1), ("ARG", 2), ("THIS", 3), ("THAT", 4)]

type CountTableSymbol = (Int, TableSymbols)

nextStepLabel :: Line -> CountTableSymbol -> CountTableSymbol
nextStepLabel (Local s) (count, table) = (count, M.insert s count table)
nextStepLabel _ (count, table) = (count + 1, table)

nextStepSymbol :: Line -> CountTableSymbol -> CountTableSymbol
nextStepSymbol (ACommand s) (count, table) = 
    if not (isDigit $ head s) && not (M.member s table)
       then (count+1, M.insert s count table)
       else (count, table) 

toTableValue :: [Line] -> TableSymbols
toTableValue lines = table2
    where table0 = tableValueList
          (_, table1) = foldr nextStepLabel (0, table0) lines
          (_, table2) = foldr nextStepSymbol (16, table1) lines

data IntegerLine = ACommandI Int | DCommandI String String

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

convertLine :: TableSymbols -> Line -> Maybe IntegerLine
convertLine _ (Local _) = Nothing
convertLine table (ACommand s) = 
    if isDigit $ head s 
        then Just $ ACommandI $ read s 
        else Just $ ACommandI $ fromJust $ M.lookup s table
convertLine _ (DCommand d1 d2) = Just $ DCommandI d1 d2

intToBin :: Int -> Int -> [Int]
intToBin 0 _ = []
intToBin size n = (n `mod` 2) : intToBin (size - 1) (n `div` 2)

comp :: M.HashMap String [Int]
comp = M.fromList [("0", [0, 1, 0, 1, 0, 1, 0]), ("1", [0, 1, 1, 1, 1, 1, 1]), ("-1", [0, 1, 1, 1, 0, 1, 0]), ("D", [0, 0, 0, 1, 1, 0, 0]), ("A", [0, 1, 1, 0, 0, 0, 0]), ("!D", [0, 0, 0, 1, 1, 0, 1]), ("!A", [0, 1, 1, 0, 0, 0, 1]), ("-D", [0, 0, 0, 1, 1, 1, 1]), ("-A", [0, 1, 1, 0, 0, 1, 1]), ("D+1", [0, 0, 1, 1, 1, 1, 1]), ("A+1", [0, 1, 1, 0, 1, 1, 1]), ("D-1", [0, 0, 0, 1, 1, 1, 0]), ("A-1", [0, 1, 1, 0, 0, 1, 0]), ("D+A", [0, 0, 0, 0, 0, 1, 0]), ("D-A", [0, 0, 1, 0, 0, 1, 1]), ("A-D", [0, 0, 0, 0, 1, 1, 1]), ("D&A", [0, 0, 0, 0, 0, 0, 0]), ("D|A", [0, 0, 1, 0, 1, 0, 1])]

main :: IO ()
main = putStrLn "Hello, Haskell!"
