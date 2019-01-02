module Main where

import Data.List.Split
import qualified Data.Map as Map
import Data.Char
import Data.List
import qualified Data.HashMap.Strict as M

removeSpacesComments :: String -> String
removeSpacesComments = (filter (/=' ')) . head . (splitOn "//")

splitCodeClean :: String -> [String]
splitCodeClean = (filter (/= "")) . (map removeSpacesComments) . (splitOn ['\n'])

data Line = Local String | ACommand String | DCommand String String String deriving (Show) 

parseLine :: String -> Line
parseLine s = 
    case head s of
      '(' -> Local $ init $ tail s
      '@' -> ACommand $ tail s
      otherwise -> let 
        dParts = splitOn ";" s 
        part0 = dParts !! 0
        isFirstJ = part0 !! 0 == 'J'
        in
            if isFirstJ then DCommand "" "" $ dParts !! 0
            else
                let cs = splitOn "=" part0
                    cs0 = cs !! 0
                    cs1 = if length cs >= 2 then cs !! 1 else ""
                 in
                    if length dParts == 1 
                       then DCommand cs0 cs1 ""
                       else DCommand cs0 cs1 $ dParts !! 1

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
nextStepSymbol _ ct = ct

toTableValue :: [Line] -> TableSymbols
toTableValue lines = table2
    where table0 = tableValueList
          (_, table1) = foldl (flip nextStepLabel) (0, table0) lines
          (_, table2) = foldl (flip nextStepSymbol) (16, table1) lines

data IntegerLine = ACommandI Int | DCommandI String String String deriving (Show) 

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

convertLine :: TableSymbols -> Line -> Maybe IntegerLine
convertLine _ (Local _) = Nothing
convertLine table (ACommand s) = 
    if isDigit $ head s 
        then Just $ ACommandI $ read s 
        else Just $ ACommandI $ fromJust $ M.lookup s table
convertLine _ (DCommand d1 d2 d3) = Just $ DCommandI d1 d2 d3

mapD :: (a -> Maybe b) -> [a] -> [b]
mapD f [] = []
mapD f (x : xs) =
    case fx of
      Nothing -> mapD f xs
      Just y -> y : mapD f xs
    where fx = f x

linesToIlines :: [Line] -> [IntegerLine]
linesToIlines lines = mapD (convertLine table) lines
    where table = toTableValue lines

intToBin :: Int -> Int -> [Int]
intToBin 0 _ = []
intToBin size n = (n `mod` 2) : intToBin (size - 1) (n `div` 2)

compM :: M.HashMap String [Int]
compM = M.fromList [("", [0, 0, 0, 0, 0, 0, 0]), ("0", [0, 1, 0, 1, 0, 1, 0]), ("1", [0, 1, 1, 1, 1, 1, 1]), ("-1", [0, 1, 1, 1, 0, 1, 0]), ("D", [0, 0, 0, 1, 1, 0, 0]), ("A", [0, 1, 1, 0, 0, 0, 0]), ("M", [1, 1, 1, 0, 0, 0, 0]), ("!D", [0, 0, 0, 1, 1, 0, 1]), ("!A", [0, 1, 1, 0, 0, 0, 1]), ("!M", [1, 1, 1, 0, 0, 0, 1]), ("-D", [0, 0, 0, 1, 1, 1, 1]), ("-A", [0, 1, 1, 0, 0, 1, 1]), ("-M", [1, 1, 1, 0, 0, 1, 1]), ("D+1", [0, 0, 1, 1, 1, 1, 1]), ("A+1", [0, 1, 1, 0, 1, 1, 1]), ("M+1", [1, 1, 1, 0, 1, 1, 1]), ("D-1", [0, 0, 0, 1, 1, 1, 0]), ("A-1", [0, 1, 1, 0, 0, 1, 0]), ("M-1", [1, 1, 1, 0, 0, 1, 0]), ("D+A", [0, 0, 0, 0, 0, 1, 0]), ("D+M", [1, 0, 0, 0, 0, 1, 0]), ("D-A", [0, 0, 1, 0, 0, 1, 1]), ("D-M", [1, 0, 1, 0, 0, 1, 1]), ("A-D", [0, 0, 0, 0, 1, 1, 1]), ("M-D", [1, 0, 0, 0, 1, 1, 1]), ("D&A", [0, 0, 0, 0, 0, 0, 0]), ("D&M", [1, 0, 0, 0, 0, 0, 0]), ("D|A", [0, 0, 1, 0, 1, 0, 1]), ("D|M", [1, 0, 1, 0, 1, 0, 1])]

boolToInt True = 1
boolToInt False = 0

fromDict s d = fromJust $ M.lookup s d

toDest :: String -> [Int]
toDest s = [f 'A', f 'D', f 'M']
    where 
        f x = boolToInt $ x `elem` s

jumpM :: M.HashMap String [Int]
jumpM = M.fromList [("", [0, 0, 0]), ("JGT", [0, 0, 1]), ("JEQ", [0, 1, 0]), ("JGE", [0, 1, 1]), ("JLT", [1, 0, 0]), ("JNE", [1, 0, 1]), ("JLE", [1, 1, 0]), ("JMP", [1, 1, 1])]

convertCommand :: IntegerLine -> [Int]
convertCommand (ACommandI s) = reverse $ intToBin 16 s
convertCommand (DCommandI dest comp jump) = [1, 1, 1] ++ fromDict comp compM ++ toDest dest ++ fromDict jump jumpM

compiler :: String -> [[Int]]
compiler = map convertCommand . linesToIlines . map parseLine . splitCodeClean

main :: IO ()
main = do 
    contents <- getContents
    putStr $ intercalate ['\n'] $ map (concat . map show) $ compiler contents
