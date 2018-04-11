{-# LANGUAGE TemplateHaskell #-}
module Sudoku where

import Data.List
import Data.Maybe
import Data.Char
-------------------------------------------------------------------------

data Sudoku = Sudoku [[Maybe Int]]
 deriving ( Eq )

instance Show Sudoku where
  show (Sudoku ls) = show [ [ f m | m <- row] | row <- ls]
    where
      f m = fromMaybe "_" $ fmap (show) m

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = 
    Sudoku
    [if x <= 8 then replicate 9 Nothing else [] | x <- [0..8]]

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku x = (checkRowLength (rows x)) == 9 && checkIntegers (rows x) && checkNumbers (rows x)

checkRowLength :: [[Maybe Int]] -> Int
checkRowLength x = length x 

checkIntegers :: [[Maybe Int]] -> Bool
checkIntegers [] = True
checkIntegers (h:t) = (length h == 9) && (checkIntegers t)

checkNumbers :: [[Maybe Int]] -> Bool
checkNumbers [] = True
checkNumbers (h:t) = (checkValue h) && (checkNumbers t)

checkValue :: [Maybe Int] -> Bool
checkValue [] = True
checkValue (h:t) = (h > Just 0 && h < Just 10 || h == Nothing) && checkValue t

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved x = checkNothingLists (rows x)

checkNothingLists :: [[Maybe Int]] -> Bool
checkNothingLists [] = True
checkNothingLists (h:t) = (checkNothing h) && (checkNothingLists t)

-- If there are no nothings, return true, else return false
checkNothing :: [Maybe Int] -> Bool
checkNothing [] = True
checkNothing (h:t) = (h /= Nothing) && checkNothing t

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku x = (printRow (rows x))

printRow :: [[Maybe Int]] -> IO ()
printRow x = putStr (convertToFullString x)

convertToChar :: Maybe Int -> Char
convertToChar x = case x of
    Just x -> intToDigit x
    Nothing -> '.'

convertToFullString :: [[Maybe Int]] -> String
convertToFullString [] = ""
convertToFullString (h:t) = (convertToString h)++ "\n" ++(convertToFullString t )



convertToString :: [Maybe Int] -> String
convertToString []  = ""
convertToString (h:t) = (convertToChar h):(convertToString t) 

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku x = do string <- readFile x
                  if isSudoku(convertStringToSud string) 
                  then return (convertStringToSud string)
                  else error ("This is not a sudoku")

-- Helper function, creates a Sudoku from a one string. Each line is separated by newline
convertStringToSud :: String -> Sudoku
convertStringToSud s = Sudoku (createListOfRows(lines s))

-- Helper function, creates a list of list of Maybe Int from a list of strings
createListOfRows :: [String] -> [[Maybe Int]]
createListOfRows [] = []
createListOfRows (h:t) = createRow(h):createListOfRows(t)

-- Helper function, creates a list of Maybe Int from a single string
createRow :: String -> [Maybe Int]
createRow [] = []
createRow (h:t)
    |   h == '.' = Nothing:createRow t
    |   otherwise = (Just (digitToInt (h))):(createRow t)


-------------------------------------------------------------------------

type Block = [Maybe Int]
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (x@(Just a):xs) = not (elem x xs) && isOkayBlock xs
isOkayBlock (Nothing:xs) = isOkayBlock xs

rowBlocks :: [[Maybe Int]] -> [[Maybe Int]]
rowBlocks rs = rs

colBlocks :: [[Maybe Int]] -> [[Maybe Int]]
colBlocks rs = (map reverse . transpose) rs

squareBlocks :: [[Maybe Int]] -> [[Maybe Int]]
squareBlocks rs =  concat [ 
                            [concat ( map (take 3. drop d) (take 3 . drop d' $ rs)) | d <- [0, 3, 6] ]
                            |
                            d' <- [0, 3, 6]
                          ]

blocks :: Sudoku -> [Block]
blocks (Sudoku rs) = (rowBlocks rs) ++ (colBlocks rs) ++ (squareBlocks rs)


isOkay :: Sudoku -> Bool
isOkay sud = and [ isOkayBlock block | block <- blocks sud]


type Pos = (Int,Int)

blank :: Sudoku -> Pos
blank sud = blankHelper(rows sud)

blankHelper :: [[Maybe Int]] -> (Int, Int)
blankHelper s = (row, col)
    where
        row = case findRow s of
            Just n -> n
            Nothing -> error ("blank row not found")
        col = case findRow s of
            Just p -> case findBlankIndex(s !! p) of
                Just q -> q
                Nothing -> error ("blank column not found")
            Nothing -> error ("blank row not found")

findBlankIndex :: [Maybe Int] -> Maybe Int
findBlankIndex l = findIndex(==Nothing) l

findRow :: [[Maybe Int]] -> Maybe Int
findRow s = findIndex(checkForNothing) s

-- If there are no nothings, return false, else return true
checkForNothing :: [Maybe Int] -> Bool
checkForNothing [] = False
checkForNothing (h:t) = (h == Nothing) || checkForNothing t

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (n, v) = take n xs ++ [v] ++ drop (n + 1) xs


update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rs) (x,y) v = Sudoku ( rs !!= (x, rs !! x !!= (y, v)))


solve :: Sudoku -> [Maybe Sudoku]
solve sud
    | not (isOkay sud) = [Nothing]
    | isSolved sud = [Just sud]
    | otherwise =  filter(/=Nothing) [a | x <- [1..9], a <- solve(update sud (blank sud) (Just x))]

readAndSolve :: FilePath -> IO ()
readAndSolve path = do
  s <- readSudoku path
  case (solve s) of
      (h:t) -> printSudoku (fromJust h)
      [] -> putStrLn "No solution!"

example :: Sudoku
example =
    Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]

complete :: Sudoku
complete =
    Sudoku
      [ [Just 3, Just 6, Just 4, Just 8, Just 7, Just 1, Just 2, Just 9, Just 5]
      , [Just 7, Just 5, Just 2, Just 9, Just 3, Just 6, Just 1, Just 8, Just 4]
      , [Just 8, Just 1, Just 9, Just 2, Just 5, Just 4, Just 7, Just 3, Just 6]
      , [Just 5, Just 9, Just 6, Just 7, Just 1, Just 3, Just 4, Just 2, Just 8]
      , [Just 4, Just 3, Just 1, Just 5, Just 8, Just 2, Just 6, Just 7, Just 9]
      , [Just 2, Just 7, Just 8, Just 4, Just 6, Just 9, Just 3, Just 5, Just 1]
      , [Just 6, Just 4, Just 5, Just 3, Just 2, Just 8, Just 9, Just 1, Just 7]
      , [Just 9, Just 8, Just 3, Just 1, Just 4, Just 7, Just 5, Just 6, Just 2]
      , [Just 1, Just 2, Just 7, Just 6, Just 9, Just 5, Just 8, Just 4, Just 3]
      ]
