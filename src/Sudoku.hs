module Sudoku (Cell, Sudoku (Sudoku), solveSudoku, isSudokuValid) where

import qualified Data.Vector.Unboxed as Vector
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import Data.Maybe (catMaybes)
import List (hasDuplicates)
import Error

type Cell = Char

-- We represent a Sudoku as a contiguous vector, storing row by row in the vector
newtype Sudoku = Sudoku (Vector.Vector Cell) deriving (Show, Eq)

coordinatesToIndex :: (Int, Int) -> Int
coordinatesToIndex (x, y) = y * 9 + x

indexToCoordinates :: Int -> (Int, Int)
indexToCoordinates index = Tuple.swap (divMod index 9)

--
-- The functions containing the main logic are 'findEmptyCellWithMinimalOptions' and 'solveSudoku'
--

-- Captures the allowed values for each empty cell
data CellOptions = CellOptions { x :: Int, y :: Int, allowedValues :: Set.Set Cell } deriving (Eq)

-- Order CellOptions by the number of allowed values
instance Ord CellOptions where
    compare (CellOptions {allowedValues = a}) (CellOptions {allowedValues = b}) = compare (length a) (length b)

-- Given a Sudoku, returns the cell with the least number of allowed values
findEmptyCellWithMinimalOptions :: Sudoku -> CellOptions
findEmptyCellWithMinimalOptions sudoku = minimum listOfOptions
    where optionsAt (x, y) = CellOptions { x = x, y = y, allowedValues = getAllowedValuesAt (x, y) sudoku }
          listOfOptions = concat [[ optionsAt (x, y) | x <- [0..8], isEmptyCell (getCell (x, y) sudoku) ] | y <- [0..8]]

-- Returns the solved Sudoku. If there is no solution, returns Nothing
solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku sudoku
    | isSolved sudoku    = Just sudoku
    | null allowedValues = Nothing
    | otherwise          = pickSolution possibleSolutions
    where
    possibleSolutions = [solveSudoku (setValueAt (x, y) sudoku value) | value <- Set.toList allowedValues]
    minOption@CellOptions{x = x, y = y, allowedValues = allowedValues} = findEmptyCellWithMinimalOptions sudoku

-- Picks the first solution or return Nothing if there is no solution
pickSolution :: [Maybe Sudoku] -> Maybe Sudoku
pickSolution listOfMaybeSolutions
    | null listOfSolutions = Nothing
    | otherwise            = Just (head listOfSolutions)
    where listOfSolutions = catMaybes listOfMaybeSolutions

hasNoDuplicateValues :: [Cell] -> Bool
hasNoDuplicateValues = not . hasDuplicates . getFilledCells

isRowValid :: Int -> Sudoku -> Bool
isRowValid y sudoku = hasNoDuplicateValues (getRow y sudoku)

isColumnValid :: Int -> Sudoku -> Bool
isColumnValid x sudoku = hasNoDuplicateValues (getColumn x sudoku)

isBlockValid :: (Int, Int) -> Sudoku -> Bool
isBlockValid (blockPosX, blockPosY) sudoku = hasNoDuplicateValues (getCellsInBlock (blockPosX, blockPosY) sudoku)

isSudokuValid :: Sudoku -> Bool
isSudokuValid sudoku = areRowsValid && areColumnsValid && areBlocksValid
    where areRowsValid = all id [isRowValid y sudoku | y <- [0..8]]
          areColumnsValid = all id [isColumnValid x sudoku | x <- [0..8]]
          areBlocksValid = all id (concat [[isBlockValid (blockPosX, blockPosY) sudoku | blockPosX <- [0..2]] | blockPosY <- [0..2]])

--
-- Basic computations with a Sudoku puzzle
--

getCell :: (Int, Int) -> Sudoku -> Cell
getCell (x, y) (Sudoku cells) = cells Vector.! index
    where index = coordinatesToIndex (x, y)

isEmptyCell :: Cell -> Bool
isEmptyCell '.' = True
isEmptyCell _   = False

isFilledCell :: Cell -> Bool
isFilledCell = not . isEmptyCell

getFilledCells :: [Cell] -> [Cell]
getFilledCells = filter isFilledCell

-- Checks if all values are set in Sudoku
isSolved :: Sudoku -> Bool
isSolved (Sudoku cells) = Vector.all isFilledCell cells

-- Creates a new Sudoku puzzle with a a value set at the specified coordinate (x,y)
-- optimized using Vector.// that updates a list of values given their index
setValueAt :: (Int, Int) -> Sudoku -> Cell -> Sudoku
setValueAt coordinates (Sudoku cells) newValue = Sudoku (cells Vector.// [(index, newValue)])
    where index = coordinatesToIndex coordinates

getRow :: Int -> Sudoku -> [Cell]
getRow y sudoku = [getCell (cursorX, y) sudoku | cursorX <- [0..8]]

getColumn :: Int -> Sudoku -> [Cell]
getColumn x sudoku = [getCell (x, cursorY) sudoku | cursorY <- [0..8]]

getValuesInCrosshairsOf :: (Int, Int) -> Sudoku -> Set.Set Cell
getValuesInCrosshairsOf (x, y) sudoku = Set.union row column
    where row = (Set.fromList . getFilledCells ) (getRow y sudoku)
          column = (Set.fromList . getFilledCells) (getColumn x sudoku)

getCellsInBlock :: (Int, Int) -> Sudoku -> [Cell]
getCellsInBlock (blockPosX, blockPosY) sudoku = [getCell (cursorX, cursorY) sudoku | cursorX <- [x..(x + 2)], cursorY <- [y..(y + 2)] ]
    where x = blockPosX * 3
          y = blockPosY * 3

getValuesInBlockOf :: (Int, Int) -> Sudoku -> Set.Set Cell
getValuesInBlockOf (x, y) sudoku = (Set.fromList . getFilledCells) (getCellsInBlock (blockPosX, blockPosY) sudoku)
    where blockPosX = (x `div` 3)
          blockPosY = (y `div` 3)

getProhibitedValuesAt :: (Int, Int) -> Sudoku -> Set.Set Cell
getProhibitedValuesAt coordinates sudoku = Set.union (getValuesInBlockOf coordinates sudoku) (getValuesInCrosshairsOf coordinates sudoku)

getAllowedValuesAt :: (Int, Int) -> Sudoku -> Set.Set Cell
getAllowedValuesAt coordinates sudoku = Set.difference allAllowedValues (getProhibitedValuesAt coordinates sudoku)
    where allAllowedValues = Set.fromList ['1', '2', '3', '4', '5', '6', '7', '8', '9']

