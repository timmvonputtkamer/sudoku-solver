module Input (parseSudokuInput) where

import Data.Char (chr, ord)
import Data.Either (lefts, rights)
import qualified Data.List as List
import qualified Data.Vector.Unboxed as Vector
import Error
import Sudoku

--
-- Input parsing of a Sudoku puzzle
--

parseCharacter :: Char -> Either Error Cell
parseCharacter char
   | char == '.'                                = Right '.'
   | ord char >= ord '1' && ord char <= ord '9' = Right char
   | otherwise                                  = Left (Error ("Parsing error: Invalid character '" ++ [char] ++ "'"))

-- Parse a set of characters to cells, ensuring that there are exactly 9 characters
parseRow :: String -> Either Error [Cell]
parseRow rowString
    | hasParseErrors        = Left (head parseErrors)
    | length rowString /= 9 = Left (Error "Parsing error: There need to be exactly 9 entries per row")
    | otherwise             = Right parsedRow
    where parsedRowResults = map parseCharacter rowString :: [Either Error Cell]
          parsedRow = rights parsedRowResults :: [Cell]
          parseErrors = lefts parsedRowResults :: [Error]
          hasParseErrors = not (null parseErrors)

parseRows :: [String] -> Either Error Sudoku
parseRows rows
    | hasParseErrors   = Left (head parseErrors)
    | length rows /= 9 = Left (Error "Parsing error: There need to be exactly 9 rows")
    | otherwise        = Right (Sudoku ((Vector.fromList . concat) parsedRows ))
    where parsedRowsResults = map parseRow rows :: [Either Error [Cell]]
          parsedRows = rights parsedRowsResults :: [[Cell]]
          parseErrors = lefts parsedRowsResults :: [Error]
          hasParseErrors = not (null parseErrors)

parseSudokuInput :: String -> Either Error Sudoku
parseSudokuInput = parseRows . List.lines
