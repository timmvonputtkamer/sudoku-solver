module Program (processSudoku) where

import Error
import Input (parseSudokuInput)
import Output (formatSudoku)
import Sudoku (Sudoku, solveSudoku, isSudokuValid)

validateSudoku :: Either Error Sudoku -> Either Error Sudoku
validateSudoku (Left error) = Left error
validateSudoku (Right sudoku)
    | isSudokuValid sudoku = Right sudoku
    | otherwise            = Left (Error "The given input Sudoku does not satisfy the rules")

trySolveSudoku :: Either Error Sudoku -> String
trySolveSudoku = either extractErrorMessage (formatSudokuSolution . solveSudoku)

formatSudokuSolution :: Maybe Sudoku -> String
formatSudokuSolution Nothing = "The Sudoku does not have a solution"
formatSudokuSolution (Just sudoku) = formatSudoku sudoku

processSudoku :: String -> String
processSudoku = trySolveSudoku . validateSudoku . parseSudokuInput
