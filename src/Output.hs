module Output (formatSudoku) where

import qualified Data.Vector.Unboxed as Vector
import Sudoku

--
-- Format a Sudoku puzzle for printing by inserting newlines
--

-- Inserts a character after every n elements
insertCharacters :: Char -> Int -> String -> String
insertCharacters char n [] = []
insertCharacters char n str
    | length str <= n = str
    | otherwise       = take n str ++ [char] ++ insertCharacters char n (drop n str)

formatSudoku :: Sudoku -> String
formatSudoku (Sudoku cells) = insertCharacters '\n' 9 (Vector.toList cells)
