import Test.Hspec
import Program (processSudoku)

solvableSudoku = "..2.....3\n\
                 \.853...76\n\
                 \.1..9....\n\
                 \2.38....1\n\
                 \.64..2.39\n\
                 \.7.93.2..\n\
                 \...259.1.\n\
                 \1.8...6..\n\
                 \.29....57"

sudokuSolution = "742165983\n\
                 \985324176\n\
                 \316798425\n\
                 \293847561\n\
                 \864512739\n\
                 \571936248\n\
                 \637259814\n\
                 \158473692\n\
                 \429681357"

unsolvableSudoku = "1.9.25.3.\n\
                   \6....4..7\n\
                   \.7.6...2.\n\
                   \...37..9.\n\
                   \....1....\n\
                   \.86...2..\n\
                   \.34...7..\n\
                   \..25.....\n\
                   \.....23.."

main :: IO ()
main = hspec $ do
  describe "Sudoku solver" $ do
    it "can solve a Sudoku" $ do
      processSudoku solvableSudoku `shouldBe` sudokuSolution
    it "detects unsolvable Sudoku" $ do
      processSudoku unsolvableSudoku `shouldBe` "The Sudoku does not have a solution"
