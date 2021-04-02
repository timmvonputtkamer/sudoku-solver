import qualified System.IO as IO

import Program (processSudoku)

main :: IO ()
main = IO.interact processSudoku
