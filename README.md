# Sudoku Solver in Haskell

## Algorithm

To solve the Sudoku, at each step, a cell with the least number of allowed values (according
to the current partially filled Sudoku and the Sudoku rules) is chosen and each of the
allowed values is tried. 

## Usage

A partially filled input Sudoku is fed into the program via stdin. The program outputs
the solved Sudoku on stdout. Example input Sudokus can be found in the folder example-inputs, 
note that we designate an empty cell by '.'.

## Building and running

To build the project using Stack, run

```bash
stack build
```

To solve one of the example Sudokus, run

```bash
stack exec sudoku-solver-exe < example-inputs/sudoku1.txt
```
