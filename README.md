# Sudoku Solver and Game

This is a Sudoku solver and game application written in Haskell using the Gloss library for graphical user interface. The project offers features to generate Sudoku puzzles, allow user-generated Sudoku boards, and provide an interactive interface to play and solve Sudoku puzzles.

## Features

- Generate Sudoku puzzles of varying sizes (4x4, 6x6, 9x9) with adjustable difficulty levels.
- Create custom Sudoku boards with manual input and validate their solvability.
- Interactive gameplay with a timer, error highlighting, and restart functionality.
- Sudoku puzzle solver using combinatorial methods to find solutions.
- Real-time interface updates for incorrect values and conflicts.

## How to Use

1. Install the necessary libraries: `Gloss` and `System.Random` (required for random number generation).
2. Compile the program using GHC: `ghc Sudoku.hs`.
3. Run the executable: `./Sudoku`.

## Interface

The application's interface utilizes Gloss for rendering 2D graphics and interactive elements. It supports various menus and options, each with distinct behaviors and visual cues. The user can navigate through the menus, generate puzzles, solve or play puzzles, and interact with the puzzle board.

## Solving Algorithm

The Sudoku solving algorithm is based on combinatorial methods. It assigns possible values to each cell and iteratively reduces possibilities based on the rules of Sudoku. The algorithm aims to find a unique solution for the puzzle.

<p>
 <img width=45% alt="jugar9x9" src="https://github.com/lucia-jiang/Sudoku-haskell/assets/104275311/45e224d0-e674-4363-8e52-3f1ab94e8d16">
  
<img width=45% alt="generar2_4x4_resolver" src="https://github.com/lucia-jiang/Sudoku-haskell/assets/104275311/dff77cde-980f-4978-818c-b51c24ba7aea">
</p>

