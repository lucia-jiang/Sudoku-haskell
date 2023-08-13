# Sudoku

## Required Libraries
To compile and run the code, you need to have the **Gloss** graphics library downloaded (`cabal install gloss`) and **System.Random** (`cabal install random`) to generate random numbers without the need for using seeds. You must have `cabal` for installations.

We have also executed the commands `cabal install --lib [package]` to add the libraries to the environment without manually loading them every time we compile.

We used a Mac to compile and run the program. To compile it, from the directory containing the file (or providing the full path) in the terminal: `ghc PF_Practica4.hs`. This creates the executable, and you can run it directly with `./PF_Practica4`.

## Program Functionality
We've created an interactive program for the user that can generate Sudoku boards and allow the user to create their own board. Additionally, for both types of boards, the program can solve them or allow the user to play and solve them.


![picture 3](images/main.png)  

In either of the two options, you can choose the **game difficulty**, which corresponds to the size of the board (4x4, 6x6, or 9x9). The selected option will be highlighted with a different color, and it's necessary to have a chosen option to create or generate.


![picture 4](images/generar.png)  | ![picture 6](images/crear.png)  
:-------------:|:---------------:


![picture 7](images/crear1.png) | ![picture 9](images/crear2.png) |![picture 10](images/crear3.png)  
:-------------:|:---------------:|:---------------:

### Generate Sudoku
Once the difficulty is chosen, a Sudoku with at least one solution is generated. The user can choose from the following options:
1. **Another board**, which would generate another board of the same difficulty.
2. **Solve**, the program would search for a solution and display it to the user. In case a cell could take multiple values, the cell would remain blank without any number on it.
3. **Play level**, the program would take the user to the playing interface, where they can solve the generated Sudoku.

 
![picture 2](images/generar2_4x4.png) | ![picture 3](images/generar2_4x4_resolver.png) | ![picture 1](images/generar2_9x9.png)   
:-------------:|:---------------:|:---------------:

<div style="page-break-after: always"></div>

### Create Sudokus

In this interface, it's the user who creates the Sudoku. The difficulty, just like in the case of "Generate Sudoku," is determined by the dimension of the board. The options are:
1. **New board**, which clears the entire Sudoku.
2. **Solve**, the program would search if there's a solution for the posed board and, if affirmative, display it. In case of multiple solutions, only cells with fixed values are indicated.
3. **Play level**, the program verifies that it's a solvable board and takes the user to the playing interface, where they can play the created level.

> NOTE: Only numbers from 1 to n are allowed, where n is the dimension of the Sudoku. In case of attempting to input a character or a number greater than n, the program would ignore it. To remove the digit from a cell (provided it's not a fixed one), it can be deleted using the Delete, Backspace, or pressing the number 0 key.

Example of creating a board with a unique solution:

![](images/jugar2_4x4.png) | ![](images/jugar2_4x4_2.png)
:-------------:|:---------------:

Example of creating a board without a solution. When clicking "Solve" or "Play level," a notification appears:

![](images/jugar2_4x4_nosol.png) | ![](images/jugar2_4x4_aviso.png)
:-------------:|:---------------:

<div style="page-break-after: always"></div>

### Playing Sudokus

The board becomes interactive, the hints will turn gray and cannot be altered. Within this option:
1. There's a **time indicator** showing how long we've been playing, in seconds.
2. You can **check** the board at any time, and cells that repeat with others in rows, columns, or regions will be highlighted in red; those that don't repeat will be highlighted in green.
3. You can **restart** the level, clearing all non-initial cells and resetting the timer to 0.

> NOTE: Only numbers from 1 to n can be entered, just like in "Create Sudoku," and the functionality is the same.


![picture 11](images/jugar4x4.png) | ![picture 12](images/jugar6x6.png)
:-------------:|:---------------:

![picture 13](images/jugar9x9.png) | ![picture 14](images/jugar9x9-2.png)
:-------------:|:---------------:

<div style="page-break-after: always"></div>

## Interface Code

The `Gloss` library has been used, specifically, `Gloss.Interface.Game`. Gloss is very useful for drawing 2D vectors, animations, and simulations as it utilizes OpenGL. However, it's not as convenient for creating buttons and widgets, which need to be manually crafted, as we'll demonstrate below.

To begin with, the `GameState` data has been defined, which indicates the interface page we are on:


```
data GameState = MainMenu | GenMenu1 Int | GenMenu2 Sudoku
    | CreatMenu1 Int | CreatMenu2 Sudoku Selected
    | SolveMenu (Maybe Sudoku) GameState 
    | PlayMenu Sudoku Sudoku Selected Float Bool GameState

data Selected = Maybe (Int, Int)
```
A different set of parameters is passed to each interface page depending on what needs to be displayed. For example, `MainMenu` doesn't require any parameters, while `GenMenu2` has the generated Sudoku to display.

The code for the interface can be broadly divided into the following sections:

1. Defining "variables":
   - We've implemented a new data type, `type Bounds = (Float, Float, Float, Float)`, which represents the coordinates of the upper-left corner (x-coordinate, y-coordinate), width, and height.
   - We've defined the `Bounds` for all the buttons used in the interface.
   - We've defined the colors used in our interface (using `(Gloss) makeColor`).

2. Displaying different pages: We use the function `printWorld :: GameState -> IO Picture`, which, given the game state, displays the corresponding interface.
   - We've previously defined the `Bounds` and button colors to be able to display them from this function.
   - To create the board, we've implemented a function to visualize the Sudoku board called `paintSudoku`. We draw the grid using the `paintSudokuField` function and add values and colors to the cells using `paintSudokuCell`.

3. Responding to different actions: The `handleEvents` function decides what to do when it receives a user action. `handleEvents :: Event -> GameState -> IO GameState`
   - An `Event` encompasses various actions that can occur, such as pressing or releasing a key or moving the mouse.
   - It takes the current game state along with the event and returns a new game state wrapped in `IO`.
   - To return a new state, we call auxiliary functions, each dedicated to a specific menu, to better organize the code. Examples of these auxiliaries are `mainMenuEvent`, `genMenu1Event`, or `creatMenu2Event`, which receive an event and use `inBounds` to check if a button's `Bounds` has been clicked.

4. Registering game time: We use `elapseTime :: Float -> GameState -> IO GameState`, which indicates the time elapsed since the last frame shown on the screen. When in the play menu, we increment the time by this value.

5. Creating buttons: We create widgets that trigger certain actions when clicked.
   - With the new data type, we draw rectangles using `rectangle`. It consists of a filled polygon (`(Gloss) polygon`) to which we pass a color (defined with `(Gloss) makeColor`), outline lines (`(Gloss) line`), and text for display. Additionally, we set horizontal alignment and font size for the button.
   - Creating buttons: `button`. We call `rectangle` with specified font size.

## Game Mechanics Code

### Generating Sudokus
We first generate a filled board (to ensure it has at least one solution) and then leave a random number of clues between 35% and 45%.

To achieve this, we've implemented the `generate` function, which first places all the 1's in random positions, ensuring they don't repeat within rows, columns, or regions. Next, the 2's are placed in the remaining spots, verifying that they also don't repeat, and this process continues up to the number 'n', which corresponds to the dimension of the Sudoku.

We achieve this by calling `genField`, which utilizes `genLoop` to iterate and attempt to place values on the board. In cases where the board reaches a point without a solution, and thus cannot be further filled, `genLoop` aborts, and `genField` calls it again to generate a new board.

> On average, the 4x4 rarely fails, the 6x6 fails around 15 times on average with a standard deviation of 5, and the 9x9 generally falls between 1000 and 3000 times. In any case, this process is very fast, and all boards can be generated in less than half a second.


<div style="page-break-after: always"></div>

### Solving Sudokus

We employ a combinatorial approach to solve Sudokus. Each cell is assigned a list of possible values it can take, which is stored in the so-called value board. Initially, if a Sudoku has a given value in a cell, its list of possible values consists only of that specific value. If not, the possible values would be all numbers.

For each row, column, and group (referred to as sets from now on), there's also a list of all possible combinations of values that the cells within these sets can take.

The algorithm involves updating the value board by analyzing all combinations within all sets. As cells on the board have fewer possible values, various combinations must be discarded since they're no longer valid. After multiple iterations, the value board and combinations are reduced to a point where each cell has a unique possible value, and each set has a unique combination. Thus, the obtained solution is returned. If there are multiple solutions, the process might reach a point where combinations can't be reduced further, and the solutions obtained until that moment are returned.

We've created the `solve` function to implement this algorithm. It starts by calling `solStart`, which performs an initial synchronization to reduce the value board based on all possible initial combinations of the Sudoku. Then, `solLoop` is called in a loop to discard invalid combinations after the previous synchronization and to re-synchronize the value board. Once no further changes are made to the synchronizations in an iteration of the loop (either because the final solution is reached or because no more distinct solutions can be distinguished), the result is returned.

If the Sudoku to be solved has no solution, there will be a point where the algorithm leaves a set without any possible combinations of values for its cells. This situation will be detected, and the execution will be aborted, returning `Nothing`.


### Checking Solution

We use this method when the user wants to verify if the board with the entered numbers is correct. A cell has a correct value if its value is not repeated in the row, column, and group it belongs to. The cell will be colored green if its value is correct and red if conflicts exist. Empty cells will remain white.

To verify if the value of a cell is correct, we obtain a list representing the values of the cells in the same row, and we ensure that its value is not repeated within these cells. The same process is applied to the column and group to which the cell belongs.

To color the board, we use the `colorSudoku` function that iterates through all cells, checking their values and calling the `colorCell` function. This function receives the three lists of values from cells in the same row, column, and group. Obtaining these lists is straightforward for rows since the Sudoku itself is a list of rows, so we can directly access the row. For columns, we need to use the transpose of the Sudoku matrix to access them directly. For groups, it's more complex, and we use the `getGrpsMat` function to modify the matrix and the `pos2grp` function to determine the index to access.

<div style="page-break-after: always"></div>

### Auxiliary Functions Code

We have defined several utility functions that are mentioned in this section since they are not specific to either the game mechanics or the interface. Instead, they are used extensively throughout the entire program. These functions are all properly documented in the code, but examples include `member`, `removeDups`, `getValMat` and `setValMat`, `getGrpsMat`, `fracture`, `size2dim`, among others. Most of these functions are polymorphic, allowing them to abstract the type and be used in various situations. For instance, `createMat` is used in multiple instances to create matrices of `Int`s, `Bool`s, and `Color`s.

