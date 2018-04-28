# Haskell-7

The repository includes code for the game 'Nine Men's Morris' in Haskell as part of the course **POPL-2**.

## Overview of the game :
Nine Men's Morris is a strategy board game for two players dating at least to the Roman Empire. The board consists of a grid with twenty-four intersections or points. Each player has nine pieces, or "men", usually coloured red and blue. Players try to form 'mills'—three of their own men lined horizontally or vertically—allowing a player to remove an opponent's man from the game. A player wins by reducing the opponent to two pieces (where he could no longer form mills and thus be unable to win), or by leaving him without a legal move.

<br />
<br />

The game proceeds in three phases: <br />

* Placing men on vacant points
* Moving men to adjacent points
* (optional phase) Moving men to any vacant point when the player has been reduced to three men

**Phase 1: Placing pieces** <br/ >
The game begins with an empty board. The players determine who plays first, then take turns placing their men one per play on empty points. If a player is able to place three of his pieces on contiguous points in a straight line, vertically or horizontally, he has formed a mill and may remove one of his opponent's pieces from the board and the game, with the caveat that a piece in an opponent's mill can only be removed if no other pieces are available. After all men have been placed, phase two begins.

**Phase 2: Moving pieces**
Players continue to alternate moves, this time moving a man to an adjacent point. A piece may not "jump" another piece. Players continue to try to form mills and remove their opponent's pieces as in phase one. A player can "break" a mill by moving one of his pieces out of an existing mill, then moving it back to form the same mill a second time (or any number of times), each time removing one of his opponent's men. The act of removing an opponent's man is sometimes called "pounding" the opponent. When one player has been reduced to three men, phase three begins.

**Phase 3: "Flying"**
When a player is reduced to three pieces, there is no longer a limitation on that player of moving to only adjacent points: The player's men may "fly" (or "hop" or "jump") from any point to any vacant point.

## Libraries used
* **Cabal :** <br />
Cabal is a system for building and packaging Haskell libraries and programs. It defnes a common interface for package authors and distributors to easily build their applications in a portable way. Cabal is part of a larger infrastructure for distributing, organizing, and cataloging Haskell libraries and programs.

* **Gloss :** <br />
Gloss is a graphics library used for drawing simple vector based graphics. It uses OpenGL under the hood.

## Screenshots of the game
<!-- ![Screenshot](https://github.com/saksham-mittal/iith-placement-cell/blob/master/screenshot.png) -->
<!-- ![Screenshot](https://github.com/saksham-mittal/iith-placement-cell/blob/master/screenshot.png) -->

## How to run
* Note : Instead of installing gloss globally, we’re going to create a sandbox. A sandbox is a directory in which cabal will (for the most part) ignore the global packages, and will instead install packages directly to that directory.

*  Make sure you cd to your new directory before running this and have cloned the repository, since your working directory is what will be turned into a sandbox. <br />
`cabal sandbox init`

* This command will turn the project directory into a Haskell package directory (as well as a sandbox): <br />
`cabal init` <br />

Make sure to enter "2" in order to create an executable package. <br />
`What does the package build:
   1) Library
   2) Executable
Your choice?`

* We install the gloss library into the sandbox: <br />
`cabal install gloss==1.12.*`

*  Next, once gloss is installed, we have to tell cabal that our package is allowed to use it. Find the line in <_directory-name_>.cabal that mentions build-depends and change it to the following: <br />
`build-depends:       base >=4.7 && <4.8, gloss==1.12.*`

* Now to run the game, run the following command : <br />
`cabal run`
