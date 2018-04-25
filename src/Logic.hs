module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game

isCoordCorrect (x, y) = elem (x, y) [ (0, 0),
                                      (0, 3),
                                      (0, 6),
                                      (1, 1),
                                      (1, 3),
                                      (1, 5),
                                      (2, 2),
                                      (2, 3),
                                      (2, 4),
                                      (3, 0),
                                      (3, 1),
                                      (3, 2),
                                      (3, 4),
                                      (3, 5),
                                      (3, 6),
                                      (4, 2),
                                      (4, 3),
                                      (4, 4),
                                      (5, 1),
                                      (5, 3),
                                      (5, 5),
                                      (6, 0),
                                      (6, 3),
                                      (6, 6) ]

switchPlayer game =
    case gamePlayer game of
      Player1 -> game { gamePlayer = Player2 }
      Player2 -> game { gamePlayer = Player1 }

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

countCells :: Cell -> Board -> Int
countCells cell = length . filter ((==) cell) . elems

checkGameOver game
    | p1 <= 2 =
        game { gameState = GameOver $ Just Player2 }
    | p2 <= 2 =
        game { gameState = GameOver $ Just Player1 }
    | countCells cell board == 0 =
        game { gameState = GameOver Nothing }
    | otherwise = game
    where board = gameBoard game
          cell  = Full Dot
          p1    = player1Stone game
          p2    = player2Stone game

playerTurn :: Game ->(Int, Int) -> Game
playerTurn game cellCoord
    | isCoordCorrect cellCoord && board ! cellCoord == Full Dot =
        checkGameOver
        $ switchPlayer
        $ game { gameBoard = board // [(cellCoord, Full player)] }
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
        Running -> playerTurn game $ mousePosAsCellCoord mousePos
        GameOver _ -> initialGame
transformGame _ game = game
