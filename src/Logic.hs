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


horizontalLine game  [row, column, distance, validity]    | board!(row, column) == board!(row, column + distance) && board!(row, column) == board!(row, column + 2*distance) && board!(row, column) == player && validity == 1 = player
                                                          | otherwise = Full Dot
                                                                where  board = gameBoard game
                                                                       player = Full $ gamePlayer game

verticleLine game  [column, row, distance, validity]      | board!(row, column) == board!(row + distance, column) && board!(row, column) == board!(row + 2*distance, column) && board!(row, column) == player && validity == 1 = player
                                                          | otherwise = Full Dot
                                                                where  board = gameBoard game
                                                                       player = Full $ gamePlayer game
                                        


finalHorizontalCheck game  = map (horizontalLine game) $ gameList game
finalVerticalCheck game = map (verticleLine game) $ gameList game

viewUpdate game     | (finalHorizontalCheck game)!!1 == Full Player1 = game {gameBoard = board // [((3,2), Full Player2)]}
                    | (finalVerticalCheck game)!!1 == Full Player2 = game {gameBoard = board // [((6,6), Full Player1)]}
                    | otherwise =  game {gameBoard = board // [((6,0), Full Player1)]}
                        where board = gameBoard game

-- listUpdater game | elem ([] : player) (finalHorizontalCheck) =  game {gameBoard = board // [((3,2), Full Player2)]}
--                  | elem ([] : player) (finalVerticalCheck) =  game {gameBoard = board // [((6,6), Full Player2)]}
--                  | otherwise  = game {gameBoard = board // [((6,0), Full Player1)]}
--                     where board = gameBoard game
--                           player = Full $ gamePlayer game



transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = 
    case gameState game of 
        Running -> viewUpdate game


