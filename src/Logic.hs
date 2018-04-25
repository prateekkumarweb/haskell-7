module Logic where

import Data.Array
import Game
import Graphics.Gloss.Interface.Pure.Game


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

transformGame _ game = game

