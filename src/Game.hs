module Game where

import Data.Array

data Player = Player1 | Player2 | Dot deriving(Eq, Show)
data Cell = Empty | Full Player deriving (Eq, Show)
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell


data Game  = Game { gameBoard :: Board,
                    gamePlayer :: Player,
                    gameState :: State,
                    gameList :: [[Int]]
                  } deriving(Eq, Show)


n :: Int
n = 7

listForCheck = [[0,0,3,1], [1,1,2,1], [2,2,1,1], [3,0,1,1], [3,4,1,1], [4,2,1,1], [5,1,2,1], [6,0,3,1]]

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

initialGame = Game { gameBoard = (array indexRange $ zip (range indexRange) (cycle [Empty])) // [ ((0, 0), Full Player1),
                                                                                                  ((0, 3), Full Player1),
                                                                                                  ((0, 6), Full Player1),
                                                                                                  ((1, 1), Full Player2),
                                                                                                  ((1, 3), Full Dot),
                                                                                                  ((1, 5), Full Dot),
                                                                                                  ((2, 2), Full Dot),
                                                                                                  ((2, 3), Full Dot),
                                                                                                  ((2, 4), Full Dot),
                                                                                                  ((3, 0), Full Dot),
                                                                                                  ((3, 1), Full Player2),
                                                                                                  ((3, 2), Full Dot),
                                                                                                  ((3, 4), Full Dot),
                                                                                                  ((3, 5), Full Dot),
                                                                                                  ((3, 6), Full Dot),
                                                                                                  ((4, 2), Full Dot),
                                                                                                  ((4, 3), Full Dot),
                                                                                                  ((4, 4), Full Dot),
                                                                                                  ((5, 1), Full Player2),
                                                                                                  ((5, 3), Full Dot),
                                                                                                  ((5, 5), Full Dot),
                                                                                                  ((6, 0), Full Dot),
                                                                                                  ((6, 3), Full Dot),
                                                                                                  ((6, 6), Full Dot) ]
                     , gamePlayer = Player2
                     , gameState = Running,
                     gameList = listForCheck
                   }
          where indexRange = ((0, 0), (n - 1, n - 1))
