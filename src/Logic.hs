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

switchPlayer game checker
    | checker == 1 && gamePlayer game == Player1 = game { gamePlayer = Player2 }
    | checker == 1 && gamePlayer game == Player2 = game { gamePlayer = Player1 }
    | otherwise = game

switchPlayer1 game =
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
          p1    = removeStone1 game
          p2    = removeStone2 game

playerTurn :: Game ->(Int, Int) -> Game
playerTurn game cellCoord
    -- If player1Stone <= maxStone1 and chance = player1
    -- then player1Stone++ and replace cellCoord with Full player
    -- Else don't recognise the click (for now)
    | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && (takeOther game 0) >= 8 && player == Player1 && (player1Stone game) <= (maxStone1 game) =
         checkGameOver
        $ switchPlayer game { gameBoard = board // [(cellCoord, Full player)], player1Stone = n1 + 1  }
        $ playerSwitcherConfirm
        $ game { gameBoard = board // [(cellCoord, Full player)], player1Stone = n1 + 1 }
    | isCoordCorrect cellCoord && board ! cellCoord == Full Player1 && player == Player1 && (player1Stone game) > (maxStone1 game) && (movedCoordSet game) == 0 =
         -- Store the current clicked coordinates (only if atleast one neighbour is a 'dot')
         -- make 'stored' = 1
         -- Don't give chance to the next player
         -- If movedCoordSet == 1, then check if curent click is a neighbour of stored coords (and then remove the stored coords and render the current clicked coords)
         checkGameOver
         $ setCoords game cellCoord
         $ checkNeighbour game cellCoord
    | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && player == Player1 && (player1Stone game) > (maxStone1 game) && (movedCoordSet game) == 1 && (validCellCoords game cellCoord mCoords) =
          checkGameOver
          $ listUnblocker
          $ switchPlayer game { gameBoard = board // [(cellCoord, Full player), (mCoords, Full Dot)],  moveCoords = (-1, -1), movedCoordSet = 0 }
          $ playerSwitcherConfirm
        --   $ setCoordsBack -- this sets the moveCoords back to (-1, -1)
          $ game { gameBoard = board // [(cellCoord, Full player), (mCoords, Full Dot)] }
    | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && (takeOther game 0) >= 8 && player == Player2 && (player2Stone game) <= (maxStone2 game) =
         checkGameOver
        $ switchPlayer game { gameBoard = board // [(cellCoord, Full player)], player2Stone = n2 + 1  }
        $ playerSwitcherConfirm
        $ game { gameBoard = board // [(cellCoord, Full player)], player2Stone = n2 + 1 }
    | isCoordCorrect cellCoord && board ! cellCoord == Full Player2 && player == Player2 && (player2Stone game) > (maxStone2 game) && (movedCoordSet game) == 0 =
         -- Store the current clicked coordinates (only if atleast one neighbour is a 'dot')
         -- make 'stored' = 1
         -- Don't give chance to the next player
         -- If movedCoordSet == 1, then check if curent click is a neighbour of stored coords (and then remove the stored coords and render the current clicked coords)
         checkGameOver
         $ setCoords game cellCoord
         $ checkNeighbour game cellCoord
    | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && player == Player2 && (player2Stone game) > (maxStone2 game) && (movedCoordSet game) == 1 && (validCellCoords game cellCoord mCoords) =
          checkGameOver
          $ listUnblocker
          $ switchPlayer game { gameBoard = board // [(cellCoord, Full player), (mCoords, Full Dot)],  moveCoords = (-1, -1), movedCoordSet = 0 }
          $ playerSwitcherConfirm
        --   $ setCoordsBack -- this sets the moveCoords back to (-1, -1)
          $ game { gameBoard = board // [(cellCoord, Full player), (mCoords, Full Dot)] }
    | isCoordCorrect cellCoord && board ! cellCoord /= Full Dot && (takeOther game 0) < 8 && board ! cellCoord /= Full player = switchPlayer1 $ listUnblocker $ remover game cellCoord game
    | otherwise = game
        where board = gameBoard game
              player = gamePlayer game
              mCoords = moveCoords game
              n1 = player1Stone game
              n2 = player2Stone game

-- 'movedCoordSet' = 1 means the movedCoords are set

validCellCoords game (x, y) mCoords
    | isUpNeighbour game (x, y) mCoords == 1 =
        True
    | isDownNeighbour game (x, y) mCoords == 1 =
        True
    | isLeftNeighbour game (x, y) mCoords == 1 =
        True
    | isRightNeighbour game (x, y) mCoords == 1 =
        True
    | otherwise = False

isUpNeighbour game (x, y) mCoords
    | x <= 5 && board ! (x + 1, y) == Empty =
        isUpNeighbour game (x + 1, y) mCoords
    | x <= 5 && (x + 1, y) == mCoords =
        1
    | otherwise = 0
        where board = gameBoard game

isDownNeighbour game (x, y) mCoords
    | x >= 1 && board ! (x - 1, y) == Empty =
        isDownNeighbour game (x - 1, y) mCoords
    | x >= 1 && (x - 1, y) == mCoords =
        1
    | otherwise = 0
        where board = gameBoard game

isLeftNeighbour game (x, y) mCoords
    | y >= 1 && board ! (x, y - 1) == Empty =
        isLeftNeighbour game (x, y - 1) mCoords
    | y >= 1 && (x, y - 1) == mCoords =
        1
    | otherwise = 0
        where board = gameBoard game

isRightNeighbour game (x, y) mCoords
    | y <= 5 && board ! (x, y + 1) == Empty =
        isRightNeighbour game (x, y + 1) mCoords
    | y <= 5 && (x, y + 1) == mCoords =
        1
    | otherwise = 0
        where board = gameBoard game

setCoordsBack game =
    game { moveCoords = (-1, -1), movedCoordSet = 0 }

setCoords game cellCoord checker
    | checker == 1 =
        game { moveCoords = cellCoord, movedCoordSet = 1 }
    | otherwise = game

checkNeighbour game cellCoord
    | isUp game (x, y) == 1 =
        1
    | isDown game (x, y) == 1 =
        1
    | isLeft game (x, y) == 1 =
        1
    | isRight game (x, y) == 1 =
        1
    | otherwise = 0
        where x = fst cellCoord
              y = snd cellCoord

isUp game (x, y)
    | x <= 5 && board ! (x + 1, y) == Empty =
        isUp game (x + 1, y)
    | x <= 5 && board ! (x + 1, y) == Full Dot =
        1
    | otherwise = 0
        where board = gameBoard game

isDown game (x, y)
    | x >= 1 && board ! (x - 1, y) == Empty =
        isDown game (x - 1, y)
    | x >= 1 && board ! (x - 1, y) == Full Dot =
        1
    | otherwise = 0
        where board = gameBoard game

isLeft game (x, y)
    | y >= 1 && board ! (x, y - 1) == Empty =
        isLeft game (x, y - 1)
    | y >= 1 && board ! (x, y - 1) == Full Dot =
        1
    | otherwise = 0
        where board = gameBoard game

isRight game (x, y)
    | y <= 5 && board ! (x, y + 1) == Empty =
        isRight game (x, y + 1)
    | y <= 5 && board ! (x, y + 1) == Full Dot =
        1
    | otherwise = 0
        where board = gameBoard game

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
        Running -> playerTurn game $ mousePosAsCellCoord mousePos
        GameOver _ -> initialGame
transformGame _ game = game


horizontalLine game  [row, column, distance]    | board!(row, column) == board!(row, column + distance) && board!(row, column) == board!(row, column + 2*distance) && board!(row, column) == player  = player
                                                          | otherwise = Full Dot
                                                                where  board = gameBoard game
                                                                       player = Full $ gamePlayer game

verticleLine game  [column, row, distance]      | board!(row, column) == board!(row + distance, column) && board!(row, column) == board!(row + 2*distance, column) && board!(row, column) == player  = player
                                                          | otherwise = Full Dot
                                                                where  board = gameBoard game
                                                                       player = Full $ gamePlayer game

-- horizontalLine game  [((row, column), data)]                | board!(row, column) == board!(row, column + distance) && board!(row, column) == board!(row, column + 2*distance) && board!(row, column) == player && (data == 10 || data == 7 || data == 4 ) = player
--                                                             | otherwise = Full Dot
--                                                                 where  board = gameBoard game
--                                                                        player = Full $ gamePlayer game

-- verticleLine game  [(row, column), data]                  | board!(row, column) == board!(row + distance, column) && board!(row, column) == board!(row + 2*distance, column) && board!(row, column) == player && (data == 10 || data == 7 || data == 4 ) = player
--                                                           | otherwise = Full Dot
--                                                                 where  board = gameBoard game
--                                                                        player = Full $ gamePlayer game

horizontalLineUnblock game  [row, column, distance]    | (board!(row, column) /= board!(row, column + distance) || board!(row, column) /= board!(row, column + 2*distance))   = Full Dot
                                                          | otherwise = player
                                                                where  board = gameBoard game
                                                                       player = Full $ gamePlayer game

verticleLineUnblock game  [column, row, distance]      | (board!(row, column) /= board!(row + distance, column) || board!(row, column) /= board!(row + 2*distance, column))  = Full Dot
                                                          | otherwise = player
                                                                where  board = gameBoard game
                                                                       player = Full $ gamePlayer game




finalHorizontalCheck game  = map (horizontalLine game) $ gameList game
finalVerticalCheck game = map (verticleLine game) $ gameList game

finalHorizontalCheckUnblocker game  = map (horizontalLineUnblock game) $ gameList game
finalVerticalCheckUnbloccker game = map (verticleLineUnblock game) $ gameList game

-- viewUpdate game     | (finalHorizontalCheck game)!!1 == Full Player1 = game {gameBoard = board // [((3,2), Full Player2)]}
--                     | (finalVerticalCheck game)!!1 == Full Player2 = game {gameBoard = board // [((6,6), Full Player1)]}
--                     | otherwise =  game {gameBoard = board // [((6,0), Full Player1)]}
--                         where board = gameBoard game

takeOther game n        | n < 8 && ((finalHorizontalCheck game)!!n == player || (finalVerticalCheck game)!!n == player) && validity!!n == 1 = n
                        | n == 8 = 8
                        | otherwise =  takeOther game (n + 1)
                            where board = gameBoard game
                                  player = Full $ gamePlayer game
                                  validity = checkList game

unblockOther game k     | k < 8 && ((finalHorizontalCheckUnblocker game)!!k == Full Dot && (finalVerticalCheckUnbloccker game)!!k == Full Dot) && validity!!k == 0 = k
                        | k == 8 = 8
                        | otherwise =  unblockOther game (k + 1)
                            where board = gameBoard game
                                  validity = checkList game

listUnblocker game  | (unblockOther game 0) < 8 = game { checkList = (replaceNth 1 listf $ unblockOther game 0)}
                    | otherwise = game
                        where
                            listf = checkList game
-- checkListUpdater game n    | n < 8 = game  { checkList = (replaceNth n listf 0) }
--                            | otherwise = game
--                             where
--                                 listf = checkList game
replaceNth newVal (x:xs) n
                        | n == 0 = newVal:xs
                        | otherwise = x:replaceNth newVal xs (n - 1)



remover game cellCoord game1   | (takeOther game 0) < 8 && player == Player1 =
                                    game { gameBoard = board // [(cellCoord, Full Dot)],  checkList = (replaceNth 0 listf $ takeOther game 0), removeStone1 = stone2 - 1}
                               | (takeOther game 0) < 8 && player == Player2 =
                                    game { gameBoard = board // [(cellCoord, Full Dot)],  checkList = (replaceNth 0 listf $ takeOther game 0), removeStone2 = stone1 - 1}
                               | otherwise = game
                                    where board = gameBoard game
                                          stone1 = removeStone1 game
                                          stone2 = removeStone2 game
                                          listf = checkList game
                                          player = gamePlayer game

playerSwitcherConfirm game | (takeOther game 0) < 8 = 0
                           | otherwise = 1


-- isTrueTakeOther | takeOther 0 (finalHorizontalCheck game)  < 9 = game {gameBoard = board // [(, Full Player2)]}

-- listUpdater game | elem ([] : player) (finalHorizontalCheck) =  game {gameBoard = board // [((3,2), Full Player2)]}
--                  | elem ([] : player) (finalVerticalCheck) =  game {gameBoard = board // [((6,6), Full Player2)]}
--                  | otherwise  = game {gameBoard = board // [((6,0), Full Player1)]}
--                     where board = gameBoard game
--                           player = Full $ gamePlayer game





-- transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
--     case gameState game of
--         Running -> viewUpdate game
