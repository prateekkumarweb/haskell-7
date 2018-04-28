module Logic1 where

    import Data.Array
    import Data.Foldable ( asum )

    import Game
    import Graphics.Gloss.Interface.Pure.Game

    -- | The 'isCoordCorrect' function checks whether the given coordinates are correct or not
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

    -- | 'twoHCoord'
    -- List of coordinates which have two horizontal neighbours
    twoHCoord = [(0,0),
                 (3,0),
                 (6,0),
                 (0,6),
                 (3,6),
                 (6,6),
                 (1,1),
                 (5,1),
                 (1,5),
                 (5,5),
                 (2,2),
                 (3,2),
                 (4,2),
                 (2,4),
                 (3,4),
                 (4,4)]

    -- | 'twoVCoord'
    -- | List of coordinates which have two verticle neighbours
    twoVCoord = [(0,0),
                 (0,3),
                 (0,6),
                 (6,0),
                 (6,3),
                 (6,6),
                 (1,1),
                 (1,5),
                 (5,1),
                 (5,5),
                 (2,2),
                 (2,3),
                 (2,4),
                 (4,2),
                 (4,3),
                 (4,4)]

    -- | 'oneHCoord'
    -- List of coordinates which have one horizontal neighbours
    oneHCoord = [(0,3),
                 (1,3),
                 (2,3),
                 (3,1),
                 (3,5),
                 (4,3),
                 (5,3),
                 (6,3)]

    -- | 'oneVCoord'
    -- List of coordinates which have one verticle neighbours
    oneVCoord = [(3,0),
                 (3,1),
                 (3,2),
                 (5,3),
                 (1,3),
                 (3,4),
                 (3,5),
                 (3,6)]


    -- | 'switchPlayer'
    -- Fuction which switches the player according to the value of checker data in the game data set
    switchPlayer game
        | checker game == 1 && gamePlayer game == Player1 = game { gamePlayer = Player2 }
        | checker game == 1 && gamePlayer game == Player2 = game { gamePlayer = Player1 }
        | otherwise = game

    -- | 'switchPlaye1'
    -- No checker invollved, this function switches the player when called
    switchPlayer1 game =
        case gamePlayer game of
            Player1 -> game { gamePlayer = Player2 }
            Player2 -> game { gamePlayer = Player1 }

    -- | 'mousePosAsMenuCellCoord'
    -- This funciton converts the mouse coordinates to menu cell coordinates
    mousePosAsMenuCellCoord (x, y) = ( floor((y + (fromIntegral screenHeight * 0.5)) / (fromIntegral screenHeight))
                                   , floor((x + (fromIntegral screenWidth * 0.5)) / (fromIntegral screenWidth / 2))
                                   )

    -- | 'mousePosAsCellCoord'
    -- This function converts the mouse coordinates to cell coordinates
    mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
    mousePosAsCellCoord (x, y) = ( floor((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                                 , floor((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                                 )

    --'countCells'
    -- This funciton returns the count of the number of cells with the 'cell' as an element
    countCells :: Cell -> Board -> Int
    countCells cell = length . filter ((==) cell) . elems

    -- | 'checkGameOver'
    -- This funcion implements the gameover condition for the game
    -- 1) When either of a player has less than 3 stones left
    -- 2) A player does not have a viable move left
    checkGameOver game
        | p1 <= 2 =
            game { gameState = GameOver $ Just Player2 }
        | p2 <= 2 =
            game { gameState = GameOver $ Just Player1 }
        | countCells cell board == 0 =
            game { gameState = GameOver Nothing }
        -- 'checkBoardForViable game player' returns 0 if no viable move for 'player' in that game state
        | checkBoardForViable game == 0 && player == Player1 && (player1Stone game) > (maxStone1 game)  =
            game { gameState = GameOver $ Just Player2 }
        | checkBoardForViable game == 0 && player == Player2 && (player1Stone game) > (maxStone1 game) =
            game { gameState = GameOver $ Just Player1 }
        | otherwise = game
        where board = gameBoard game
              cell  = Full Dot
              player = gamePlayer game
              p1    = removeStone1 game
              p2    = removeStone2 game

    -- | 'checkBoardForViable'
    -- This function checks the viability of the board, that a player still has a valid move or not
    checkBoardForViable game
        | traverseBoard game (0, 0) == 0 =
            -- | It returns 0 if there is no viable move in the board for that player
            0
        | otherwise = 1
            -- | Otherwise it returns 1


    -- 'traverseBoard'
    -- This funciton is called by the check board for checking for any viable moves left or not
    -- This function just simply scans the whole board and checks for the possible moves for the current player
    traverseBoard game (x, y)
        | checkNeighbour game (x, y) == 0 && x <= 5 =
            -- | checkNeighbour game (x, y) == 0, Means no viable move for player at (x, y)
            traverseBoard game (x + 1, y)
        | checkNeighbour game (x, y) == 1 && Full player /= board ! (x, y) && x <= 5 =
            traverseBoard game (x + 1, y)
        | checkNeighbour game (x, y) == 0 && x == 6 && y <= 5 =
            traverseBoard game (0, y + 1)
        | checkNeighbour game (x, y) == 1 && Full player /= board ! (x, y) && x == 6 && y <= 5 =
            traverseBoard game (0, y + 1)
        | checkNeighbour game (x, y) == 0 && x == 6 && y == 6 =
            0
        | checkNeighbour game (x, y) == 1 && Full player /= board ! (x, y) && x == 6 && y == 6 =
            0
        | otherwise = 1
            where board = gameBoard game
                  player = gamePlayer game

    -- | 'playerTurn'
    -- This function is for Multi Player deceiding the player turn,
    -- This is the main heart of the program this function implements all the required logic for the game
    playerTurn :: Game ->(Int, Int) -> Game
    playerTurn game cellCoord
        -- If player1Stone <= maxStone1 and chance = player1
        -- then player1Stone++ and replace cellCoord with Full player
        -- Else don't recognise the click (for now)
        | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && (takeOther game) >= 8 && player == Player1 && (player1Stone game) <= (maxStone1 game) =
             checkGameOver
            $ listUnblockerV
            $ listUnblockerH
            $ switchPlayer
            $ playerSwitcherConfirm
            $ game { gameBoard = board // [(cellCoord, Full player)], player1Stone = n1 + 1 }
        | isCoordCorrect cellCoord && board ! cellCoord == Full Player1 && player == Player1 && (removeStone1 game) > 3 && (player1Stone game) > (maxStone1 game) && (movedCoordSet game) ==  0 && (takeOther game) >= 8 =
             -- Store the current clicked coordinates (only if atleast one neighbour is a 'dot')
             -- make 'stored' = 1
             -- Don't give chance to the next player
             -- If stored == 1, then check if curent click is a neighbour of stored coords (and then remove the stored coords and render the current clicked coords)
             checkGameOver
             $ setCoords game cellCoord
             $ checkNeighbour game cellCoord
        | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && player == Player1 && (removeStone1 game) > 3 && (player1Stone game) > (maxStone1 game) && (movedCoordSet game) == 1 && (validCellCoords game cellCoord mCoords) && (takeOther game) >= 8 =
              checkGameOver
              $ listUnblockerV
              $ listUnblockerH
              $ switchPlayer
              $ playerSwitcherConfirm
              $ game { gameBoard = board // [(cellCoord, Full player), (mCoords, Full Dot)],  moveCoords = (-1, -1), movedCoordSet = 0 }
        -- Fly Logic when 3 stones are left
        | isCoordCorrect cellCoord && board ! cellCoord == Full Player1 && player == Player1 && (removeStone1 game) <= 3 && (movedCoordSet game) ==  0 && (takeOther game) >= 8 =
             -- Store the current clicked coordinates (only if atleast one neighbour is a 'dot')
             -- make 'stored' = 1
             -- Don't give chance to the next player
             -- If stored == 1, then check if curent click is a neighbour of stored coords (and then remove the stored coords and render the current clicked coords)
             checkGameOver
             $ setCoords game cellCoord 1
        | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && player == Player1 && (removeStone1 game) <= 3 && (movedCoordSet game) == 1 && board ! cellCoord == Full Dot && (takeOther game) >= 8 =
              checkGameOver
              $ listUnblockerV
              $ listUnblockerH
              $ switchPlayer
              $ playerSwitcherConfirm
              $ game { gameBoard = board // [(cellCoord, Full player), (mCoords, Full Dot)],  moveCoords = (-1, -1), movedCoordSet = 0 }
        -- Fly logic ends
        | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && (takeOther game) >= 8 && player == Player2 && (player2Stone game) <= (maxStone2 game) =
             checkGameOver
            $ listUnblockerV
            $ listUnblockerH
            $ switchPlayer
            $ playerSwitcherConfirm
            $ game { gameBoard = board // [(cellCoord, Full player)], player2Stone = n2 + 1 }
        | isCoordCorrect cellCoord && board ! cellCoord == Full Player2 && player == Player2 && (removeStone2 game) > 3 && (player2Stone game) > (maxStone2 game) && (movedCoordSet game) == 0 && (takeOther game) >= 8 =
             -- Store the current clicked coordinates (only if atleast one neighbour is a 'dot')
             -- make 'stored' = 1
             -- Don't give chance to the next player
             -- If stored == 1, then check if curent click is a neighbour of stored coords (and then remove the stored coords and render the current clicked coords)
             checkGameOver
             $ setCoords game cellCoord
             $ checkNeighbour game cellCoord
        | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && player == Player2 && (removeStone2 game) > 3 && (player2Stone game) > (maxStone2 game) && (movedCoordSet game) == 1 && (validCellCoords game cellCoord mCoords) && (takeOther game) >= 8 =
              checkGameOver
              $ listUnblockerV
              $ listUnblockerH
              $ switchPlayer
              $ playerSwitcherConfirm
              $ game { gameBoard = board // [(cellCoord, Full player), (mCoords, Full Dot)],  moveCoords = (-1, -1), movedCoordSet = 0 }
        -- Fly Logic when 3 stones are left
        | isCoordCorrect cellCoord && board ! cellCoord == Full Player2 && player == Player2 && (removeStone2 game) <= 3 && (movedCoordSet game) ==  0 && (takeOther game) >= 8 =
             -- Store the current clicked coordinates (only if atleast one neighbour is a 'dot')
             -- make 'stored' = 1
             -- Don't give chance to the next player
             -- If stored == 1, then check if curent click is a neighbour of stored coords (and then remove the stored coords and render the current clicked coords)
             checkGameOver
             $ setCoords game cellCoord 1
        | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && player == Player2 && (removeStone2 game) <= 3 && (movedCoordSet game) == 1 && board ! cellCoord == Full Dot && (takeOther game) >= 8 =
              checkGameOver
              $ listUnblockerV
              $ listUnblockerH
              $ switchPlayer
              $ playerSwitcherConfirm
              $ game { gameBoard = board // [(cellCoord, Full player), (mCoords, Full Dot)],  moveCoords = (-1, -1), movedCoordSet = 0 }
        -- Fly logic ends
        | isCoordCorrect cellCoord && board ! cellCoord /= Full Dot && (takeOther game) < 8 && board ! cellCoord /= Full player = switchPlayer1 $ listUnblockerV $ listUnblockerH $ removerV cellCoord  $ removerH cellCoord game
        | otherwise = game
            where board = gameBoard game
                  player = gamePlayer game
                  mCoords = moveCoords game
                  n1 = player1Stone game
                  n2 = player2Stone game


    -- | 'playerTurnB'
    -- This function is for Single Player (when playing against a bot) deceiding the player turn,
    playerTurnB :: Game ->(Int, Int) -> Game
    playerTurnB game cellCoord
        -- If player1Stone <= maxStone1 and chance = player1
        -- then player1Stone++ and replace cellCoord with Full player
        -- Else don't recognise the click (for now)
        |(takeOther game) >= 8 && player == Player1 && (player1Stone game) <= (maxStone1 game) =
             checkGameOver
            $ listUnblockerV
            $ listUnblockerH
            $ switchPlayer
            $ playerSwitcherConfirm
            $ twoInRowChecker game (0,0)

        --calling the bot movement logic for bots turn
        | player == Player1 && (removeStone1 game) > 3 && (player1Stone game) > (maxStone1 game) && (takeOther game) >= 8 =
              checkGameOver
              $ listUnblockerV
              $ listUnblockerH
              $ switchPlayer
              $ playerSwitcherConfirm
              $ botMove game (0,0)

        | player == Player1 && (removeStone1 game) <= 3 && (takeOther game) >= 8 =
              checkGameOver
              $ listUnblockerV
              $ listUnblockerH
              $ switchPlayer
              $ playerSwitcherConfirm
              $ botFlyMove game (0, 0)

        -- Fly logic ends
        | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && (takeOther game) >= 8 && player == Player2 && (player2Stone game) <= (maxStone2 game) =
             checkGameOver
            $ listUnblockerV
            $ listUnblockerH
            $ switchPlayer
            $ playerSwitcherConfirm
            $ game { gameBoard = board // [(cellCoord, Full player)], player2Stone = n2 + 1, playerMovedCoords = cellCoord }
        | isCoordCorrect cellCoord && board ! cellCoord == Full Player2 && player == Player2 && (removeStone2 game) > 3 && (player2Stone game) > (maxStone2 game) && (movedCoordSet game) == 0 && (takeOther game) >= 8 =
             -- Store the current clicked coordinates (only if atleast one neighbour is a 'dot')
             -- make 'stored' = 1
             -- Don't give chance to the next player
             -- If stored == 1, then check if curent click is a neighbour of stored coords (and then remove the stored coords and render the current clicked coords)
             checkGameOver
             $ setCoords game cellCoord
             $ checkNeighbour game cellCoord
        | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && player == Player2 && (removeStone2 game) > 3 && (player2Stone game) > (maxStone2 game) && (movedCoordSet game) == 1 && (validCellCoords game cellCoord mCoords) && (takeOther game) >= 8 =
              checkGameOver
              $ listUnblockerV
              $ listUnblockerH
              $ switchPlayer
              $ playerSwitcherConfirm
              $ game { gameBoard = board // [(cellCoord, Full player), (mCoords, Full Dot)], playerMovedCoords = cellCoord, moveCoords = (-1, -1), movedCoordSet = 0 }
        -- Fly Logic when 3 stones are left
        | isCoordCorrect cellCoord && board ! cellCoord == Full Player2 && player == Player2 && (removeStone2 game) <= 3 && (movedCoordSet game) ==  0 && (takeOther game) >= 8 =
             -- Store the current clicked coordinates (only if atleast one neighbour is a 'dot')
             -- make 'stored' = 1
             -- Don't give chance to the next player
             -- If stored == 1, then check if curent click is a neighbour of stored coords (and then remove the stored coords and render the current clicked coords)
             checkGameOver
             $ setCoords game cellCoord 1
        | isCoordCorrect cellCoord && board ! cellCoord == Full Dot && player == Player2 && (removeStone2 game) <= 3 && (movedCoordSet game) == 1 && board ! cellCoord == Full Dot && (takeOther game) >= 8 =
              checkGameOver
              $ listUnblockerV
              $ listUnblockerH
              $ switchPlayer
              $ playerSwitcherConfirm
              $ game { gameBoard = board // [(cellCoord, Full player), (mCoords, Full Dot)], playerMovedCoords = cellCoord, moveCoords = (-1, -1), movedCoordSet = 0}
        -- Fly logic ends
        | isCoordCorrect cellCoord && board ! cellCoord /= Full Dot && (takeOther game) < 8 && board ! cellCoord /= Full player && player == Player2 = switchPlayer1 $ listUnblockerV $ listUnblockerH $ removerV cellCoord  $ removerH cellCoord game
        |  (takeOther game) < 8 &&  player == Player1 = switchPlayer1 $ listUnblockerV $ listUnblockerH $ removerVB $ removerHB game
        | otherwise = game
            where board = gameBoard game
                  player = gamePlayer game
                  mCoords = moveCoords game
                  n1 = player1Stone game
                  n2 = player2Stone game

    -- 'movedCoordSet' = 1 means the movedCoords are set


    -- | 'validCellCoords'
    -- This function checks for the validity of the corrdinates when selected to move
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


    -- | Helper functions for checking the valid cell coordinates
    -- | 'isUpNeighbour'
    -- This funciton traverse in one direction up down left or right and checks that the given coordinates have free neighbours
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
        | y >= 1 && (x , y - 1) == mCoords =
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

    -- | 'setCoords'
    -- Setting the first clicked coords
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

    -- | 'isUp'
    -- This function return the required data for movement as the neighbous position or if it is present or not
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
   ---------------------------------------------------
    isUpB game (x, y)
     | x <= 5 && board ! (x + 1, y) == Empty =
         isUpB game (x + 1, y)
     | x <= 5 && board ! (x + 1, y) == Full Dot =
         (x+1, y)
     | otherwise = (-1,-1)
         where board = gameBoard game

    isDownB game (x, y)
        | x >= 1 && board ! (x - 1, y) == Empty =
            isDownB game (x - 1, y)
        | x >= 1 && board ! (x - 1, y) == Full Dot =
            (x-1, y)
        | otherwise = (-1,-1)
            where board = gameBoard game

    isLeftB game (x, y)
        | y >= 1 && board ! (x, y - 1) == Empty =
            isLeftB game (x, y - 1)
        | y >= 1 && board ! (x, y - 1) == Full Dot =
            (x, y - 1)
        | otherwise = (-1,-1)
            where board = gameBoard game

    isRightB game (x, y)
        | y <= 5 && board ! (x, y + 1) == Empty =
            isRightB game (x, y + 1)
        | y <= 5 && board ! (x, y + 1) == Full Dot =
            (x, y + 1)
        | otherwise = (-1,-1)
            where board = gameBoard game

    -------------------------------------------------------------
    isUpBB game (x, y)
        | x <= 5 && board ! (x + 1, y) == Empty =
            isUpBB game (x + 1, y)
        | x <= 5 && board ! (x + 1, y) /= Full Dot =
            (x+1, y)
        | otherwise = (-1,-1)
            where board = gameBoard game

    isDownBB game (x, y)
        | x >= 1 && board ! (x - 1, y) == Empty =
            isDownBB game (x - 1, y)
        | x >= 1 && board ! (x - 1, y) /= Full Dot =
            (x-1, y)
        | otherwise = (-1,-1)
            where board = gameBoard game

    isLeftBB game (x, y)
        | y >= 1 && board ! (x, y - 1) == Empty =
            isLeftBB game (x, y - 1)
        | y >= 1 && board ! (x, y - 1) /= Full Dot =
            (x, y - 1)
        | otherwise = (-1,-1)
            where board = gameBoard game

    isRightBB game (x, y)
        | y <= 5 && board ! (x, y + 1) == Empty =
            isRightBB game (x, y + 1)
        | y <= 5 && board ! (x, y + 1) /= Full Dot =
            (x, y + 1)
        | otherwise = (-1,-1)
            where board = gameBoard game

    -- | Function checks that are there and two in a line or not
    -- 'isRightTwoB', 'isLeftTwoB', 'isDownTwoB', 'isUpTwoB', 'isVOneB', 'isHOneB'

    isRightTwoB game (x,y) | isRightBB game (x,y) /= (-1,-1) && isRightBB game (isRightBB game (x,y)) /= (-1,-1) && board ! isRightBB game (x,y) == board ! isRightBB game (isRightBB game (x,y)) = (x,y)
                           | otherwise = (-1,-1)
                                where board = gameBoard game

    isLeftTwoB game (x,y)  | isLeftBB game (x,y) /= (-1,-1) && isLeftBB game (isLeftBB game (x,y)) /= (-1,-1) && board ! isLeftBB game (x,y) == board ! isLeftBB game (isLeftBB game (x,y)) = (x,y)
                           | otherwise = (-1,-1)
                                where board = gameBoard game

    isDownTwoB game (x,y)  | isDownBB game (x,y) /= (-1,-1) && isDownBB game (isDownBB game (x,y)) /= (-1,-1) && board ! isDownBB game (x,y) == board ! isDownBB game (isDownBB game (x,y)) = (x,y)
                           | otherwise = (-1,-1)
                                where board = gameBoard game

    isUpTwoB game (x,y)    | isUpBB game (x,y) /= (-1,-1) && isUpBB game (isUpBB game (x,y)) /= (-1,-1) && board ! isUpBB game (x,y) == board ! isUpBB game (isUpBB game (x,y)) = (x,y)
                           | otherwise = (-1,-1)
                                where board = gameBoard game

    isVOneB game (x,y)     | isUpBB game (x,y) /= (-1,-1) && isDownBB game (x,y) /= (-1,-1) && board ! isUpBB game (x,y) == board ! isDownBB game (x,y) = (x,y)
                           | otherwise = (-1,-1)
                                where board = gameBoard game

    isHOneB game (x,y)     | isRightBB game (x,y) /= (-1,-1) && isLeftBB game (x,y) /= (-1,-1) && board ! isRightBB game (x,y) == board ! isLeftBB game (x,y) = (x,y)
                           | otherwise = (-1,-1)
                                where board = gameBoard game


    -- | 'transformGame'
    -- This function recognise the mouse position and converts it into the cell position where the user clicks
    transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
        case gameState game of
            Menu -> openGame game $ mousePosAsMenuCellCoord mousePos
            Running -> playerTurn game $ mousePosAsCellCoord mousePos
            RunningB -> playerTurnB game $ mousePosAsCellCoord mousePos
            GameOver _ -> initialGame
    transformGame _ game = game

    -- | 'openGame'
    -- This function is used in displaying the menu
    openGame :: Game ->(Int, Int) -> Game
    openGame game menuCell
        | menuCell == (0, 0) = game { gameState = Running }
        | menuCell == (0, 1) = game { gameState = RunningB }


    -- | Placing logic ::
        -- -> when some one places a stone check for that its movement has created a morris or not
        -- -> the horiontal line funcion when mapped creates a list showing that are there any horizontal morris or not
        -- -> same is done by verticle
        -- -> as the game rules once a morris is formed that norris gets blocked until some change or movement are made in that morris
        -- -> so there are take other functions which checks that the morris formed is a new one or the old one
        -- -> if the morris is a new one than you give the current player a second chance to take opposite players stone
        -- -> this locking and unloccking thing is mainted by the list called checklistH and checklistV
        -- -> there are funcions called remover which removes the stone of oppostie layer and blocks the list by which the current player got chance to take
        -- -> similarly there are unblock list funciton which unblocks the morris list when there are any changes or movement in the configuration

    horizontalLine game  [row, column, distance]    | board!(row, column) == board!(row, column + distance) && board!(row, column) == board!(row, column + 2*distance) && board!(row, column) == player  = player
                                                              | otherwise = Full Dot
                                                                    where  board = gameBoard game
                                                                           player = Full $ gamePlayer game

    verticleLine game  [column, row, distance]      | board!(row, column) == board!(row + distance, column) && board!(row, column) == board!(row + 2*distance, column) && board!(row, column) == player  = player
                                                              | otherwise = Full Dot
                                                                    where  board = gameBoard game
                                                                           player = Full $ gamePlayer game

    horizontalLineUnblock game  [row, column, distance]    | (board!(row, column) /= board!(row, column + distance) || board!(row, column) /= board!(row, column + 2*distance))   = Full Dot
                                                              | otherwise = player
                                                                    where  board = gameBoard game
                                                                           player = Full $ gamePlayer game

    verticleLineUnblock game  [column, row, distance]      | (board!(row, column) /= board!(row + distance, column) || board!(row, column) /= board!(row + 2*distance, column))  = Full Dot
                                                              | otherwise = player
                                                                    where  board = gameBoard game
                                                                           player = Full $ gamePlayer game


    -- | Mapping above function to create a list
    -- 'finalHorizontalCheck', 'finalVerticalCheck'
    finalHorizontalCheck game  = map (horizontalLine game) $ gameList game
    finalVerticalCheck game = map (verticleLine game) $ gameList game

    finalHorizontalCheckUnblocker game  = map (horizontalLineUnblock game) $ gameList game
    finalVerticalCheckUnbloccker game = map (verticleLineUnblock game) $ gameList game

    -- | Blocker functions
    -- 'takeOtherH', 'takeOtherV', 'takeOther'
    takeOtherH game n       | n < 8 && (finalHorizontalCheck game)!!n == player && validityH!!n == 1 = n
                            | n == 8 = 8
                            | otherwise =  takeOtherH game (n + 1)
                                where board = gameBoard game
                                      player = Full $ gamePlayer game
                                      validityH = checkListH game

    takeOtherV game n       | n < 8 && ((finalVerticalCheck game)!!n == player) && validityV!!n == 1 = n
                            | n == 8 = 8
                            | otherwise =  takeOtherV game (n + 1)
                                where board = gameBoard game
                                      player = Full $ gamePlayer game
                                      validityV = checkListV game

    takeOther game         |  (((takeOtherH game 0) < 8) || ((takeOtherV game 0) < 8)) = 1
                            | otherwise = 8


    -- | Unblocker funcitons
    -- 'unblockOtherH', 'unblockOtherV', 'listUnblockerH', 'listUnblockerV'
    unblockOtherH game k    | k < 8 && ((finalHorizontalCheckUnblocker game)!!k == Full Dot) && validityH!!k == 0 = k
                            | k == 8 = 8
                            | otherwise =  unblockOtherH game (k + 1)
                                where board = gameBoard game
                                      validityH = checkListH game

    unblockOtherV game k   | k < 8 && ((finalVerticalCheckUnbloccker game)!!k == Full Dot) && validityV!!k == 0 = k
                            | k == 8 = 8
                            | otherwise =  unblockOtherV game (k + 1)
                                where board = gameBoard game
                                      validityV = checkListV game

    listUnblockerH  game | (unblockOtherH game 0) < 8 = game { checkListH = (replaceNth 1 validityH $ unblockOtherH game 0)}
                         | otherwise = game
                            where validityH = checkListH game

    listUnblockerV  game | (unblockOtherV game 0) < 8 = game { checkListV = (replaceNth 1 validityV $ unblockOtherV game 0)}
                         | otherwise = game
                            where validityV = checkListV game

    -- | 'replaceNth'
    -- Funciton to replace nth postion of list with new value
    replaceNth newVal (x:xs) n
                            | n == 0 = newVal:xs
                            | otherwise = x:replaceNth newVal xs (n - 1)


    -- | 'removerH', 'removerV'
    -- Funcitons for removing others stone and blocking the list
    removerH cellCoord game          | (takeOtherH game 0) < 8 && player == Player1  = game { gameBoard = board // [(cellCoord, Full Dot)], removeStone2 = stone2 - 1,   checkListH = (replaceNth 0 listH $ takeOtherH game 0)}
                                     | (takeOtherH game 0) < 8 && player == Player2  = game { gameBoard = board // [(cellCoord, Full Dot)], removeStone1 = stone1 - 1,   checkListH = (replaceNth 0 listH $ takeOtherH game 0)}
                                     | otherwise = game
                                        where board = gameBoard game
                                              listH = checkListH game
                                              player = gamePlayer game
                                              stone1 = removeStone1 game
                                              stone2 = removeStone2 game



    removerV cellCoord game          | (takeOtherV game 0) < 8 && player == Player1  = game { gameBoard = board // [(cellCoord, Full Dot)], removeStone2 = stone2 - 1,  checkListV = (replaceNth 0 listV $ takeOtherV game 0)}
                                     | (takeOtherV game 0) < 8 && player == Player2  = game { gameBoard = board // [(cellCoord, Full Dot)], removeStone1 = stone1 - 1,  checkListV = (replaceNth 0 listV $ takeOtherV game 0)}
                                     | otherwise = game
                                        where board = gameBoard game
                                              listV = checkListV game
                                              player = gamePlayer game
                                              stone1 = removeStone1 game
                                              stone2 = removeStone2 game
    -- | These are for bot
    -- 'removerHB', 'removerVB'
    removerHB game          | (takeOtherH game 0) < 8 && threeTakerH game 0 /= (-1,-1)  = game { gameBoard = board // [(threeTakerH game 0, Full Dot)], removeStone2 = stone2 - 1,   checkListH = (replaceNth 0 listH $ takeOtherH game 0)}
                            | (takeOtherH game 0) < 8  = game { gameBoard = board // [(traverseBoardBotTake game (0,0), Full Dot)], removeStone2 = stone2 - 1,   checkListH = (replaceNth 0 listH $ takeOtherH game 0)}
                            | otherwise = game
                                        where board = gameBoard game
                                              listH = checkListH game
                                              player = gamePlayer game
                                              stone1 = removeStone1 game
                                              stone2 = removeStone2 game



    removerVB game          | (takeOtherV game 0) < 8 && threeTakerV game 0 /= (-1,-1)  = game { gameBoard = board // [(threeTakerV game 0, Full Dot)], removeStone2 = stone2 - 1,  checkListV = (replaceNth 0 listV $ takeOtherV game 0)}
                            | (takeOtherV game 0) < 8  = game { gameBoard = board // [(traverseBoardBotTake game (0,0), Full Dot)], removeStone2 = stone2 - 1,  checkListV = (replaceNth 0 listV $ takeOtherV game 0)}
                            | otherwise = game
                                        where board = gameBoard game
                                              listV = checkListV game
                                              player = gamePlayer game
                                              stone1 = removeStone1 game
                                              stone2 = removeStone2 game

    playerSwitcherConfirm game | (takeOther game)  < 8 = game { checker = 0}
                               | otherwise = game { checker = 1}

    -- |  Movement Logic ::
        -- -> First we store the corrdinates of the click position by player in a variable
        -- -> then we check wheter the coordinates are coorect or not
        -- -> if correct we let him to click to other neighbour
        -- -> if that are also correct we just move
        -- -> bot also follows same logic but it does both step in one
        -- -> it first checks if he can make or block a morris
        -- -> if not than it tries to block the player


    -- | 'botMoveThree'
    -- Function to check that this empty postion is where a morris can be formed
    botMoveThree game (x,y)     | isRightB game (x,y) /= (-1,-1) && twoInRowCheckerBotMove game (isRightB game (x,y)) /= (-1,-1) = isRightB game (x,y)
                                | isLeftB game (x,y) /= (-1,-1) && twoInRowCheckerBotMove game (isLeftB game (x,y)) /= (-1,-1) = isLeftB game (x,y)
                                | isDownB game (x,y) /= (-1,-1) && twoInRowCheckerBotMove game (isDownB game (x,y)) /= (-1,-1) = isDownB game (x,y)
                                | isUpB game (x,y) /= (-1,-1) && twoInRowCheckerBotMove game (isUpB game (x,y)) /= (-1,-1) = isUpB game (x,y)
                                | otherwise = (-1,-1)


    -- | 'simpleTraverseBotMove'
    -- Find the first position on board for bot for which it can move
    simpleTraverseBotMove game (x,y) | ((board ! (x,y) /= Full Player1) || (board ! (x,y) == Full Player1 && checkNeighbour game (x,y) == 0)) && x <= 5 = simpleTraverseBotMove game (x + 1, y)
                                     | ((board ! (x,y) /= Full Player1) || (board ! (x,y) == Full Player1 && checkNeighbour game (x,y) == 0)) && x == 6 && y <= 5 = simpleTraverseBotMove game (0, y + 1)
                                     | ((board ! (x,y) /= Full Player1) || (board ! (x,y) == Full Player1 && checkNeighbour game (x,y) == 0)) && x == 6 && y == 6 = (-1,-1)
                                     | otherwise = (x,y)
                                         where board = gameBoard game
                                               n1  = player1Stone game

    -- | 'posneighSimpTrv'
    -- Movement to its neighbouur
    posneighSimpTrv game (x,y)  | isUpB game (x,y) /= (-1,-1) = isUpB game (x,y)
                                | isRightB game (x,y) /= (-1,-1) = isRightB game (x,y)
                                | isDownB game (x,y) /= (-1,-1) = isDownB game (x,y)
                                | isLeftB game (x,y) /= (-1,-1) = isLeftB game (x,y)



    -- | 'botMove'
    -- Main function to handle bot movement
    botMove game (x,y)      | board ! (x,y) == Full Player1 && botMoveThree game (x,y) /= (-1,-1) = game { gameBoard = board // [((x,y),Full Dot), ((botMoveThree game (x,y)), Full Player1)]}
                            | x <= 5 = botMove game (x + 1, y)
                            | x==6 && y <= 5 = botMove game (0, y + 1)
                            | otherwise = game { gameBoard =  board // [((simpleTraverseBotMove game (0,0)), Full Dot), ((posneighSimpTrv game (simpleTraverseBotMove game (0,0))), Full Player1)]}
                                where board = gameBoard game



    -- | Move When Less than 3 stones
        -- -> when a bot or player has less than three stones than it can move its stone to anywhere on board
        -- -> so the locgic is implemented here

    -- | 'botMoveThreeFly'
    -- Fly to the corrdinate were morris can be formed or blocked
    botMoveThreeFly game (x, y)   | twoInRowCheckerBotMove game ((x, y)) /= (-1,-1) = (x, y)
                                  | twoInRowCheckerBotMove game ((x, y)) /= (-1,-1) = (x, y)
                                  | twoInRowCheckerBotMove game ((x, y)) /= (-1,-1) = (x, y)
                                  | twoInRowCheckerBotMove game ((x, y)) /= (-1,-1) = (x, y)
                                  | x <= 5 = botMoveThreeFly game (x + 1, y)
                                  | x == 6 && y <= 5 = botMoveThreeFly game (0, y + 1)
                                  | otherwise = (-1,-1)

    -- | 'botFlyMove'
    -- Checking bot fly movement
    botFlyMove game (x, y)      | board ! (x, y) == Full Player1 && botMoveThreeFly game (0, 0) /= (-1, -1) =
                                      game { gameBoard = board // [ ((x, y), Full Dot), ((botMoveThreeFly game (0, 0)), Full Player1) ]}
                                | x <= 5 = botFlyMove game (x + 1, y)
                                | x == 6 && y <= 5 = botFlyMove game (0, y + 1)
                                | otherwise =
                                      game { gameBoard =  board // [((simpleTraverseBotMove game (0,0)), Full Dot), ((posneighSimpTrv game (simpleTraverseBotMove game (0,0))), Full Player1)]}
                                    where board = gameBoard game

    -- | 'traverseBoardBot'
    -- Traverse board for simple board movement
    traverseBoardBot game (x, y)    | board ! (x,y) /= Full Dot && x <= 5 = traverseBoardBot game (x + 1, y)
                                    | board ! (x,y) /= Full Dot && x == 6 && y <= 5 = traverseBoardBot game (0, y + 1)
                                    | board ! (x,y) /= Full Dot && x == 6 && y == 6 = game
                                    | otherwise = game { gameBoard = board // [((x, y), Full Player1)], player1Stone = n1 + 1, botCoords = (x,y)}
                                        where board = gameBoard game
                                              n1  = player1Stone game

    threeTakerH game  n      |  n < 8 && (validityH!!n == 0 && board ! ((list!!n)!!0, (list!!n)!!1) == Full Player2)  = ((list!!n)!!0, (list!!n)!!1)
                             |  n == 8 = (-1,-1)
                             | otherwise = threeTakerH game (n + 1)
                                where board = gameBoard game
                                      list = gameList game
                                      validityH = checkListH game

    threeTakerV game n       |  n < 8 && (validityV!!n == 0 && board ! ((list!!n)!!1, (list!!n)!!0) == Full Player2) = ((list!!n)!!1, (list!!n)!!0)
                             |  n == 8 = (-1,-1)
                             |  otherwise = threeTakerV game (n + 1)
                                    where board = gameBoard game
                                          list = gameList game
                                          validityV = checkListV game


    traverseBoardBotTake game (x, y)    | board ! (x,y) /= Full Player2 && x <= 5 = traverseBoardBotTake game (x + 1, y)
                                        | board ! (x,y) /= Full Player2 && x == 6 && y <= 5 = traverseBoardBotTake game (0, y + 1)
                                        | board ! (x,y) /= Full Player2 && x == 6 && y == 6 = (-1,-1)
                                        | otherwise = (x,y)
                                            where board = gameBoard game

    traverseBoardBotMove game (x, y)    | board ! (x,y) /= Full Player1 && x <= 5 = traverseBoardBotTake game (x + 1, y)
                                        | board ! (x,y) /= Full Player1 && x == 6 && y <= 5 = traverseBoardBotTake game (0, y + 1)
                                        | board ! (x,y) /= Full Player1 && x == 6 && y == 6 = (-1,-1)
                                        | otherwise = (x,y)
                                            where board = gameBoard game

    -- | 'twoInRowChecker'
    -- This function checks are there any two same placed stone in a row
    twoInRowChecker game (x,y) | board ! (x,y) == Full Dot && (elem (x,y) twoHCoord) && (isRightTwoB game (x,y) /= (-1,-1) || isLeftTwoB game (x,y) /= (-1,-1)) = game { gameBoard = board // [((x,y),Full Player1)], player1Stone = n1 + 1, botCoords = (x,y)}
                               | board ! (x,y) == Full Dot && (elem (x,y) twoVCoord) && (isUpTwoB game (x,y) /= (-1,-1) || isDownTwoB game (x,y) /= (-1,-1)) = game { gameBoard = board // [((x,y),Full Player1)], player1Stone = n1 + 1, botCoords = (x,y)}
                               | board ! (x,y) == Full Dot && (elem (x,y) oneHCoord) && (isHOneB game (x,y) /= (-1,-1)) = game { gameBoard = board // [((x,y),Full Player1)], player1Stone = n1 + 1, botCoords = (x,y)}
                               | board ! (x,y) == Full Dot && (elem (x,y) oneVCoord) && (isVOneB game (x,y) /= (-1,-1)) = game { gameBoard = board // [((x,y),Full Player1)], player1Stone = n1 + 1, botCoords = (x,y)}
                               | x <= 5 = twoInRowChecker game (x + 1, y)
                               | x == 6 && y <= 5 = twoInRowChecker game (0, y + 1)
                               | board ! (x,y) /= Full Dot && x == 6 && y == 6 = botMoveOnPlayer game
                               | otherwise = botMoveOnPlayer game
                                    where board = gameBoard game
                                          cellCoord = playerMovedCoords game
                                          n1 = player1Stone game

    -- | 'twoInRowCheckerBotMove'
    -- To take action accordin to two stones in a row
    twoInRowCheckerBotMove game (x,y) | board ! (x,y) == Full Dot && (elem (x,y) twoHCoord) && (isRightTwoB game (x,y) /= (-1,-1) || isLeftTwoB game (x,y) /= (-1,-1)) = (x,y)
                                      | board ! (x,y) == Full Dot && (elem (x,y) twoVCoord) && (isUpTwoB game (x,y) /= (-1,-1) || isDownTwoB game (x,y) /= (-1,-1)) = (x,y)
                                      | board ! (x,y) == Full Dot && (elem (x,y) oneHCoord) && (isHOneB game (x,y) /= (-1,-1)) = (x,y)
                                      | board ! (x,y) == Full Dot && (elem (x,y) oneVCoord) && (isVOneB game (x,y) /= (-1,-1)) = (x,y)
                                      | otherwise = (-1,-1)
                                           where board = gameBoard game
                                                 cellCoord = playerMovedCoords game
                                                 n1 = player1Stone game


    -- | 'botMoveOnPlayer'
    -- The function which tries to block the player when no morris can be formed or blocked
    botMoveOnPlayer game | isRightB game cellCoord /= (-1,-1) = game { gameBoard = board // [(isRightB game cellCoord,Full Player1)], player1Stone = n1 + 1, botCoords = isRightB game cellCoord}
                         | isLeftB game cellCoord /= (-1,-1) = game { gameBoard = board // [(isLeftB game cellCoord,Full Player1)], player1Stone = n1 + 1, botCoords = isLeftB game cellCoord}
                         | isUpB game cellCoord /= (-1,-1) = game { gameBoard = board // [(isUpB game cellCoord,Full Player1)], player1Stone = n1 + 1, botCoords = isUpB game cellCoord}
                         | isDownB game cellCoord /= (-1,-1) = game { gameBoard = board // [(isDownB game cellCoord,Full Player1)], player1Stone = n1 + 1, botCoords = isDownB game cellCoord}
                         | otherwise =  traverseBoardBot game (0,0)
                            where board = gameBoard game
                                  cellCoord = playerMovedCoords game
                                  n1 = player1Stone game
