module Game where


  import Data.Array

  -- | Data fields required
  -- | 'Player' returns which player is on the cell
  data Player = Player1 | Player2 | Dot deriving(Eq, Show)
  -- | 'Cell' shows the state of the cell
  data Cell = Empty | Full Player deriving (Eq, Show)
  -- | 'State' is the state of teh game
  data State = Menu | Running | RunningB | GameOver (Maybe Player) deriving (Eq, Show)

  -- | 'Board' is for storing the 2-D array
  type Board = Array (Int, Int) Cell

  -- | 'Choice' is the choice the user selects in the menu
  data Choice = Multi | Single deriving(Eq, Show)
  -- | 'MenuBoard' is the array to represent the menu board
  type MenuBoard = Array (Int, Int) Choice

  -- | 'Game' returns the current state of teh game witn all it's member variables
  data Game  = Game { gameBoard :: Board,
                      menuBoard :: MenuBoard,
                      gamePlayer :: Player,
                      gameState :: State,
                      player1Stone :: Int,
                      player2Stone :: Int,
                      maxStone1 :: Int,
                      maxStone2 :: Int,
                      removeStone1 :: Int,
                      removeStone2 :: Int,
                      moveCoords :: (Int, Int),
                      movedCoordSet :: Int,
                      gameList :: [[Int]],
                      checkListH :: [Int],
                      checkListV :: [Int],
                      checker :: Int,
                      playerMovedCoords :: (Int, Int),
                      botCoords :: (Int, Int)
                    } deriving(Eq, Show)

  -- | 'n' is the size of the grid
  n :: Int
  n = 7

  -- | This are the list required for checking when a player forms a morris
  -- 'listForCheck', 'listForHorizontalCheck', 'listForVerticalCheck'
  listForCheck :: [[Int]]
  listForCheck = [[0,0,3], [1,1,2], [2,2,1], [3,0,1], [3,4,1], [4,2,1], [5,1,2], [6,0,3]]

  listForHorizontalCheck :: [Int]
  listForHorizontalCheck = [1,1,1,1,1,1,1,1]

  listForVerticalCheck :: [Int]
  listForVerticalCheck = [1,1,1,1,1,1,1,1]

  -- | Screen window discription
  -- screenWidth = 644
  -- screenHeight = 644
  screenWidth :: Int
  screenWidth = 644

  screenHeight :: Int
  screenHeight = 644

  cellWidth :: Float
  cellWidth = fromIntegral screenWidth / fromIntegral n

  cellHeight :: Float
  cellHeight = fromIntegral screenHeight / fromIntegral n

  -- | 'initialGame'
  -- Initial constructor for game
  initialGame :: Game
  initialGame = Game { gameBoard = (array indexRange $ zip (range indexRange) (cycle [Empty])) // [ ((0, 0), Full Dot),
                                                                                                    ((0, 3), Full Dot),
                                                                                                    ((0, 6), Full Dot),
                                                                                                    ((1, 1), Full Dot),
                                                                                                    ((1, 3), Full Dot),
                                                                                                    ((1, 5), Full Dot),
                                                                                                    ((2, 2), Full Dot),
                                                                                                    ((2, 3), Full Dot),
                                                                                                    ((2, 4), Full Dot),
                                                                                                    ((3, 0), Full Dot),
                                                                                                    ((3, 1), Full Dot),
                                                                                                    ((3, 2), Full Dot),
                                                                                                    ((3, 4), Full Dot),
                                                                                                    ((3, 5), Full Dot),
                                                                                                    ((3, 6), Full Dot),
                                                                                                    ((4, 2), Full Dot),
                                                                                                    ((4, 3), Full Dot),
                                                                                                    ((4, 4), Full Dot),
                                                                                                    ((5, 1), Full Dot),
                                                                                                    ((5, 3), Full Dot),
                                                                                                    ((5, 5), Full Dot),
                                                                                                    ((6, 0), Full Dot),
                                                                                                    ((6, 3), Full Dot),
                                                                                                    ((6, 6), Full Dot) ]
                       , menuBoard = (array indexRange1 $ zip (range indexRange1) (cycle [Multi])) // [ ((0, 0), Multi),
                                                                                                        ((0, 1), Single) ]
                       , gamePlayer = Player2
                       , gameState = Menu
                       , player1Stone = 0
                       , player2Stone = 0
                       , maxStone1 = 8
                       , maxStone2 = 8
                       , removeStone1 = 9
                       , removeStone2 = 9
                       , moveCoords = (-1, -1)
                       , movedCoordSet = 0
                       , gameList = listForCheck
                       , checkListH = listForHorizontalCheck
                       , checkListV = listForVerticalCheck
                       , checker = 1
                       , playerMovedCoords = (-1, -1)
                       , botCoords = (-1, -1)
                     }
            where indexRange = ((0, 0), (n - 1, n - 1))
                  indexRange1 = ((0, 0), (0, 1))
