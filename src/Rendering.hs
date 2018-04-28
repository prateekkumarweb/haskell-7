module Rendering where

import Data.Array

import Graphics.Gloss

import Game

import Logic1

player1Color = makeColorI 255 50 50 255
player2Color = makeColorI 50 100 255 255
gridLightPlayer1 = makeColorI 255 91 91 255
gridLightPlayer2 = makeColorI 91 131 255 255
blackcolor = makeColorI 0 0 0 255
whitecolor = makeColorI 255 255 255 255
tieColor = greyN 0.5

boardGrid :: Picture
boardGrid =
     pictures [ line [(46.0, 46.0),(598.0,  46.0)],
                line [(46.0, 46.0),(46.0,  598.0)],
                line [(46.0, 598.0),(598.0,  598.0)],
                line [(598.0, 46.0),(598.0,  598.0)],
                line [(138.0, 138.0),(506.0,  138.0)],
                line [(138.0, 506.0),(506.0,  506.0)],
                line [(506.0, 138.0),(506.0,  506.0)],
                line [(138.0, 138.0),(138.0,  506.0)],
                line [(230.0, 230.0),(230.0,  414.0)],
                line [(230.0, 230.0),(414.0,  230.0)],
                line [(230.0, 414.0),(414.0,  414.0)],
                line [(414.0, 230.0),(414.0,  414.0)],
                line [(46.0, 322.0),(230.0,  322.0)],
                line [(414.0, 322.0),(598.0,  322.0)],
                line [(322.0, 46.0),(322.0, 230.0)],
                line [(322.0, 414.0),(322.0,  598.0)]]

menuBoardLine :: Picture
menuBoardLine =
              pictures [ line [(322.0, 644.0), (322.0, 0.0)] ]

rectangleMenuBlack =
              pictures [ translate x y $ rectangleSolid 644.0 644.0 ]
                  where x = 0.0
                        y = fromIntegral screenHeight * 0.5

tileRenderMulti :: Picture
tileRenderMulti = Text "Multi Player"

tileRenderSingle :: Picture
tileRenderSingle =  Text "Single Player"

boardMenuPicture game board =
          pictures [  color blackcolor $ menuBoardLine
                      , color blackcolor $ rectangleMenuBlack
                      , color whitecolor $ tileRenderMultiCell board
                      , color blackcolor $ tileRenderSingleCell board ]

boardAsRunningPicture game board
    | player == Player1 =
          pictures [ color gridLightPlayer1 $ dotCellsOfBoard board
                    , color gridLightPlayer1 $ boardGrid
                    , color player1Color $ aCellsOfBoard board
                    , color player2Color $ bCellsOfBoard board
                   ]
   | player == Player2 =
         pictures [ color gridLightPlayer2 $ dotCellsOfBoard board
                   , color gridLightPlayer2 $ boardGrid
                   , color player1Color $ aCellsOfBoard board
                   , color player2Color $ bCellsOfBoard board
                  ]
        where player = gamePlayer game

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

snapPictureToMenuCell picture (row, column) = translate x y picture
    where x = fromIntegral screenWidth * (column * 0.5 + 0.09)
          y = fromIntegral screenHeight * (row + 0.5)

aCell :: Picture
aCell = circleSolid radius
    where radius = min cellWidth cellHeight * 0.25

bCell :: Picture
bCell = circleSolid radius
    where radius = min cellWidth cellHeight * 0.25

dotCell :: Picture
dotCell = circleSolid radius
    where radius = min cellWidth cellHeight * 0.08

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

menuCellsBoard board choice menuPicture =
    pictures
    $ map (snapPictureToMenuCell menuPicture . fst)
    $ filter (\(_, e) -> e == choice)
    $ [((0, 0), Multi), ((0, 1), Single)]

tileRenderMultiCell board = menuCellsBoard board Multi $ scale 0.25 0.25 tileRenderMulti

tileRenderSingleCell board = menuCellsBoard board Single $ scale 0.25 0.25 tileRenderSingle

aCellsOfBoard :: Board -> Picture
aCellsOfBoard board = cellsOfBoard board (Full Player1) aCell

bCellsOfBoard :: Board -> Picture
bCellsOfBoard board = cellsOfBoard board (Full Player2) bCell

dotCellsOfBoard :: Board -> Picture
dotCellsOfBoard board = cellsOfBoard board (Full Dot) dotCell

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                              (fromIntegral screenHeight * (-0.5))
                              frame
   where frame = case gameState game of
                   Menu -> boardMenuPicture game (menuBoard game)
                   Running -> boardAsRunningPicture game (gameBoard game)
                   RunningB -> boardAsRunningPicture game (gameBoard game)
