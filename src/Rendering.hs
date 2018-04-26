module Rendering where

import Data.Array

import Graphics.Gloss

import Game

import Logic

player1Color = makeColorI 255 50 50 255
player2Color = makeColorI 50 100 255 255
boardGridColor = makeColorI 139 69 19 255
dotColor = makeColorI 139 69 19 255
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
                 


boardAsRunningPicture board =
    pictures [ color dotColor $ dotCellsOfBoard board
              , color boardGridColor $ boardGrid
              , color player1Color $ aCellsOfBoard board
              , color player2Color $ bCellsOfBoard board
             ]

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

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
                   Running -> boardAsRunningPicture (gameBoard game)
