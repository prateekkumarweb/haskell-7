module Main where

import Graphics.Gloss 
import Graphics.Gloss.Data.Color

import Game

window :: Display
window = InWindow "Circle Window" (640, 480) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing