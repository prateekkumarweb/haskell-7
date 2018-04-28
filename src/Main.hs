--main module
module Main where

-- importing required libraries
import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Rendering
import Logic1

--creating a window
window :: Display
window = InWindow "Nine Men Morris" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 255 222 173 255

--main function which initialize the game
main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture (transformGame) (const id)
