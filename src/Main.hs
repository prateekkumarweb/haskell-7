-- | Main module
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Rendering
import Logic1

-- | Creating a window
window :: Display
window = InWindow "Nine Men Morris" (screenWidth, screenHeight) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 255 222 173 255

-- | Main function which initialize the game
main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture (transformGame) (const id)
