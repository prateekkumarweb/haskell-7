module Logic where

import Data.Array



type Board = Array (Int, Int) 
n :: Int
n = 7


listOfN = take a (repeat 1)
    where a = 18


b :: Int
b = 0


createBoard =  (array indexRange $ zip (range indexRange) (repeat b))
    where indexRange = ((0,0), (n - 1, n - 1))




horizontalLine board [row, column, distance]    | board!(row, column) == board!(row, column + distance) && board!(row, column) == board!(row, column + 2*distance) && board!(row, column) == 1 && listOfN!!row == 1 = 1
                                                | board!(row, column) == board!(row, column + distance) && board!(row, column) == board!(row, column + 2*distance) && board!(row, column) == 2 && listOfN!!row == 1 = 2
                                                | otherwise = b
    
                                           

                                         
listForCheck = [[0,0,3], [1,1,2], [2,2,1], [3,0,1], [3,4,1], [4,2,1], [5,1,2], [6,0,3]]