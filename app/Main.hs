module Main (main) where

import Lib
import End 
import Choose
import Err
import State
import Data.List (nub)
import Util
import LockConc
import Programs 
import ABPModel

-- example of possible programs to run with our implementation
main :: IO ()
main = do

  let result :: [(Either String Int, [Bool])]
      result = un(handle hChooseLog (handle hErr choiceAndError))

  let result2 :: [(Either String (Int, Bool), [Int])]
      result2 = un(handle hChoose' (handle_ hState' (handle hErr pairing) [0::Int]))

  let result3 :: [(Either String ((Int, Bool), Bool), [Int])]
      result3 = un(handle hChoose' (handle_ hState' ((handle hErr triplepairing)) [0::Int]))

  let result4 ::[(Either String ((Int, Bool), Bool), [Int])] 
      result4 = un(handle hChoose' (handle_ hState' ((handle hErr triplepairing2)) [0::Int]))
  
  let result5 ::[((), [Int])]
      result5 = un(handle hChoose' (dabpmodel 20 [0::Int,1,2,3,4]))
  
  printElementCounts result4
  print "length"
  print $ length result4
  print "different elements"
  print $ length $ nub result4

  printElementCounts result3 
  print "length"
  print $ length result3
  print "different elements"
  print $ length $ nub result3
