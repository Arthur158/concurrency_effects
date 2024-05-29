module Main (main) where

import Lib
import End 
import Choose
import Err
import State
import Choose (hChoose', hChooseTrue, hChooseOrder)
import Data.List (nub)
import Util
import LockConc
import Programs 

main :: IO ()
main = do
  -- let result :: (Either String [Int], [Int])
  --     result = un ((handle_ hState' (handle hErr incerr')[0]))
  -- let result :: Maybe (Either String Bool, [Int])
  --     result = un(handle hChooseTrue (handle_ hState' (handle hErr useZero)[0:: Int]))
  -- let result :: [(Either String [Int], [Int])]
  --     result = unToND(handleToND hChooseToND (handle_ hState' (handle hErr ancerrDouble)[0:: Int]))
  -- let result :: ([Either String [Int]], [Int])
  --     result = un(handle_ hState' (handle hChoose' (handle hErr ancerrDouble))[0:: Int])
  -- let result :: ([Either String ([Int], [Int])], [Int])
  --     result = un(handle_ hState' (handle hChoose' (handle hErr par2))[0:: Int])
  -- let result :: [((Either String ([Int], [Int]), [Bool]), [Int])]
  --     result = unToND(handleToND hChooseToND (handle_ hState' (handle_ hState' (handle hErr par3)[True:: Bool])[0::Int]))
  -- let result :: [((Either String ([Int], [Int]), [Bool]), [Int])]
  --     result = un(handle hChoose' (handle_ hState' (handle_ hState' (handle hErr par3)[True:: Bool])[0::Int]))
  -- let result :: ([(Either String ([Int], [Int]), [Bool])], [Int])
  --     result = un(handle_ hState' (handle hChoose' (handle_ hState' (handle hErr par3)[True:: Bool]))[0::Int])
  -- let result :: (([Either String ([Int], [Int])], [Bool]), [Int])
  --     result = un(handle_ hState' (handle_ hState' (handle hChoose' (handle hErr par3))[True:: Bool])[0::Int])
  -- let result :: [(Either String ([Int],[Int]), [Int])]
  --     result = unToND(handleToND hChooseToND (handle_ hState' (handle hErr par2)[0:: Int]))
  -- let result :: [Bool]
  --     result = unToND(handleToND hChooseToND (handle hErr pureChooseProg))
  -- let result :: (Either String [Bool], [Bool])
  --     result = un (handle hChoose (handle_ hState' (handle hErr stateb)[True]))
  -- let result :: (Either String [Int], [Int])
  --     result = un (handle hChoose (handle_ hState' (handle hErr useplus)[0]))
  -- let result :: [(Either String (Int, Int), [Int])]
  --     result = un (handle hChoose' (handle_ hState' (handle hErr par1)[0]))
  -- let result :: Maybe (Either String (Int, Int), [Int])
  --     result = un (handle hChooseTrue (handle_ hState' (handle hErr par1)[0]))
  --
  --
  -- let result :: [((Int, Int), [Int])]
      -- result = un(handle hChoose' (handle_ hState' par5 [0:: Int]))
  -- let result :: [((Int, Bool), [Int])]
  --     result = nub $ un(handle hChoose' (handle_ hState' partrans [0:: Int]))
  -- let result :: ([Either String ([Int], [Int])], [Int])
  --     result = un(handle_ hState' (handle hChoose' (handle hErr par5))[0:: Int])
  -- let result :: (Either String [(Int, Int)], [Int])
  --     result = un(handle_ hState' (handle hErr (handle hChoose' par5))[0:: Int])
  -- let result :: [Either String Bool]
  --     result = un(handle hChoose' (handle hErr twitch))
  --
  -- let result2 :: Either String [Bool]
  --     result2 = un(handle hErr (handle hChoose' twitch))
  --
  -- let result :: [(Bool, [Int])]
  --     result = un(handle hChoose' (handle_ hState' useZero[0:: Int]))
  -- let result :: [(Int, Int)]
  --     result = un(handle hChoose' par6)
  --
  -- let result :: (Either String Int, [Int])
  --     result = un(handle_ hState' (handle hErr makeState) [0:: Int])
  -- let result :: [(Either String Int, [Bool])]
  --     result = un(handle hChooseLog (handle hErr progr1))
  -- let result :: (Either String (Either String Int))
  --     result = un(handle_ hChooseOrder (handle hErr progr1) [True,False,False])
  -- let result :: [(Int, Int)]
  --     result = un(handle hChoose' newpar)

  -- let result :: [Either String Int]
  --     result = un(handle hChoose' (handle hErr choice_and_error))

  -- let result :: [(Either String (Int, Int), [Int])]
  --     result = un(handle hChoose' (handle_ hState' (handle hErr (handle hLock parWithLock3)) [0:: Int]))
  -- let result2 :: [(Either String (Int, Int), [Int])]
  --     result2 = un(handle hChoose' (handle_ hState' (handle hErr (handle hLock parWithoutLock)) [0:: Int]))

  -- let result :: [((Int, Int), [Int])]
  --     result = un(handle hChoose' (handle_ hState' (handle hLock tst) [0:: Int]))

  -- let result :: [((Either String (Int, Bool), [Int]), [Bool])]
  --     result = un(handle hChooseLog (handle_ hState' (handle hErr (handle hLock pairing)) [0::Int]))
  -- let result :: [((Either String (Bool, Bool), [Int]), [Bool])]
  --     result = un(handle hChooseLog (handle_ hState' (handle hErr (handle hLock pairing2)) [0::Int]))

  let result :: [(((Int, Bool), Bool), [Int])]
      result = un(handle hChoose' (handle_ hState' (handle hLock triplepairing) [0::Int]))
  -- let result2 :: [((Int, (Bool, Bool)), [Int])]
  --     result2 = un(handle hChoose' (handle_ hState' (handle hLock triplepairing2) [0::Int]))
  -- let result :: [(Either String (Int, Int), [Int])]
  --     result = un(handle hChoose' (handle_ hState' (handle hLock(handle hErr (handle hEndOfProgram something))) [0:: Int]))
  -- let result :: [(Either String (Int, Bool), [Int])]
  --     result = un(handle hChoose' (handle_ hState' (handle hLock(handle hErr (handle hEndOfProgram parnew))) [0:: Int]))
  
  
  -- let result :: [Int]
  --     result = un(handle hChoose' (pickNumber))

  -- Print the value of result
  -- print result
  -- print "par with lock"
  printElementCounts result
  print "length"
  print $ length result
  print "different elements"
  print $ length $ nub result

  -- printElementCounts result2
  -- print "length"
  -- print $ length result2
  -- print "different elements"
  -- print $ length $ nub result2
  --
  -- let diff = symmetricDifference result (map (\(x,t) -> (assoc x,t)) result2)
  -- printElementCounts diff
  -- print "length"
  -- print $ length diff
  
  
  -- print result
  -- print "par without lock"
  -- printElementCounts result2
  -- print "length"
  -- print $ length result2

  -- -- Then, you can check if the result is as expected
  -- if result == (Left "foo", [2, 0])
  --   then putStrLn "Assertion passed: the result is as expected."
  --   else putStrLn "Assertion failed: the result is not as expected."
