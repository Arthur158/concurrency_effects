module Test () where

import Lib
import State
import Control.Exception (assert)
import Programs
import End
import Conc
import Data.Typeable
import Control.Eff.Choose
import NonDet
import Choose

main :: IO ()
main = do
  -- let result :: (Either String [Int], [Int])
  --     result = un ((handle_ hState' (handle hErr incerr')[0]))
  -- let result :: (Either String [Int], [Int])
  --     result = un(handle hChooseTrue (handle_ hState' (handle hErr ancerr')[0:: Int]))
  let result :: [(Either String [Int], [Int])]
      result = unToND(handleToND hChooseToND (handle_ hState' (handle hErr ancerrDouble)[0:: Int]))
  -- let result :: [Bool]
  --     result = unToND(handleToND hChooseToND (handle hErr pureChooseProg))
  -- let result :: (Either String [Bool], [Bool])
  --     result = un (handle hChoose (handle_ hState' (handle hErr stateb)[True]))
  -- let result :: (Either String [Int], [Int])
  --     result = un (handle hChoose (handle_ hState' (handle hErr useplus)[0]))

  -- Print the value of result
  print result

