{-# LANGUAGE ExtendedDefaultRules #-}
module Lib (someFunc) where

import Conduit
import Data.List

beginningOfTime = 1
endOfTime = 10

data WorldData = WorldData { _time :: Int } deriving (Show)
time :: WorldData -> Int
time (WorldData t) = t

runTimeStep :: WorldData -> WorldData
runTimeStep w = w { _time = succ $ time w }

  -- RIGHT !!!
runTimeSteps :: WorldData -> [WorldData]
runTimeSteps w@(WorldData t) | t == endOfTime = [w]
runTimeSteps w = w : runTimeSteps (runTimeStep w)
  -- WRONG !!!
  -- runTimeSteps :: WorldData -> [WorldData] -> [WorldData]
  -- runTimeSteps w@(WorldData t) xs | t == endOfTime = w:xs
  -- runTimeSteps w xs = runTimeSteps (runTimeStep w) (w:xs)
  -- WRONG !!!
  -- runTimeSteps :: WorldData -> [WorldData] -> [WorldData]
  -- runTimeSteps w@(WorldData t) xs | t == endOfTime = xs ++ [w]
  -- runTimeSteps w xs = runTimeSteps (runTimeStep w) (xs ++ [w])

someFunc :: IO ()
someFunc = do
  runConduit
    $ yieldMany (runTimeSteps (WorldData beginningOfTime))
    .| mapM_C print
