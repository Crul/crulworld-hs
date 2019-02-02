{-# LANGUAGE ExtendedDefaultRules #-}
module Simulation (runSimulation) where

import Conduit
import Data.List

beginningOfTime = 1
endOfTime = 10

data WorldData = WorldData { time :: Int, agents :: [AgentData] } deriving (Show)
data AgentData = AgentData { agentId :: Int, counter :: Int, xPos :: Int, yPos :: Int } deriving (Show)
data MoveActionData = MoveActionData { xMove :: Int, yMove :: Int } deriving (Show)

runTimeSteps :: WorldData -> [WorldData]
runTimeSteps w@(WorldData t _) | t == endOfTime = [w]
runTimeSteps w = w : runTimeSteps (runTimeStep w)
{-  WRONG !!!
    runTimeSteps :: WorldData -> [WorldData] -> [WorldData]
    runTimeSteps w@(WorldData t) xs | t == endOfTime = w:xs
    runTimeSteps w xs = runTimeSteps (runTimeStep w) (w:xs)
    WRONG !!!
    runTimeSteps :: WorldData -> [WorldData] -> [WorldData]
    runTimeSteps w@(WorldData t) xs | t == endOfTime = xs ++ [w]
    runTimeSteps w xs = runTimeSteps (runTimeStep w) (xs ++ [w])
-}

runTimeStep :: WorldData -> WorldData
runTimeStep w = do
  let t = succ $ time w
  let agsAndMvs = map runAgentStep (agents w)
  let ags = map runMoveAction agsAndMvs
  w { time = t, agents = ags }

runAgentStep :: AgentData -> (AgentData, MoveActionData)
runAgentStep a = do
  let nextA = a { counter = succ $ counter a }
  let m = MoveActionData 1 1
  ( nextA, m )

runMoveAction :: (AgentData, MoveActionData) -> AgentData
runMoveAction agAndMv = do
  let a = fst agAndMv
  let m = snd agAndMv
  a {
    xPos = (xPos a) + (xMove m),
    yPos = (yPos a) + (yMove m)
  }

runSimulation :: IO ()
runSimulation = do
  runConduit
    $ yieldMany (
      runTimeSteps (
        WorldData beginningOfTime 
        [AgentData 0 beginningOfTime 0 0]
      )
    )
    .| mapM_C print
