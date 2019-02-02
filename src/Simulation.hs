{-# LANGUAGE ExtendedDefaultRules #-}
module Simulation (runSimulation) where

import Conduit
import Data.List

beginningOfTime = 1
endOfTime = 10

data WorldData = WorldData { _time :: Int, _agents :: [AgentData] } deriving (Show)
getTime   (WorldData t _) = t
getAgents (WorldData _ a) = a

data AgentData = AgentData { _agentId :: Int, _counter :: Int, _xPos :: Int, _yPos :: Int } deriving (Show)
getCounter (AgentData _ c _ _) = c
getXPos    (AgentData _ _ x _) = x
getYPos    (AgentData _ _ _ y) = y

data MoveActionData = MoveActionData { _xMove :: Int, _yMove :: Int } deriving (Show)
getXMove (MoveActionData x _) = x
getYMove (MoveActionData _ y) = y

runAgentStep :: AgentData -> (AgentData, MoveActionData)
runAgentStep a = do
  let nextA = a { _counter = succ $ getCounter a }
  let m = MoveActionData 1 1
  ( nextA, m )

runMoveAction :: (AgentData, MoveActionData) -> AgentData
runMoveAction agAndMv = do
  let a = fst agAndMv
  let m = snd agAndMv
  a {
    _xPos = (getXPos a) + (getXMove m),
    _yPos = (getYPos a) + (getYMove m)
  }

runTimeStep :: WorldData -> WorldData
runTimeStep w = do
  let t = succ $ getTime w
  let agsAndMvs = map runAgentStep (getAgents w)
  let ags = map runMoveAction agsAndMvs
  w { _time = t, _agents = ags }

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
