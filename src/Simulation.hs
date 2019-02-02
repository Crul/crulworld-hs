{-# LANGUAGE ExtendedDefaultRules #-}
module Simulation (runSimulation) where

import Conduit
import Data.List

beginningOfTime = 1
endOfTime       = 9
agentsSpeed     = 1.5  -- TODO make speed work properly (now it applies to x,y not to the real move)

data WorldData  = WorldData  { time     :: Int,   agents  :: [AgentData], objects :: [ObjectData] } deriving (Show)
data ObjectData = ObjectData { objectId :: Int,   xObjPos :: Float, yObjPos :: Float } deriving (Show)
data AgentData  = AgentData  { agentId  :: Int,   xAgPos  :: Float, yAgPos  :: Float } deriving (Show)
data MoveData   = MoveData   { xMove    :: Float, yMove   :: Float }

runTimeSteps :: WorldData -> [WorldData]
runTimeSteps w@(WorldData t _ _) | t == endOfTime = [w]
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
  let t         = succ $ time w
  let agsAndMvs = map (runAgentStep w) (agents w)
  let ags       = map runMoveAction agsAndMvs
  w { time = t, agents = ags }

sense :: AgentData -> WorldData -> [ObjectData]
sense _ w = objects w  -- TODO filter objects based on agent position and vision-range parameter

runAgentStep :: WorldData -> AgentData -> (AgentData, MoveData)
runAgentStep w a = do
  let objs = sense a w
  let trgt = head objs
  let mvTw = moveTowards a trgt
  let mv   = MoveData (fst mvTw) (snd mvTw)
  let nxtA = a
  ( nxtA, mv )

moveTowards :: AgentData -> ObjectData -> (Float, Float)
moveTowards ag ob = do
  let moveAgTowardsOb = moveTowardsComp ag ob
  let xMv = moveAgTowardsOb xAgPos xObjPos
  let yMv = moveAgTowardsOb yAgPos yObjPos
  ( xMv, yMv )

moveTowardsComp :: AgentData -> ObjectData -> (AgentData -> Float) -> (ObjectData -> Float) -> Float
moveTowardsComp ag ob agPFn obPFn
  | dst < agentsSpeed = dst
  | otherwise = agentsSpeed * (dst / abs dst)
  where dst = (obPFn ob) - (agPFn ag)

runMoveAction :: (AgentData, MoveData) -> AgentData
runMoveAction agAndMv = do
  let a = fst agAndMv
  let m = snd agAndMv
  a {
    xAgPos = (xAgPos a) + (xMove m),
    yAgPos = (yAgPos a) + (yMove m)
  }

runSimulation :: IO ()
runSimulation = do
  runConduit
    $ yieldMany (
      runTimeSteps (
        WorldData beginningOfTime 
        [AgentData  1 0 0]
        [ObjectData 1 8 8]
      )
    )
    .| mapM_C print
