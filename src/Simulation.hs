{-# LANGUAGE ExtendedDefaultRules #-}
module Simulation (runSimulation) where

import Conduit
import Data.List

beginningOfTime = 1
endOfTime       = 9
agentsSpeed     = 1.5  -- TODO make speed work properly (now it applies to x,y not to the real move)

data WorldData = WorldData {
  time    :: Int,
  agents  :: [AgentData],
  objects :: [ObjectData]
} deriving (Show)

data ObjectData = ObjectData {
  objectId :: Int,
  xObjPos  :: Float,
  yObjPos  :: Float
}

instance Show ObjectData where
  show o = "Obj[" ++ (show $ objectId o) ++ "]"
           ++ "(" ++ (show $ xObjPos o) ++ "," ++ (show $ yObjPos o) ++ ")"

data AgentType  = Prey | Hunter deriving (Eq, Show)
data AgentData = AgentData {
  agentType :: AgentType,
  agentId   :: Int,
  xAgPos    :: Float,
  yAgPos    :: Float
}

instance Show AgentData where
  show a = (show $ agentType a)
           ++ "[" ++ (show $ agentId a) ++ "]"
           ++ "(" ++ (show $ xAgPos a) ++ "," ++ (show $ yAgPos a) ++ ")"

data MoveData = MoveData {
  xMove :: Float,
  yMove :: Float
}

runTimeSteps :: WorldData -> [WorldData]
runTimeSteps w
  | time w == endOfTime = [w]
  | otherwise           = w : runTimeSteps (runTimeStep w)
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
runTimeStep w = w { time = t, agents = ags }
  where
    t         = succ $ time w
    agsAndMvs = map (runAgentStep w) (agents w)
    ags       = map runMoveAction agsAndMvs
  

senseObjs :: AgentData -> WorldData -> [ObjectData]
senseObjs _ w = objects w  -- TODO filter objects based on agent position and vision-range parameter

senseAgents :: AgentData -> WorldData -> [AgentData]
senseAgents _ w = agents w  -- TODO filter agents based on agent position and vision-range parameter

runAgentStep :: WorldData -> AgentData -> (AgentData, MoveData)
runAgentStep w a@(AgentData Prey   _ _ _) = runPreyStep   w a
runAgentStep w a@(AgentData Hunter _ _ _) = runHunterStep w a

runPreyStep :: WorldData -> AgentData -> (AgentData, MoveData)
runPreyStep w a = ( nxtA, mv )
  where
    objs = senseObjs a w
    trgt = head objs
    mvTw = moveTowardsObj a trgt
    mv   = MoveData (fst mvTw) (snd mvTw)
    nxtA = a
    

runHunterStep :: WorldData -> AgentData -> (AgentData, MoveData)
runHunterStep w a = ( nxtA, mv )
  where
    ags  = senseAgents a w
    trgt = head ags
    mvTw = moveTowardsAg a trgt
    mv   = MoveData (fst mvTw) (snd mvTw)
    nxtA = a
    

moveTowardsObj :: AgentData -> ObjectData -> (Float, Float)
moveTowardsObj ag ob = ( xMv, yMv )
  where
    moveAgTowardsOb = moveTowardsObjComp ag ob
    xMv = moveAgTowardsOb xAgPos xObjPos
    yMv = moveAgTowardsOb yAgPos yObjPos
  

moveTowardsObjComp :: AgentData -> ObjectData -> (AgentData -> Float) -> (ObjectData -> Float) -> Float
moveTowardsObjComp ag ob agPFn obPFn
  | (abs dst) < agentsSpeed = dst
  | otherwise         = agentsSpeed * (dst / abs dst)
  where dst = (obPFn ob) - (agPFn ag)

moveTowardsAg :: AgentData -> AgentData -> (Float, Float)
moveTowardsAg ag trgt = ( xMv, yMv )
  where
    moveAgTowardsAg = moveTowardsAgComp ag trgt
    xMv = moveAgTowardsAg xAgPos
    yMv = moveAgTowardsAg yAgPos
  

moveTowardsAgComp :: AgentData -> AgentData -> (AgentData -> Float) -> Float
moveTowardsAgComp ag trgt agPFn
  | (abs dst) < agentsSpeed = dst
  | otherwise         = agentsSpeed * (dst / abs dst)
  where dst = (agPFn trgt) - (agPFn ag)

runMoveAction :: (AgentData, MoveData) -> AgentData
runMoveAction agAndMv = a {
    xAgPos = (xAgPos a) + (xMove m),
    yAgPos = (yAgPos a) + (yMove m)
  }
  where
    a = fst agAndMv
    m = snd agAndMv
  

runSimulation :: IO ()
runSimulation = do
  runConduit
    $ yieldMany (
      runTimeSteps (
        WorldData beginningOfTime
        [
          (AgentData Prey   1 0 0),
          (AgentData Hunter 1 4 0)
        ]
        [ObjectData 1 8 8]
      )
    )
    .| mapM_C print
