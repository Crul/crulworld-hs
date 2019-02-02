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

senseObjs :: AgentData -> WorldData -> [ObjectData]
senseObjs _ w = objects w  -- TODO filter objects based on agent position and vision-range parameter

senseAgents :: AgentData -> WorldData -> [AgentData]
senseAgents _ w = agents w  -- TODO filter agents based on agent position and vision-range parameter

runAgentStep :: WorldData -> AgentData -> (AgentData, MoveData)
runAgentStep w a@(AgentData t _ _ _)
  | t == Prey   = runPreyStep w a
  | t == Hunter = runHunterStep w a

runPreyStep :: WorldData -> AgentData -> (AgentData, MoveData)
runPreyStep w a = do
    let objs = senseObjs a w
    let trgt = head objs
    let mvTw = moveTowardsObj a trgt
    let mv   = MoveData (fst mvTw) (snd mvTw)
    let nxtA = a
    ( nxtA, mv )

runHunterStep :: WorldData -> AgentData -> (AgentData, MoveData)
runHunterStep w a = do
    let ags  = senseAgents a w
    let trgt = head ags
    let mvTw = moveTowardsAg a trgt
    let mv   = MoveData (fst mvTw) (snd mvTw)
    let nxtA = a
    ( nxtA, mv )

moveTowardsObj :: AgentData -> ObjectData -> (Float, Float)
moveTowardsObj ag ob = do
  let moveAgTowardsOb = moveTowardsObjComp ag ob
  let xMv = moveAgTowardsOb xAgPos xObjPos
  let yMv = moveAgTowardsOb yAgPos yObjPos
  ( xMv, yMv )

moveTowardsObjComp :: AgentData -> ObjectData -> (AgentData -> Float) -> (ObjectData -> Float) -> Float
moveTowardsObjComp ag ob agPFn obPFn
  | (abs dst) < agentsSpeed = dst
  | otherwise         = agentsSpeed * (dst / abs dst)
  where dst = (obPFn ob) - (agPFn ag)

moveTowardsAg :: AgentData -> AgentData -> (Float, Float)
moveTowardsAg ag trgt = do
  let moveAgTowardsAg = moveTowardsAgComp ag trgt
  let xMv = moveAgTowardsAg xAgPos
  let yMv = moveAgTowardsAg yAgPos
  ( xMv, yMv )

moveTowardsAgComp :: AgentData -> AgentData -> (AgentData -> Float) -> Float
moveTowardsAgComp ag trgt agPFn
  | (abs dst) < agentsSpeed = dst
  | otherwise         = agentsSpeed * (dst / abs dst)
  where dst = (agPFn trgt) - (agPFn ag)

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
        [
          (AgentData Prey   1 0 0),
          (AgentData Hunter 1 4 0)
        ]
        [ObjectData 1 8 8]
      )
    )
    .| mapM_C print
