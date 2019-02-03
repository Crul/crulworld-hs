module Simulation (runSimulation) where

beginningOfTime = 1
endOfTime       = 9
agentsSpeed     = 1.5  -- TODO make speed work properly (now it applies to x,y not to the real move)

type Time = Int

data WorldData = WorldData { time :: Int, agents :: [AgentData] } deriving (Show)
data AgentType = Prey | Hunter | Food deriving (Eq, Show)
data AgentData = AgentData {
  agentType :: AgentType,
  agentId   :: Int,
  xAgPos    :: Float,
  yAgPos    :: Float
}

isFood :: AgentData -> Bool
isFood a = agentType a == Food

isPrey :: AgentData -> Bool
isPrey a = agentType a == Prey

instance Show AgentData where
  show a = (show $ agentType a)
           ++ "[" ++ (show $ agentId a) ++ "]"
           ++ "(" ++ (show $ xAgPos a) ++ "," ++ (show $ yAgPos a) ++ ")"

data MoveData = MoveData { xMove :: Float, yMove :: Float }

runTimeStep :: WorldData -> WorldData
runTimeStep w = w { time = t, agents = ags }
  where
    t         = succ $ time w
    agsAndMvs = map (runAgentStep w) (agents w)
    ags       = map runMoveAction agsAndMvs

senseAgents :: AgentData -> WorldData -> [AgentData]
senseAgents _ w = agents w  -- TODO filter agents based on agent position and vision-range parameter

runAgentStep :: WorldData -> AgentData -> (AgentData, MoveData)
runAgentStep w a@(AgentData Prey   _ _ _) = runTowardsType isFood w a
runAgentStep w a@(AgentData Hunter _ _ _) = runTowardsType isPrey w a
runAgentStep w a@(AgentData Food   _ _ _) = (a, MoveData 0 0)  -- TODO Food shouldn't do actions

runTowardsType :: (AgentData -> Bool) -> WorldData -> AgentData -> (AgentData, MoveData)
runTowardsType tf w a = ( nxtA, mv )
  where
    ags  = senseAgents a w
    trgt = head $ filter tf ags
    mvTw = moveTowardsAg a trgt
    mv   = MoveData (fst mvTw) (snd mvTw)
    nxtA = a

moveTowardsAg :: AgentData -> AgentData -> (Float, Float)
moveTowardsAg ag trgt = ( xMv, yMv )
  where
    moveAgTowardsAg = moveTowardsAgComp ag trgt
    xMv = moveAgTowardsAg xAgPos
    yMv = moveAgTowardsAg yAgPos

moveTowardsAgComp :: AgentData -> AgentData -> (AgentData -> Float) -> Float
moveTowardsAgComp ag trgt agPFn =
  if (abs dst) < agentsSpeed
    then dst
    else agentsSpeed * (dst / abs dst)
  where dst = (agPFn trgt) - (agPFn ag)

runMoveAction :: (AgentData, MoveData) -> AgentData
runMoveAction (a, m) = a {
    xAgPos = (xAgPos a) + (xMove m),
    yAgPos = (yAgPos a) + (yMove m)
  }

initialWorld :: WorldData
initialWorld = WorldData beginningOfTime
  [
    (AgentData Food   1 8 8),
    (AgentData Prey   1 0 0),
    (AgentData Hunter 1 4 0)
  ]

timeline :: [WorldData]
timeline = iterate runTimeStep initialWorld

runSimulationUpTo :: Time -> [WorldData]
runSimulationUpTo i = takeWhile (\x -> time x <= i) timeline

runSimulation :: IO ()
runSimulation = mapM_ print (runSimulationUpTo 10)
