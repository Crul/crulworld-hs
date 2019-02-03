module Simulation (runSimulation) where

beginningOfTime = 1
endOfTime       = 9
agentsSpeed     = 1.5  -- TODO make speed work properly (now it applies to x,y not to the real move)

data Vector = Vector {x :: Float, y :: Float}
instance Show Vector where
  show v = "(" ++ (show $ x v) ++ "," ++ (show $ y v) ++ ")"
  
vecAdd :: Vector -> Vector -> Vector
vecAdd v1 v2 = Vector ((x v1) + (x v2)) ((y v1) + (y v2))

newtype Position = Position Vector deriving (Show)
getPosVector :: Position -> Vector
getPosVector (Position v) = v

newtype Movement = Movement Vector
getMovVector :: Movement -> Vector
getMovVector (Movement v) = v

type Time = Int

data World = World { time :: Int, agents :: [Agent] } deriving (Show)

data AgentType = Prey | Hunter | Food deriving (Eq, Show)
data Agent = Agent { agentType :: AgentType, agentId :: Int, agentPos :: Position }
instance Show Agent where
  show a = (show $ agentType a) ++ "[" ++ (show $ agentId a) ++ "]" ++ (show $ getPosVector $ agentPos a)

isFood :: Agent -> Bool
isFood a = agentType a == Food

isPrey :: Agent -> Bool
isPrey a = agentType a == Prey

updateWorld :: World -> World
updateWorld w = w { time = t, agents = ags }
  where
    t         = succ $ time w
    agsAndMvs = map (updateAgent w) (agents w)
    ags       = map applyMovement agsAndMvs

senseAgents :: Agent -> World -> [Agent]
senseAgents _ w = agents w  -- TODO filter agents based on agent position and vision-range parameter

updateAgent :: World -> Agent -> (Agent, Movement)
updateAgent w a@(Agent Prey   _ _) = updateTowardsType isFood w a
updateAgent w a@(Agent Hunter _ _) = updateTowardsType isPrey w a
updateAgent w a@(Agent Food   _ _) = (a, Movement (Vector 0 0))  -- TODO Food shouldn't do actions

updateTowardsType :: (Agent -> Bool) -> World -> Agent -> (Agent, Movement)
updateTowardsType tf w a = ( nxtA, mv )
  where
    ags  = senseAgents a w
    trgt = head $ filter tf ags
    mv   = moveTowardsAg a trgt
    nxtA = a

moveTowardsAg :: Agent -> Agent -> Movement
moveTowardsAg ag trgt = Movement $ Vector xMv yMv
  where
    moveAgTowardsAg = moveTowardsAgComp ag trgt
    xMv = moveAgTowardsAg (x . getPosVector . agentPos)
    yMv = moveAgTowardsAg (y . getPosVector . agentPos)

moveTowardsAgComp :: Agent -> Agent -> (Agent -> Float) -> Float
moveTowardsAgComp ag trgt agPFn =
  if (abs dst) < agentsSpeed
    then dst
    else agentsSpeed * (dst / abs dst)
  where dst = (agPFn trgt) - (agPFn ag)

applyMovement :: (Agent, Movement) -> Agent
applyMovement (ag, mv) = ag { agentPos = newPos }
  where
    agPos   = getPosVector $ agentPos ag
    trgtPos = getMovVector mv
    newPos  = Position $ agPos `vecAdd` trgtPos

initialAgents = [
    (Agent Food   1 (Position (Vector 8 8))),
    (Agent Prey   2 (Position (Vector 0 0))),
    (Agent Hunter 3 (Position (Vector 4 0)))
  ]

initialWorld :: World
initialWorld = World beginningOfTime initialAgents

timeline :: [World]
timeline = iterate updateWorld initialWorld

updateTimeLineUpTo :: Time -> [World]
updateTimeLineUpTo i = takeWhile (\x -> time x <= i) timeline

runSimulation :: IO ()
runSimulation = mapM_ print (updateTimeLineUpTo endOfTime)
