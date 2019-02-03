module Simulation (runSimulation) where

beginningOfTime = 1
endOfTime       = 9
agentsSpeed     = 1.5  -- TODO make speed work properly (now it applies to x,y not to the real move)

type Time = Int

data World = World { time :: Int, agents :: [Agent] } deriving (Show)
data AgentType = Prey | Hunter | Food deriving (Eq, Show)
data Agent = Agent {
  agentType :: AgentType,
  agentId   :: Int,
  xAgPos    :: Float,
  yAgPos    :: Float
}

isFood :: Agent -> Bool
isFood a = agentType a == Food

isPrey :: Agent -> Bool
isPrey a = agentType a == Prey

instance Show Agent where
  show a = (show $ agentType a)
           ++ "[" ++ (show $ agentId a) ++ "]"
           ++ "(" ++ (show $ xAgPos a) ++ "," ++ (show $ yAgPos a) ++ ")"

data Move = Move { xMove :: Float, yMove :: Float }

updateWorld :: World -> World
updateWorld w = w { time = t, agents = ags }
  where
    t         = succ $ time w
    agsAndMvs = map (updateAgent w) (agents w)
    ags       = map applyMoveAction agsAndMvs

senseAgents :: Agent -> World -> [Agent]
senseAgents _ w = agents w  -- TODO filter agents based on agent position and vision-range parameter

updateAgent :: World -> Agent -> (Agent, Move)
updateAgent w a@(Agent Prey   _ _ _) = updateTowardsType isFood w a
updateAgent w a@(Agent Hunter _ _ _) = updateTowardsType isPrey w a
updateAgent w a@(Agent Food   _ _ _) = (a, Move 0 0)  -- TODO Food shouldn't do actions

updateTowardsType :: (Agent -> Bool) -> World -> Agent -> (Agent, Move)
updateTowardsType tf w a = ( nxtA, mv )
  where
    ags  = senseAgents a w
    trgt = head $ filter tf ags
    mvTw = moveTowardsAg a trgt
    mv   = Move (fst mvTw) (snd mvTw)
    nxtA = a

moveTowardsAg :: Agent -> Agent -> (Float, Float)
moveTowardsAg ag trgt = ( xMv, yMv )
  where
    moveAgTowardsAg = moveTowardsAgComp ag trgt
    xMv = moveAgTowardsAg xAgPos
    yMv = moveAgTowardsAg yAgPos

moveTowardsAgComp :: Agent -> Agent -> (Agent -> Float) -> Float
moveTowardsAgComp ag trgt agPFn =
  if (abs dst) < agentsSpeed
    then dst
    else agentsSpeed * (dst / abs dst)
  where dst = (agPFn trgt) - (agPFn ag)

applyMoveAction :: (Agent, Move) -> Agent
applyMoveAction (a, m) = a {
    xAgPos = (xAgPos a) + (xMove m),
    yAgPos = (yAgPos a) + (yMove m)
  }

initialAgents = [
    (Agent Food   1 8 8),
    (Agent Prey   1 0 0),
    (Agent Hunter 1 4 0)
  ]

initialWorld :: World
initialWorld = World beginningOfTime initialAgents

timeline :: [World]
timeline = iterate updateWorld initialWorld

updateTimeLineUpTo :: Time -> [World]
updateTimeLineUpTo i = takeWhile (\x -> time x <= i) timeline

runSimulation :: IO ()
runSimulation = mapM_ print (updateTimeLineUpTo 10)
