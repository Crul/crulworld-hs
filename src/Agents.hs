module Agents (Agent, makeFood, makePrey, makeHunter, moveAgent, updateAgent) where

import Geometry (Position, Movement, makePosition, makeMovement, getMovTowardsPos, applyMovement)

type Speed = Float
agentsSpeed :: Speed
agentsSpeed = 1.5  -- TODO make speed work properly (now it applies to x,y not to the real move)

data AgentType = Prey | Hunter | Food deriving (Eq, Show)
data Agent = Agent { agentType :: AgentType, agentId :: Int, agentPos :: Position }
instance Show Agent where
  show a = (show $ agentType a) ++ "[" ++ (show $ agentId a) ++ "]" ++ (show $ agentPos a)

makeFood :: Int -> Float -> Float -> Agent
makeFood id x y = makeAgent Food id x y

makePrey :: Int -> Float -> Float -> Agent
makePrey id x y = makeAgent Prey id x y

makeHunter :: Int -> Float -> Float -> Agent
makeHunter id x y = makeAgent Hunter id x y

makeAgent :: AgentType -> Int -> Float -> Float -> Agent
makeAgent t id x y = Agent t id (makePosition x y)

isFood :: Agent -> Bool
isFood a = agentType a == Food

isPrey :: Agent -> Bool
isPrey a = agentType a == Prey

moveAgent :: Agent -> Movement -> Agent
moveAgent ag mv = ag { agentPos = newPos }
  where newPos = applyMovement (agentPos ag) mv

updateAgent :: [Agent] -> Agent -> (Agent, Movement)
updateAgent ags a = case agentType a of
  Prey   -> moveTowardsType isFood ags a
  Hunter -> moveTowardsType isPrey ags a
  Food   -> (a, makeMovement 0 0)

moveTowardsType :: (Agent -> Bool) -> [Agent] -> Agent -> (Agent, Movement)
moveTowardsType tf ags a = ( nxtA, mv )
  where
    trgt = head $ filter tf ags
    mv   = moveTowardsAg a trgt
    nxtA = a

moveTowardsAg :: Agent -> Agent -> Movement
moveTowardsAg ag trgt = getMovTowardsPos agPos trgtPos agentsSpeed
  where
    agPos   = agentPos ag
    trgtPos = agentPos trgt
