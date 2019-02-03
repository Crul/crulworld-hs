module Agents (Agent (..), AgentType (..), updateAgent) where

import Geometry as Geo (Position (..), Movement (..), Vector (..), getPosVector)

agentsSpeed :: Float
agentsSpeed = 1.5  -- TODO make speed work properly (now it applies to x,y not to the real move)

data AgentType = Prey | Hunter | Food deriving (Eq, Show)
data Agent = Agent { agentType :: AgentType, agentId :: Int, agentPos :: Geo.Position }
instance Show Agent where
  show a = (show $ agentType a) ++ "[" ++ (show $ agentId a) ++ "]" ++ (show $ Geo.getPosVector $ agentPos a)

isFood :: Agent -> Bool
isFood a = agentType a == Food

isPrey :: Agent -> Bool
isPrey a = agentType a == Prey

updateAgent :: [Agent] -> Agent -> (Agent, Geo.Movement)
updateAgent ags a@(Agent Prey   _ _) = updateTowardsType isFood ags a
updateAgent ags a@(Agent Hunter _ _) = updateTowardsType isPrey ags a
updateAgent ags a@(Agent Food   _ _) = (a, Geo.Movement (Geo.Vector 0 0))  -- TODO Food shouldn't do actions

updateTowardsType :: (Agent -> Bool) -> [Agent] -> Agent -> (Agent, Geo.Movement)
updateTowardsType tf ags a = ( nxtA, mv )
  where
    trgt = head $ filter tf ags
    mv   = moveTowardsAg a trgt
    nxtA = a

moveTowardsAg :: Agent -> Agent -> Geo.Movement
moveTowardsAg ag trgt = Geo.Movement $ Geo.Vector xMv yMv
  where
    moveAgTowardsAg = moveTowardsAgComp ag trgt
    xMv = moveAgTowardsAg (x . Geo.getPosVector . agentPos)
    yMv = moveAgTowardsAg (y . Geo.getPosVector . agentPos)

moveTowardsAgComp :: Agent -> Agent -> (Agent -> Float) -> Float
moveTowardsAgComp ag trgt agPFn =
  if (abs dst) < agentsSpeed
    then dst
    else agentsSpeed * (dst / abs dst)
  where dst = (agPFn trgt) - (agPFn ag)
