module Agents (Agent, Action, makeFood, makePrey, makeHunter, getActions, executeAction) where

import Geometry (Position, Movement, makePosition, makeMovement, getMovTowardsPos, applyMovement)

type Speed = Float
agentsSpeed :: Speed
agentsSpeed = 1.5

data AgentType = Prey | Hunter | Food deriving (Eq, Show)

data Action = Move Agent Movement | FooAction
data Component = Positioned Position | Walker Float | Chaser AgentType -- how to express components' dependencies
instance Show Component where
  show (Positioned p) = show p
  show (Walker sp) = "Walker"
  show (Chaser at) = "Chaser:" ++ (show at)

makePositioned :: Float -> Float -> Component
makePositioned x y = Positioned $ makePosition x y

data Agent = Agent { agentType :: AgentType, agentId :: Int, components :: [Component] }
-- data Agent = Agent { agentId :: Int, components :: [Component] }
instance Show Agent where
  show a = (show $ agentType a) ++ "[" ++ (show $ agentId a) ++ "]" ++ (show $ components a)

makeFood :: Int -> Float -> Float -> Agent
makeFood id x y = Agent Food id [makePositioned x y]

makePrey :: Int -> Float -> Float -> Agent
makePrey id x y = Agent Prey id [makePositioned x y, Walker agentsSpeed, Chaser Food]

makeHunter :: Int -> Float -> Float -> Agent
makeHunter id x y = Agent Hunter id [makePositioned x y, Walker agentsSpeed, Chaser Prey]

isAgentType :: AgentType -> Agent -> Bool
isAgentType t a = agentType a == t

getActions :: Agent -> [Agent] -> [Action]
getActions ag ags = concat $ map (getComponentActions ags ag) (components ag)

getComponentActions :: [Agent] -> Agent -> Component -> [Action]
getComponentActions targets ag (Chaser agType) = [getMoveTowardsFiltered (isAgentType agType) targets ag]
getComponentActions _ _ _ = []

executeAction :: [Agent] -> Action -> [Agent]
executeAction ags (Move ag mv) = map (moveIfAgent ag mv) ags
executeAction ags _ = ags  -- TODO ?

moveIfAgent :: Agent -> Movement -> Agent -> Agent
moveIfAgent agToMove mv candidateAg =   -- is there a more efficient way ?
  if (agentId agToMove == agentId candidateAg)  -- TODO agToMove == candidateAg
  then moveAgent candidateAg mv
  else candidateAg

moveAgent :: Agent -> Movement -> Agent
moveAgent ag mv = ag { components = nxtComponents }
  where nxtComponents = map (movePositioned ag mv) (components ag)

movePositioned :: Agent -> Movement -> Component -> Component
movePositioned ag mv (Positioned p) = Positioned $ applyMovement p mv
movePositioned _ _ c = c

getMoveTowardsFiltered :: (Agent -> Bool) -> [Agent] -> Agent -> Action
getMoveTowardsFiltered tf ags a = mv
  where
    trgt = head $ filter tf ags -- TODO warning head
    mv   = getMoveTowardsAg a trgt

getComponent :: (Component -> Bool) -> Agent -> Component
getComponent ff ag = head $ filter ff (components ag)  -- TODO warning head
    
isPositioned :: Component -> Bool -- is there a better way?
isPositioned (Positioned _) = True
isPositioned _ = False

getPositioned :: Agent -> Component
getPositioned = getComponent isPositioned

getPositionFromPositioned :: Component -> Position
getPositionFromPositioned (Positioned p) = p

getMoveTowardsAg :: Agent -> Agent -> Action
getMoveTowardsAg ag trgt = Move ag mvmt
  where
    agPos   = getPositionFromPositioned $ getPositioned ag
    trgtPos = getPositionFromPositioned $ getPositioned trgt
    mvmt    = getMovTowardsPos agPos trgtPos agentsSpeed
