module ActionsMove (moveAgent) where

import Geometry (Movement, applyMovement)
import Agents (Agent(..))
import Components (Component(..))
import Actions (Action(..))

moveAgent :: [Agent] -> Action -> [Agent]
moveAgent ags (Move ag mv) = map (moveIfAgent ag mv) ags

moveIfAgent :: Agent -> Movement -> Agent -> Agent
moveIfAgent agToMove mv candidateAg =   -- is there a more efficient way ?
  if (agentId agToMove == agentId candidateAg)  -- TODO agToMove == candidateAg
  then moveAgentComponent candidateAg mv
  else candidateAg

moveAgentComponent :: Agent -> Movement -> Agent
moveAgentComponent ag mv = ag { components = nxtComponents }
  where nxtComponents = map (movePositioned ag mv) (components ag)
  
movePositioned :: Agent -> Movement -> Component -> Component
movePositioned ag mv (Positioned p) = Positioned $ applyMovement p mv
movePositioned _ _ c = c
