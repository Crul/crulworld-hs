module ActionsMove (moveAgent) where

import Agents (Agent(..))
import Components (Component(..))
import Geometry (Movement, applyMovement)

moveAgent :: [Agent] -> Agent -> Movement -> [Agent]
moveAgent ags ag mv = map (moveIfAgent ag mv) ags

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
