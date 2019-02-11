module ActionsMove (moveAgent) where

import Geometry (Movement, applyMovement)
import Agents (Agent, getComponents, setComponents)
import Components (Component(..))
import Actions (Action(..))

moveAgent :: [Agent] -> Action -> [Agent]
moveAgent ags (Move ag mv) = map (moveIfAgent ag mv) ags

moveIfAgent :: Agent -> Movement -> Agent -> Agent
moveIfAgent agToMove mv candidateAg =   -- TODO is there a better way?
  if agToMove == candidateAg
  then moveAgentComponent candidateAg mv
  else candidateAg

moveAgentComponent :: Agent -> Movement -> Agent
moveAgentComponent ag mv = setComponents ag nxtComponents
  where nxtComponents = map (movePositioned ag mv) (getComponents ag)
  
movePositioned :: Agent -> Movement -> Component -> Component
movePositioned ag mv (Positioned p) = Positioned $ applyMovement p mv
movePositioned _ _ c = c
