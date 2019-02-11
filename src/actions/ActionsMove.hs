module ActionsMove (moveAgent) where

import Geometry          (Vector(..), Position(..), Movement, applyMovement)
import Agents            (Agent, getComponents, setComponents)
import Components        (Component(..))
import ComponentsFactory (makePositioned)
import Actions           (Action(..))

moveAgent :: [Agent] -> Action -> [Agent]
moveAgent ags (Move ag mv) = map (moveIfAgent ag mv) ags

moveIfAgent :: Agent -> Movement -> Agent -> Agent  -- TODO is there a better way?
moveIfAgent agToMove mv candidateAg =  -- TODO DRY ActionsEat
  if agToMove == candidateAg
  then moveAgentComponent candidateAg mv
  else candidateAg

moveAgentComponent :: Agent -> Movement -> Agent
moveAgentComponent ag mv = setComponents ag nxtComponents
  where nxtComponents = map (movePositioned ag mv) (getComponents ag)  -- TODO is there a better way? (it is possible to use ComponentsFns.getPositioned)

movePositioned :: Agent -> Movement -> Component -> Component
movePositioned ag mv (Positioned p) = makePositioned x y
  where  -- TODO this is very weird, importing the ctors (Position, Vector) to extract values and re-pack them through makePositioned, just to force Positioned contruction to be consistent !?
    (Position (Vector x y)) = applyMovement p mv

movePositioned _ _ c                = c
