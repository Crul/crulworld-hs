module ComponentWalker (getWalkerActions) where

import Geometry (Position, getMovTowardsPos)
import Agents (Agent)
import Components (Component(..))
import ComponentsFns (getPositioned, getWalker)
import Actions (Action(..))

getWalkerActions :: Agent -> Position -> [Action]
getWalkerActions ag trgtPos = [Move ag mvmt]
  where 
    (Positioned agPos) = getPositioned ag
    (Walker speed)     = getWalker ag
    mvmt               = getMovTowardsPos agPos trgtPos speed
