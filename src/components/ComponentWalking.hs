module ComponentWalking (getWalkingActions) where

import Geometry      (Position, getMovTowardsPos)
import Agents        (Agent)
import Components    (Component(..))
import ComponentsFns (getPositioned, getWalking)
import Actions       (Action(..))

getWalkingActions :: Agent -> Position -> [Action]
getWalkingActions ag trgtPos = [Move ag mvmt]
  where 
    (Positioned agPos) = getPositioned ag
    (Walking speed)     = getWalking ag
    mvmt               = getMovTowardsPos agPos trgtPos speed
