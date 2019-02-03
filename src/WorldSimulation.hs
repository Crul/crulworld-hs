module WorldSimulation (updateWorld) where

import Geometry (Movement)
import Agents   (Agent, moveAgent, updateAgent)
import World    (World (..), senseAgents)

updateWorld :: World -> World
updateWorld w = w { time = newT, agents = ags }
  where
    newT      = succ $ time w
    agsAndMvs = map (updateWorldAgent w) (agents w)
    ags       = map moveWorldAgent agsAndMvs

updateWorldAgent :: World -> Agent -> (Agent, Movement)
updateWorldAgent w ag = updateAgent visibleAgs ag
  where visibleAgs = senseAgents w ag

moveWorldAgent :: (Agent, Movement) -> Agent
moveWorldAgent (ag, mv) = moveAgent ag mv
