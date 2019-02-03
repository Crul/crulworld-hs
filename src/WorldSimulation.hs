module WorldSimulation (updateWorld) where

import Geometry as Geo (Position (..), Movement, getPosVector, getMovVector, vecAdd)
import Agents  as Ags (Agent (..), updateAgent)
import World   as Wrl (World (..), senseAgents)

updateWorld :: World -> World
updateWorld w = w { time = t, agents = ags }
  where
    t         = succ $ time w
    agsAndMvs = map (updateAgentInWorld w) (agents w)
    ags       = map applyMovement agsAndMvs

updateAgentInWorld :: World -> Agent -> (Agent, Movement)
updateAgentInWorld w ag = Ags.updateAgent visibleAgs ag
  where visibleAgs = Wrl.senseAgents w ag

applyMovement :: (Ags.Agent, Geo.Movement) -> Ags.Agent
applyMovement (ag, mv) = ag { agentPos = newPos }
  where
    agPos   = Geo.getPosVector $ agentPos ag
    trgtPos = Geo.getMovVector mv
    newPos  = Geo.Position $ agPos `Geo.vecAdd` trgtPos
