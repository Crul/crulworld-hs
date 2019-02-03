module Simulation (runSimulation) where

import Geometry        as Geo (Position (..), Vector (..))
import Agents          as Ags (Agent (..),  AgentType(..))
import World           as Wrl (World (..), Time)
import WorldSimulation as WSm (updateWorld)

beginningOfTime = 1
endOfTime       = 9
initialAgents = [
    (Ags.Agent Ags.Food   1 (Geo.Position (Geo.Vector 8 8))),
    (Ags.Agent Ags.Prey   2 (Geo.Position (Geo.Vector 0 0))),
    (Ags.Agent Ags.Hunter 3 (Geo.Position (Geo.Vector 4 0)))
  ]

initialWorld :: Wrl.World
initialWorld = Wrl.World beginningOfTime initialAgents

timeline :: [Wrl.World]
timeline = iterate WSm.updateWorld initialWorld

updateTimeLineUpTo :: Wrl.Time -> [Wrl.World]
updateTimeLineUpTo i = takeWhile (\x -> time x <= i) timeline

runSimulation :: IO ()
runSimulation = mapM_ print (updateTimeLineUpTo endOfTime)
