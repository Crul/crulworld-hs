module Simulation (runSimulation) where

import AgentsFactory   (makeFood, makePrey, makeHunter)
import World           (World (..), Time)
import WorldSimulation (updateWorld)

beginningOfTime = 0
historySteps    = 9
initialAgents = [
    makeFood   1 8 8,
    makePrey   2 0 0,
    makeHunter 3 4 0
  ]

initialWorld :: World
initialWorld = World beginningOfTime initialAgents

timeline :: [World]
timeline = iterate updateWorld initialWorld

updateTimeLineUpTo :: Time -> [World]
updateTimeLineUpTo i = take i timeline

runSimulation :: IO ()
runSimulation = mapM_ print (updateTimeLineUpTo historySteps)
