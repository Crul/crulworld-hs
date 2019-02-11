module Simulation (runSimulation) where

import AgentsFactory   (makeFoodAgent, makePreyAgent, makeHunterAgent)
import World           (World (..), Time)
import WorldSimulation (updateWorld)

beginningOfTime = 0
historySteps    = 9
initialAgents = [
    makeFoodAgent   1 9 9,
    makePreyAgent   2 5 5,
    makeHunterAgent 3 0 0
  ]

initialWorld :: World
initialWorld = World beginningOfTime initialAgents

timeline :: [World]
timeline = iterate updateWorld initialWorld

updateTimeLineUpTo :: Time -> [World]
updateTimeLineUpTo i = take i timeline

runSimulation :: IO ()
runSimulation = mapM_ print (updateTimeLineUpTo historySteps)
