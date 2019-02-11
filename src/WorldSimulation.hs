module WorldSimulation (updateWorld) where

import Agents   (Agent, Action, getActions, executeAction)
import World    (World (..), senseAgents)

updateWorld :: World -> World
updateWorld w = w { time = nxtT, agents = nxtAgs }
  where
    nxtT   = succ $ time w
    nxtAgs = updateAgents $ agents w

updateAgents :: [Agent] -> [Agent]
updateAgents ags = updateAgentByIdx ags (length ags)  -- is there a better way?

updateAgentByIdx :: [Agent] -> Int -> [Agent]
updateAgentByIdx ags 0 = ags
updateAgentByIdx ags i = updateAgentByIdx nxtAgs nxtI
  where
    nxtI   = i - 1
    nxtAgs = updateAgent ags (ags!!nxtI)
  
updateAgent :: [Agent] -> Agent -> [Agent]
updateAgent ags ag = executeActions ags $ getAgentActions ags ag
    
getAgentActions :: [Agent] -> Agent -> [Action]
getAgentActions ags ag = getActions ag $ senseAgents ags ag

executeActions :: [Agent] -> [Action] -> [Agent]
executeActions ags [] = ags
executeActions ags (ac:acs) = executeActions (executeAction ags ac) acs
