module AgentsFactory (makeFoodAgent, makePreyAgent, makeHunterAgent) where

import AgentTypes (AgentType(..))
import Agents (Agent(..))
import Components (Speed)
import ComponentsFactory (makePositioned, makeWalker, makeHunter)

agentsSpeed :: Speed
agentsSpeed = 1.5

makeFoodAgent :: Int -> Float -> Float -> Agent
makeFoodAgent id x y = Agent Food id [makePositioned x y]

makePreyAgent :: Int -> Float -> Float -> Agent
makePreyAgent id x y = Agent Prey id [makePositioned x y, makeWalker agentsSpeed, makeHunter Food]

makeHunterAgent :: Int -> Float -> Float -> Agent
makeHunterAgent id x y = Agent Hunter id [makePositioned x y, makeWalker agentsSpeed, makeHunter Prey]
