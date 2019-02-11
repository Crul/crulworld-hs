module AgentsFactory (makeFood, makePrey, makeHunter) where

import AgentTypes (AgentType(..))
import Agents (Agent(..))
import Components (Speed)
import ComponentsFactory (makePositioned, makeWalker, makeChaser)

agentsSpeed :: Speed
agentsSpeed = 1.5

makeFood :: Int -> Float -> Float -> Agent
makeFood id x y = Agent Food id [makePositioned x y]

makePrey :: Int -> Float -> Float -> Agent
makePrey id x y = Agent Prey id [makePositioned x y, makeWalker agentsSpeed, makeChaser Food]

makeHunter :: Int -> Float -> Float -> Agent
makeHunter id x y = Agent Hunter id [makePositioned x y, makeWalker agentsSpeed, makeChaser Prey]
