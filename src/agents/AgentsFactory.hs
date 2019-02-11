module AgentsFactory (makeFoodAgent, makePreyAgent, makeHunterAgent) where

import AgentTypes        (AgentType(..))
import Agents            (Agent(..))
import Components        (Speed)
import ComponentsFactory (makePositioned, makeWalking, makeHunting)

agentsSpeed :: Speed
agentsSpeed = 1.5

makeFoodAgent :: Int -> Float -> Float -> Agent
makeFoodAgent id x y = Agent Food id cs
  where cs = [ makePositioned x y ]

makePreyAgent :: Int -> Float -> Float -> Agent
makePreyAgent id x y = Agent Prey id cs
  where cs = [ makePositioned x y
             , makeWalking agentsSpeed
             , makeHunting Food
             ]

makeHunterAgent :: Int -> Float -> Float -> Agent
makeHunterAgent id x y = Agent Hunter id cs
  where cs = [ makePositioned x y
             , makeWalking agentsSpeed
             , makeHunting Prey
             ]
