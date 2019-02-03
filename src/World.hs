module World (World (..), Time, senseAgents) where

import Agents as Ags (Agent)

type Time = Int

data World = World { time :: Int, agents :: [Ags.Agent] } deriving (Show)

senseAgents :: World -> Ags.Agent -> [Ags.Agent]
senseAgents w _ = agents w  -- TODO filter agents based on agent position and vision-range parameter
