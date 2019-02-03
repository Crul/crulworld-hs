module World (World (..), Time, senseAgents) where

import Agents (Agent)

type Time = Int
data World = World { time :: Int, agents :: [Agent] }
instance Show World where show w = (show $ time w) ++ ": " ++ (show $ agents w)

senseAgents :: World -> Agent -> [Agent]
senseAgents w _ = agents w  -- TODO filter agents based on agent position and vision-range parameter
