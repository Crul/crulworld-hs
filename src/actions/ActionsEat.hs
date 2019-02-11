module ActionsEat (eatAgent) where

import Agents (Agent(..))
import Components (Component(..))
import Actions (Action(..))

eatAgent :: [Agent] -> Action -> [Agent]
eatAgent ags (Eat hunter prey) = filter (notIsPrey prey) $ map (eatIfHunter hunter prey) ags

notIsPrey :: Agent -> Agent -> Bool  -- is there a more efficien
notIsPrey prey candidateAg = (agentId prey /= agentId candidateAg)  -- TODO prey == candidateAg

eatIfHunter :: Agent -> Agent -> Agent -> Agent  -- is there a more efficient way ?
eatIfHunter hunter prey candidateAg =
  if (agentId hunter == agentId candidateAg)  -- TODO prey == candidateAg
  then eat hunter prey
  else candidateAg

eat :: Agent -> Agent -> Agent
eat hunter prey = hunter  -- TODO decrease hunger based on (?) prey size
