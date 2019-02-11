module ActionsEat (eatAgent) where

import Agents (Agent)
import Components (Component(..))
import Actions (Action(..))

eatAgent :: [Agent] -> Action -> [Agent]
eatAgent ags (Eat hunter prey) = filter (notIsPrey prey) $ map (eatIfHunter hunter prey) ags

notIsPrey :: Agent -> Agent -> Bool  -- TODO is there a better way?
notIsPrey prey candidateAg = prey /= candidateAg

eatIfHunter :: Agent -> Agent -> Agent -> Agent  -- TODO is there a better way?
eatIfHunter hunter prey candidateAg =  -- TODO DRY ActionsMove
  if hunter == candidateAg
  then eat hunter prey
  else candidateAg

eat :: Agent -> Agent -> Agent
eat hunter prey = hunter  -- TODO decrease hunger based on (?) prey size
