module ComponentHunting (getHuntingActions) where

import AgentTypes      (AgentType)
import Agents          (Agent, isAgentType)
import Components      (Component(..))
import ComponentsFns   (getPositioned)
import Actions         (Action(..))
import ComponentWalking (getWalkingActions)

getHuntingActions :: [Agent] -> Agent -> Component -> [Action]
getHuntingActions targets ag (Hunting agType) = hunt (isAgentType agType) ag targets

hunt :: (Agent -> Bool) -> Agent -> [Agent] -> [Action]
hunt tf hunter targets = huntPreys hunter preys
  where preys = filter tf targets

huntPreys :: Agent -> [Agent] -> [Action]
huntPreys hunter []        = []
huntPreys hunter (prey:ps) = acts
  where 
    (Positioned huntPos) = getPositioned hunter
    (Positioned preyPos) = getPositioned prey
    acts = if huntPos == preyPos
      then [Eat hunter prey]
      else getWalkingActions hunter preyPos
