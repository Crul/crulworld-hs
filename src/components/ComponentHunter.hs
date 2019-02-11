module ComponentHunter (getHunterActions) where

import AgentTypes (AgentType)
import Agents (Agent, isAgentType)
import Components (Component(..))
import ComponentsFns (getPositioned)
import Actions (Action)
import ComponentWalker (getWalkerActions)

getHunterActions :: [Agent] -> Agent -> Component -> [Action]
getHunterActions targets ag (Hunter agType) = getMoveTowardsFiltered (isAgentType agType) targets ag

getMoveTowardsFiltered :: (Agent -> Bool) -> [Agent] -> Agent -> [Action]
getMoveTowardsFiltered tf ags ag = mvs
  where
    trgt = head $ filter tf ags -- TODO warning head
    (Positioned trgtPos) = getPositioned trgt
    mvs = getWalkerActions ag trgtPos
