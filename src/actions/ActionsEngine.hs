module ActionsEngine (getActions, executeAction) where

import Agents           (Agent(..))
import Components       (Component(..))
import Actions          (Action(..))
import ComponentHunting (getHuntingActions)
import ActionsMove      (moveAgent)
import ActionsEat       (eatAgent)

getActions :: Agent -> [Agent] -> [Action]
getActions ag ags = concat $ map (getComponentActions ags ag) (components ag)

-- TODO how to make this more open/close?
getComponentActions :: [Agent] -> Agent -> Component -> [Action]
getComponentActions targets ag h@(Hunting agType) = getHuntingActions targets ag h
getComponentActions _ _ _                         = []

-- TODO how to make this more open/close?
executeAction :: [Agent] -> Action -> [Agent]
executeAction ags m@(Move ag mv)      = moveAgent ags m
executeAction ags e@(Eat hunter prey) = eatAgent ags e
-- executeAction ags _                   = ags
