module ActionsEngine (getActions, executeAction) where

import Agents (Agent(..))
import Components (Component(..))
import Actions (Action(..))
import ComponentHunter (getHunterActions)
import ActionsMove (moveAgent)

getActions :: Agent -> [Agent] -> [Action]
getActions ag ags = concat $ map (getComponentActions ags ag) (components ag)

-- TODO how to make this more open/close?
getComponentActions :: [Agent] -> Agent -> Component -> [Action]
getComponentActions targets ag h@(Hunter agType) = getHunterActions targets ag h
getComponentActions _ _ _ = []

-- TODO how to make this more open/close?
executeAction :: [Agent] -> Action -> [Agent]
executeAction ags m@(Move ag mv) = moveAgent ags m
executeAction ags _ = ags
