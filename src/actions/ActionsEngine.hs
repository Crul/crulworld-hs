module ActionsEngine (getActions, executeAction) where

import Agents (Agent(..))
import Components (Component(..))
import Actions (Action(..))
import ComponentChaser (getChaserActions)
import ActionsMove (moveAgent)

getActions :: Agent -> [Agent] -> [Action]
getActions ag ags = concat $ map (getComponentActions ags ag) (components ag)

-- TODO how to make this more open/close?
getComponentActions :: [Agent] -> Agent -> Component -> [Action]
getComponentActions targets ag (Chaser agType) = getChaserActions targets ag agType
getComponentActions _ _ _ = []

executeAction :: [Agent] -> Action -> [Agent]
executeAction ags (Move ag mv) = moveAgent ags ag mv
executeAction ags _ = ags
