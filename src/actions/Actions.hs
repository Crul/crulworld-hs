module Actions (Action(..)) where

import Geometry (Movement)
import Agents   (Agent)

data Action
  = Move Agent Movement
  | Eat Agent Agent
