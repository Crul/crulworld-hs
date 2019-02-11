module Components (Component(..), Speed, isPositioned, isWalking) where

import Geometry   (Position)
import AgentTypes (AgentType)

type Speed = Float

data Component  -- TODO how to express components' dependencies
  = Positioned Position
  | Walking Speed
  | Hunting AgentType  -- TODO allow multiple AgentTypes

instance Show Component where
  show (Positioned p) = show p
  show (Walking sp)   = "Walking"
  show (Hunting at)   = "Hunting:" ++ (show at)

isPositioned :: Component -> Bool  -- TODO is there a better way?
isPositioned (Positioned _) = True
isPositioned _              = False

isWalking :: Component -> Bool  -- TODO is there a better way?
isWalking (Walking _) = True
isWalking _           = False
