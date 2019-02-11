module Components (Component(..), Speed, isPositioned, isWalker) where

import Geometry   (Position)
import AgentTypes (AgentType)

type Speed = Float

data Component  -- TODO how to express components' dependencies
  = Positioned Position
  | Walker Speed
  | Hunter AgentType  -- TODO allow multiple AgentTypes

instance Show Component where
  show (Positioned p) = show p
  show (Walker sp)    = "Walker"
  show (Hunter at)    = "Hunter:" ++ (show at)

isPositioned :: Component -> Bool  -- TODO is there a better way?
isPositioned (Positioned _) = True
isPositioned _              = False

isWalker :: Component -> Bool  -- TODO is there a better way?
isWalker (Walker _) = True
isWalker _          = False
