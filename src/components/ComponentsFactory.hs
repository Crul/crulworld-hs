module ComponentsFactory (makePositioned, makeWalker, makeHunter) where

import AgentTypes (AgentType)
import Components (Component(..), Speed)
import Geometry (makePosition)

makePositioned :: Float -> Float -> Component
makePositioned x y = Positioned $ makePosition x y

makeWalker :: Speed -> Component
makeWalker s = Walker s

makeHunter :: AgentType -> Component
makeHunter a = Hunter a
