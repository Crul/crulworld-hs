module ComponentsFactory (makePositioned, makeWalker, makeHunter) where

import Geometry   (makePosition)
import AgentTypes (AgentType)
import Components (Component(..), Speed)

makePositioned :: Float -> Float -> Component
makePositioned x y = Positioned $ makePosition x y

makeWalker :: Speed -> Component
makeWalker s = Walker s

makeHunter :: AgentType -> Component
makeHunter a = Hunter a
