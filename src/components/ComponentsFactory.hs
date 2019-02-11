module ComponentsFactory (makePositioned, makeWalking, makeHunting) where

import Geometry   (makePosition)
import AgentTypes (AgentType)
import Components (Component(..), Speed)

makePositioned :: Float -> Float -> Component
makePositioned x y = Positioned $ makePosition x y

makeWalking :: Speed -> Component
makeWalking s = Walking s

makeHunting :: AgentType -> Component
makeHunting a = Hunting a
