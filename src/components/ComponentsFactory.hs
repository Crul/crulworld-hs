module ComponentsFactory (makePositioned, makeWalker, makeChaser) where

import AgentTypes (AgentType)
import Components (Component(..), Speed)
import Geometry (makePosition)

makePositioned :: Float -> Float -> Component
makePositioned x y = Positioned $ makePosition x y

makeWalker :: Speed -> Component
makeWalker s = Walker s

makeChaser :: AgentType -> Component
makeChaser a = Chaser a
