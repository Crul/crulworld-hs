module ComponentsFns (getPositioned, getWalker) where

import Agents (Agent(..))  -- TODO is there a better way to avoid cyclic dependecies with Agent?
import Components (Component(..), isPositioned, isWalker)
import Geometry (Position)

getComponent :: (Component -> Bool) -> Agent -> Component
getComponent ff ag = head $ filter ff (components ag)  -- TODO warning head

getPositioned :: Agent -> Component
getPositioned = getComponent isPositioned

getWalker :: Agent -> Component
getWalker = getComponent isWalker
