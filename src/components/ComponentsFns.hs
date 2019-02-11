module ComponentsFns (getPositioned, getWalking) where

import Agents     (Agent(..))  -- TODO is there a better way to avoid cyclic dependecies with Agent?
import Components (Component(..), isPositioned, isWalking)
import Geometry   (Position)

getComponent :: (Component -> Bool) -> Agent -> Component
getComponent ff ag = head $ filter ff (components ag)  -- TODO warning head

getPositioned :: Agent -> Component
getPositioned = getComponent isPositioned

getWalking :: Agent -> Component
getWalking = getComponent isWalking
