module Geometry (Position (..), Movement (..), Vector (..), getPosVector, getMovVector, vecAdd) where

data Vector = Vector {x :: Float, y :: Float}
instance Show Vector where
  show v = "(" ++ (show $ x v) ++ "," ++ (show $ y v) ++ ")"
  
vecAdd :: Vector -> Vector -> Vector
vecAdd v1 v2 = Vector ((x v1) + (x v2)) ((y v1) + (y v2))

newtype Position = Position Vector deriving (Show)
getPosVector :: Position -> Vector
getPosVector (Position v) = v

newtype Movement = Movement Vector
getMovVector :: Movement -> Vector
getMovVector (Movement v) = v
