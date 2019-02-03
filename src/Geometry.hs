module Geometry (
  Position, Movement, makePosition, makeMovement, getMovTowardsPos, applyMovement
) where

data Vector = Vector {x :: Float, y :: Float}

newtype Position = Position Vector
instance Show Position where
  show (Position v) = "(" ++ (show $ x v) ++ "," ++ (show $ y v) ++ ")"

newtype Movement = Movement Vector

makePosition :: Float -> Float -> Position
makePosition x y = Position $ Vector x y

makeMovement :: Float -> Float -> Movement
makeMovement x y = Movement $ Vector x y

getMovTowardsPos :: Position -> Position -> Float -> Movement
getMovTowardsPos (Position (Vector fromX fromY)) (Position (Vector toX toY)) speed =
  makeMovement x y
    where
      x = getMovTowardsPosComp fromX toX speed
      y = getMovTowardsPosComp fromY toY speed

getMovTowardsPosComp :: Float -> Float -> Float -> Float
getMovTowardsPosComp fromPosComp toPosComp speed =
  if (abs dst) < speed
    then dst
    else speed * (dst / abs dst)
  where dst = toPosComp - fromPosComp

applyMovement :: Position -> Movement -> Position
applyMovement (Position p) (Movement m) = Position $ p `vecAdd` m

vecAdd :: Vector -> Vector -> Vector
vecAdd v1 v2 = Vector ((x v1) + (x v2)) ((y v1) + (y v2))
