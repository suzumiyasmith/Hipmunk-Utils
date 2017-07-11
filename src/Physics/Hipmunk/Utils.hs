module Physics.Hipmunk.Utils where

import Physics.Hipmunk
import Data.StateVar
import Linear

initSpace :: IO Space
initSpace = do
  initChipmunk
  newSpace

addBody :: Space -> Double -> Double -> V2 Double -> IO Body
addBody s m i (V2 x y) = do
  ball <- newBody m i
  spaceAdd s ball
  position ball $= Vector x y
  return ball

addShape :: Space -> Body -> ShapeType -> Double -> Double -> V2 Double -> IO Shape
addShape space b shapeType f e (V2 x y) = do
  s <- newShape b shapeType (Vector x y)
  spaceAdd space s
  friction s $= f
  elasticity s $= e
  return s

circle :: Double -> ShapeType
circle = Circle

lineSegment :: V2 Double -> V2 Double -> Double -> ShapeType
lineSegment (V2 startX startY) (V2 endX endY) =
  LineSegment (Vector startX startY) (Vector endX endY)

polygon :: [V2 Double] -> ShapeType
polygon = Polygon . fmap (\(V2 x y) -> Vector x y)
