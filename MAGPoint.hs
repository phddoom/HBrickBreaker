module MAGPoint where
import Graphics.Rendering.OpenGL

type MAGPoints = [MAGPoint]

data MAGPoint = MAGPoint { x :: GLfloat,
                           y :: GLfloat,
                           z :: GLfloat
                         } deriving (Show)

dx :: MAGPoint -> GLfloat -> MAGPoint
dx point@(MAGPoint x _ _) dx' = point { x = x + dx' }

dy :: MAGPoint -> GLfloat -> MAGPoint
dy point@(MAGPoint _ y _) dy' = point { y = y + dy' }

dz :: MAGPoint -> GLfloat -> MAGPoint
dz point@(MAGPoint _ _ z) dz' = point { z = z + dz' }

toTuple :: MAGPoint -> (GLfloat, GLfloat, GLfloat)
toTuple (MAGPoint { x = x', y = y', z = z'}) = (x', y', z')

fromTuple :: (GLfloat, GLfloat, GLfloat) -> MAGPoint
fromTuple (x', y', z') = MAGPoint { x = x', y = y', z = z'}

pointSquareCollision :: MAGPoint -> MAGPoints -> Bool
pointSquareCollision p ps = collision
  where (xs, ys, zs) = unzip3 $ map toTuple ps
        xmin = minimum xs
        xmax = maximum xs
        ymin = minimum ys
        ymax = maximum ys
        x' = x p
        y' = y p
        collision = x' >= xmin && x' <= xmax && y' >= ymin && y' <= ymax

-- Must be used inside a renderPrimitive block/call
drawPoint :: MAGPoint -> IO ()
drawPoint MAGPoint { x = x, y = y, z = z} = vertex $ Vertex3 x y z
