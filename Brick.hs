module Brick where
import Breaker
import MAGPoint
import MAGColor
import Graphics.Rendering.OpenGL
import GameObjects (Drawable(..))

type Bricks = [Brick]

data Brick = Brick { brickColor :: MAGColor,
                     points     :: MAGPoints,
                     drawB       :: Bool
                   } deriving (Show)


instance Drawable Brick where
    draw = drawBrick
    color = brickColor
    render = drawB

hit :: Breaker -> Brick -> (Breaker, Brick)
hit break brick@(Brick {points = ps})
  | up    = ((bounceX break){breakerUpdated = True},brick{drawB = False})
  | down  = ((bounceX break){breakerUpdated = True},brick{drawB = False})
  | left  = ((bounceY break){breakerUpdated = True},brick{drawB = False})
  | right = ((bounceY break){breakerUpdated = True},brick{drawB = False})
  | otherwise = (break, brick)
  where p = (center break)
        r = (radius break)
        right = pointSquareCollision (dx p r) ps
        left = pointSquareCollision (dx p (-r)) ps
        up = pointSquareCollision (dy p r) ps
        down = pointSquareCollision (dy p (-r)) ps

removeBrick :: Brick -> Bool
removeBrick (Brick { drawB = dw}) = dw

drawBricks :: [Brick] -> IO ()
drawBricks [] = flush
drawBricks (b : bs) = do
  drawBrick b
  drawBricks bs

drawBrick :: Brick -> IO ()
drawBrick (Brick { brickColor = c,
                   points = points,
                   drawB = draw }) = if draw
                                    then
                                    do
                                      renderPrimitive Quads $ do
                                        setColor c
                                        mapM_ drawPoint points
                                      flush
                                    else
                                      flush
