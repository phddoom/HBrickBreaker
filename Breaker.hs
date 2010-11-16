module Breaker(
    Breaker(breakerUpdated),
    makeBreaker,
    move,
    move2,
    draw,
    color,
    bounceX,
    bounceY,
    center,
    radius,
    updated) where
import MAGPoint
import MAGColor
import Graphics.Rendering.OpenGL(renderPrimitive, flush, GLfloat, PrimitiveMode(..))
import GameObjects(Drawable(..))

data Breaker = Breaker { breakerColor        :: MAGColor,
                         breakerCenter       :: MAGPoint,
                         breakerRadius       :: GLfloat,
                         breakerRender       :: Bool,
                         breakerVx           :: GLfloat,
                         breakerVy           :: GLfloat,
                         breakerUpdated      :: Bool
                       } deriving (Show)

instance Drawable Breaker where
    draw   = drawBreaker
    render = breakerRender
    color  = breakerColor

makeBreaker :: MAGColor -> MAGPoint -> GLfloat -> Breaker
makeBreaker color center radius = Breaker color center radius True 0.25 0.25 False

updated :: Breaker -> Bool
updated = breakerUpdated

center :: Breaker -> MAGPoint
center = breakerCenter

radius :: Breaker -> GLfloat
radius = breakerRadius

circlePoints x y radius number =
  [let alpha = twoPi * i /number
   in (x + radius*(sin (alpha)) , y +radius * (cos (alpha)),0) |
   i <- [1,2..number]]
  where
    twoPi = 2*pi



drawBreaker :: Breaker -> IO ()
drawBreaker breaker =
    if dw
    then
        do
            renderPrimitive Polygon $ do
                setColor c
                mapM_ drawPoint (map fromTuple (circlePoints x' y' r 100))
            flush
    else
        flush
    where dw = render breaker
          c  = color breaker
          r  = breakerRadius breaker
          center = breakerCenter breaker
          x' = x center
          y' = y center

move2 b@(Breaker{breakerVx = vx}) vx' = (move (move (move b True) False) False) {breakerVx = vx'}

move :: Breaker -> Bool -> Breaker
move breaker neg =
    if neg
    then breaker { breakerCenter = (dy (dx ps vx) vy),
                   breakerVx = (-vx),
                   breakerVy = (-vy)}
    else breaker { breakerCenter = (dy (dx ps vx) vy)}
    where ps = breakerCenter breaker
          vx = breakerVx breaker
          vy = breakerVy breaker


bounceX :: Breaker -> Breaker
bounceX break@(Breaker{breakerVx = vx}) = break{breakerVx = (-vx)}

bounceY :: Breaker -> Breaker
bounceY break@(Breaker{breakerVy = vy}) = break{breakerVy = (-vy)}
