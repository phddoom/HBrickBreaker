module GameState where
import Brick
import Breaker
import Paddle
import MAGPoint
import MAGColor
import Graphics.Rendering.OpenGL
import Data.Maybe

data GameState = GameState { bricks  :: Bricks,
                             breaker :: Breaker,
                             paddle  :: Paddle,
                             ymax    :: GLfloat,
                             xmax    :: GLfloat
                           } deriving (Show)

data StateUpdate = MoveBreaker {updateBreaker :: Breaker}
                   | BreakerBrickCollision {updatedBricks :: Bricks,
                                            updateBreaker :: Breaker
                                           }
                   | BreakerPaddleCollision {updateBreaker :: Breaker}
                   | MovePaddle
                   | WallCollision {updateBreaker :: Breaker}
                   | Start
                   deriving (Show)

updateState :: StateUpdate -> GameState -> GameState

updateState (MoveBreaker move) game@(GameState {paddle = p}) =
  game{breaker = move}

updateState (BreakerBrickCollision bricks break) game =
  game{breaker = break, bricks = bricks}

updateState (BreakerPaddleCollision move) game = game{breaker = move}

updateState (WallCollision break) game = game{breaker = break}

determineUpdate :: GameState -> StateUpdate
determineUpdate game =
  if (isJust rs)
  then fromJust rs
  else fromJust $ breakerBrickCollision game
  where rs = breakerPaddleCollision game

breakerPaddleCollision :: GameState -> Maybe StateUpdate
breakerPaddleCollision state =
  if (collisionPaddle c r p)
  then Just $ BreakerPaddleCollision $ move2 b vx'
  else Nothing
  where b        = breaker state
        p        = paddle state
        c        = center b
        r        = radius b
        pCx      = centerX p
        pMinX    = minX p
        pMaxX    = maxX p
        stepSize = 1.0/(pCx - pMinX)
        vx'      = - ((pCx - (x c)) * stepSize)

collisionPaddle :: MAGPoint -> GLfloat -> Paddle -> Bool
collisionPaddle (MAGPoint x y z) r (Paddle {paddlePoints = ps}) =
  (((x+r >= xmin) && (x+r <= xmax) && (y+r >= ymin) && (y+r <= ymax)) ||
  ((x-r >= xmin) && (x-r <= xmax) && (y-r >= ymin) && (y-r <= ymax)))
  where (xs, ys, zs) = unzip3 (map toTuple ps)
        xmin = minimum xs
        xmax = maximum xs
        ymin = minimum ys
        ymax = maximum ys

breakerBrickCollision :: GameState -> Maybe StateUpdate
breakerBrickCollision state =
  if hit'
  then Just $ BreakerBrickCollision updatedBricks (move ((head updatedBreakers){breakerUpdated = False}) True)
  else wallCollision state
  where break = (breaker state)
        bricks' = (bricks state)
        (breakers, bricks'') = unzip (map (hit break) bricks')
        updatedBricks = filter removeBrick bricks''
        updatedBreakers = filter (\ test -> (updated test) == True) breakers
        hit' = not (null updatedBreakers)


wallCollision :: GameState -> Maybe StateUpdate
wallCollision state =
  if wallHit
  then Just $ WallCollision $ move move' False
  else Just $ MoveBreaker $ move (breaker state) False
  where x' = x (center (breaker state))
        y' = y $ center $ breaker state
        r = radius $ breaker state
        ymax' = ymax state
        xmax' = xmax state
        left = (x'+r <= 0) && (y'+r <= ymax') && (y'+r >= 0) || (x'-r <=0) && (y'-r <= ymax') && (y'-r >= 0)
        right = (x'+r >= xmax') && (y'+r <= ymax') && (y'+r >= 0) || (x'-r >= xmax') && (y'-r <= ymax') && (y'-r >= 0)
        up = (x'+r >= 0) && (x'+r <= xmax') && (y'+r >= ymax') || (x'-r >= 0) && (x'-r <= xmax') && (y'-r >= ymax')
        wallHit = left || right || up
        move' = if left || right then bounceX (breaker state) else bounceY (breaker state)

drawGame game = do
            mapM_ draw (bricks game)
            draw (breaker game)
            drawPaddle (paddle game)

myPaddle = Paddle (MAGColor 1 0 0 0)
                  [(MAGPoint 0 0 0),
                   (MAGPoint 0 1 0),
                   (MAGPoint 15 1 0),
                   (MAGPoint 15 0 0)]


myColors = [(fromRGB 255 0 0), (fromRGB 255 166 0), (fromRGB 255 255 0),
            (fromRGB 0 255 0), (fromRGB 0 0 255), (fromRGB 160 35 240),
            (fromRGB 255 255 255), (fromRGB 127 127 127)]

myBreaker = makeBreaker (fromRGB 255 127 255) (MAGPoint 8 33.5 0) 2.5


level1 = GameState (createBoard myColors (MAGPoint 0 80 0)) myBreaker myPaddle 80 80

createBoard :: MAGColors -> MAGPoint -> Bricks
createBoard colorList startPos = do
  concat (createBoardH colorList
    ((makePointList colorList startPos) :: MAGPoints))
  where
    createBoardH [] [] = []
    createBoardH (c : cs) (p : ps) = (makeRow p c) : createBoardH cs ps

makePointList :: MAGColors -> MAGPoint -> MAGPoints
makePointList [] _ = []
makePointList (c : cs) point = point : makePointList cs (dy point (-5))

makeRow :: MAGPoint -> MAGColor -> Bricks
makeRow pos@(MAGPoint {x = x, y = y, z = z}) color =
  if (x == 80)
  then []
  else (Brick {brickColor = color,
               points = (makePoints pos),
               drawB = True}) : makeRow (dx pos 10) color

makePoints :: MAGPoint -> MAGPoints
makePoints point = [point,
                    (dy point (-5)),
                    (dy (dx point 10) (-5)),
                    (dx point 10)]
