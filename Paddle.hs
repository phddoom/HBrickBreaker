module Paddle where
import MAGPoint
import MAGColor
import Graphics.Rendering.OpenGL

data Paddle = Paddle { paddleColor  :: MAGColor,
                       paddlePoints :: MAGPoints
                     } deriving (Show)

drawPaddle :: Paddle -> IO ()
drawPaddle (Paddle {paddlePoints = ps, paddleColor = c}) = do
  renderPrimitive Quads $ do
    setColor c
    mapM_ drawPoint ps
  flush

movePaddle :: Paddle -> GLfloat -> Paddle
movePaddle p@(Paddle { paddlePoints = ps}) dx' =
  p { paddlePoints = map (`dx` dx') ps}

centerX :: Paddle -> GLfloat
centerX (Paddle {paddlePoints = ps}) =
    (foldr (+) 0 $ map x ps) / (fromIntegral $ length  ps)

minX :: Paddle -> GLfloat
minX (Paddle {paddlePoints = ps}) = minimum $ map x ps

maxX :: Paddle -> GLfloat
maxX (Paddle {paddlePoints = ps}) = maximum $ map x ps
