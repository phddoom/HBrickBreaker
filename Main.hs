module Main (main) where
import GameState
import Paddle
import Brick
import Breaker
import MAGPoint
import MAGColor
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import GameObjects (Drawable(..))


main = do
  (progname, _) <- getArgsAndInitialize
  storedState <- newIORef level1
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= (Size 800 800)
  createWindow "Brick Attack"
  displayCallback $= (display storedState)
  idleCallback $= Just (idleFn storedState)
  -- reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (manageInput storedState)
  initFn
  mainLoop

initFn :: IO ()
initFn = do
  matrixMode $= Projection
  ortho 0.0 80.0 0.0 80.0 0.0 1.0

manageInput :: IORef GameState -> Key -> KeyState -> Modifiers -> Position -> IO ()
manageInput storedState key keystate modifiers pos = do
  keyAct storedState key keystate
  postRedisplay Nothing

keyAct :: IORef GameState -> Key -> KeyState -> IO ()
keyAct storedState (SpecialKey KeyLeft) Down = do
  state <- get storedState
  writeIORef storedState state{paddle = movePaddle (paddle state) (-2)}

keyAct storedState (SpecialKey KeyRight) Down = do
  state <- get storedState
  writeIORef storedState state{paddle = movePaddle (paddle state) 2}

keyAct _ _ _ = return ()

idleFn :: IORef GameState -> IO ()
idleFn state = do
  gameState <- readIORef state
  writeIORef state (updateState (determineUpdate gameState) gameState)
  postRedisplay Nothing

display :: IORef GameState -> DisplayCallback
display state = do
  gameState <- readIORef state
  clear [ColorBuffer]
  drawGame gameState
  swapBuffers
