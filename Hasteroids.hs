import Graphics.Rendering.OpenGL hiding (position)
import Graphics.UI.GLUT hiding (position)
import Data.IORef

import Types
import Display
import Keyboard

main :: IO ()
main = do
  getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 600 600

  -- enable anti-aliasing?
  lineSmooth $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  hint LineSmooth $= DontCare

  -- state
  ship    <- makeShipState
  player  <- makePlayerState
  rockets <- newIORef ([] :: [RocketState])

  createWindow "Hasteroids"

  -- callbacks
  keyboardMouseCallback $= Just (keyboardMouse player)
  displayCallback $= display ship rockets
  idleCallback $= Just (idle player ship rockets)
  reshapeCallback $= Just reshape

  mainLoop

reshape :: ReshapeCallback
reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)

makeShipState :: IO ShipState
makeShipState = do
  a <- newIORef 90
  p <- newIORef (0, 0)
  v <- newIORef (0, 0)
  return $ ShipState { angle = a, position = p, velocity = v }

makePlayerState :: IO PlayerState
makePlayerState = do
  as <- newIORef []
  return $ PlayerState { actions = as }
