import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Types
import Display
import Keyboard

main = do
  getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 600 600

  -- enable anti-aliasing?
  lineSmooth $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  hint LineSmooth $= DontCare

  -- state variables
  actions  <- newIORef ([]::[Action])
  angle    <- newIORef (90.0 ::GLfloat)
  position <- newIORef ((0, 0)::(GLfloat, GLfloat))
  inertia  <- newIORef ((0, 0)::(GLfloat, GLfloat))

  createWindow "Hasteroids"

  -- callbacks
  keyboardMouseCallback $= Just (keyboardMouse actions)
  displayCallback $= (display position angle)
  idleCallback $= Just (idle actions position angle inertia)
  reshapeCallback $= Just reshape

  mainLoop


reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
