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
  angle <- newIORef (0.0::GLfloat)
  actions <- newIORef ([]::[Action])

  createWindow "Hasteroids"

  keyboardMouseCallback $= Just (keyboardMouse actions)
  displayCallback $= (display angle)
  idleCallback $= Just (idle actions angle)
  reshapeCallback $= Just reshape

  mainLoop


reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
