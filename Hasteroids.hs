import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Display

main = do
  (progname, _) <- getArgsAndInitialize

  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 600 600
  lineSmooth $= Enabled

  angle <- newIORef (0.0::GLfloat)
  --position <- newIORef (0.0::GLfloat, 0.0)

  createWindow "Hasteroids"

  displayCallback $= (display angle)
  idleCallback $= Just idle
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse angle)

  mainLoop


reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)

keyboardMouse _ (Char '\ESC') Down _ _ = leaveMainLoop
keyboardMouse angle key Down _ _ = do
  keyboardAct angle key
  a <- get angle
  putStrLn $ show a
keyboardMouse _ _ _ _ _ = return ()

keyboardAct :: IORef GLfloat -> Key -> IO ()
keyboardAct angle (SpecialKey KeyLeft) = do
  a <- get angle
  angle $= a + 5
keyboardAct angle (SpecialKey KeyRight) = do
  a <- get angle
  angle $= a - 5
keyboardAct _ _ = return ()

{-
keyboardAct pos (SpecialKey KeyUp) = do
  (x, y) <- get pos
  pos $= (x, y - 0.1)
keyboardAct pos (SpecialKey KeyDown) = do
  (x, y) <- get pos
  pos $= (x, y + 0.1)
-}
