module Display (display, idle) where

import Graphics.Rendering.OpenGL hiding (position)
import Graphics.UI.GLUT hiding (position)
import Control.Monad (forM_)
import Data.IORef (IORef)

import Types
import Text (renderText)

display :: ShipState -> DisplayCallback
display ship = do
  clear [ColorBuffer]
  loadIdentity
  scale 0.025 0.025 (0.025::GLfloat)

  renderText (-20, -30) 3 "0123456789"

  (x, y) <- get $ position ship
  translate $ Vector3 x y (0::GLfloat)

  angle <- get $ angle ship
  rotate angle $ Vector3 0 0 1

  drawShip

  swapBuffers


idle :: PlayerState -> ShipState -> IdleCallback
idle player ship = do
  as <- get $ actions player
  a  <- get $ angle ship
  (iX, iY) <- get $ inertia ship

  -- update angle and inertia based on actions
  forM_ as $ \action ->
    let dx = cos $ a * pi / 180.0
        dy = sin $ a * pi / 180.0
        c = 0.01
    in case action of
      TurnLeft   -> (angle ship)   $= (a + 5)
      TurnRight  -> (angle ship)   $= (a - 5)
      Accelerate -> (inertia ship) $= (iX + dx * c, iY + dy * c)
      _          -> return ()

  -- update position
  (x, y) <- get $ position ship
  (position ship) $= (x + iX, y + iY)

  --putStrLn $ show (x, y)

  postRedisplay Nothing


drawShip :: IO ()
drawShip = preservingMatrix $ do
  lineWidth $= 2.5
  rotate (90::GLfloat) $ Vector3 0 0 1
  translate $ Vector3 0 (-0.5) (0::GLfloat)
  color $ Color3 1 1 (1::GLfloat)
  renderPrimitive LineLoop $ do
    forM_ points $ \(x, y) -> vertex $ Vertex2 x y
      where points = shipPoints 2 3

shipPoints :: GLfloat -> GLfloat -> [(GLfloat, GLfloat)]
shipPoints w h =
  [ (-(w / 2),  h / 2),
    (0,       -(h / 2)),
    (  w / 2,   h / 2),
    (0,         h / 4) ]
