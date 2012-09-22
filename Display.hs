module Display (display, idle) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Monad (forM_)

import Types
import Text (renderText)

display position angle = do
  clear [ColorBuffer]
  loadIdentity
  scale 0.025 0.025 (0.025::GLfloat)

  renderText (-20, -30) 3 "0123456789"

  a <- get angle
  (x, y) <- get position

  translate $ Vector3 x y (0::GLfloat)
  rotate a $ Vector3 0 0 1
  drawShip

  swapBuffers


idle actions position angle inertia = do
  as <- get actions
  ang <- get angle
  (iX, iY) <- get inertia

  -- update angle and inertia based on actions
  forM_ as $ \(action) ->
    let dx = cos $ ang * pi / 180.0
        dy = sin $ ang * pi / 180.0
        c = 0.01
    in case action of
      TurnLeft  -> angle $= (ang + 5)
      TurnRight -> angle $= (ang - 5)
      Accelerate    -> inertia $= (iX + dx * c, iY + dy * c)

  -- update position
  (x, y) <- get position
  position $= (x + iX, y + iY)

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
