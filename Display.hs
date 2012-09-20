module Display (display, idle) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Monad (forM_)

display angle = do
  clear [ColorBuffer]

  loadIdentity
  scale 0.05 0.05 (0.05::GLfloat)

  a <- get angle
  rotate a $ Vector3 0 0 1

  drawShip

  swapBuffers

idle = do
  postRedisplay Nothing


drawShip :: IO ()
drawShip = do
  lineWidth $= 2.5
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
