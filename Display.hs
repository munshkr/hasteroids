module Display (display, idle) where

import Graphics.Rendering.OpenGL hiding (position)
import Graphics.UI.GLUT hiding (position)
import Control.Monad (forM_, filterM)
import Data.IORef (IORef)
import Data.List (delete)
import Data.IORef

import Types
import Text (renderText)

viewportWidth  = 80.0
viewportHeight = 80.0

display :: ShipState -> IORef [RocketState] -> DisplayCallback
display ship rockets = do
  clear [ColorBuffer]
  loadIdentity
  scale 0.025 0.025 (0.025::GLfloat)

  renderText (-20, -30) 3 "0123456789"

  (x, y) <- get $ position ship
  ang    <- get $ angle ship
  rs     <- get $ rockets

  forM_ [-1, 0, 1] $ \i ->
    forM_ [-1, 0, 1] $ \j -> do
      drawShipAt (x + viewportWidth * i, y + viewportHeight * j) ang
      forM_ rs $ \r -> do
        (x, y) <- get $ rocketPosition r
        angle  <- get $ rocketAngle r
        drawRocketAt (x + viewportWidth * i, y + viewportHeight * j) angle

  swapBuffers

idle :: PlayerState -> ShipState -> IORef [RocketState] -> IdleCallback
idle player ship rockets = do
  as <- get $ actions player
  a  <- get $ angle ship
  (x, y)   <- get $ position ship
  (iX, iY) <- get $ inertia ship
  rs <- get rockets

  -- update angle and inertia based on actions
  forM_ as $ \action ->
    let dx = cos $ a * pi / 180.0
        dy = sin $ a * pi / 180.0
        c = 0.01
    in case action of
      TurnLeft   -> (angle ship)   $= (a + 5)
      TurnRight  -> (angle ship)   $= (a - 5)
      Accelerate -> (inertia ship) $= (iX + dx * c, iY + dy * c)
      Shoot      -> do
        newRocket <- makeRocket a (x, y) (dx, dy)
        rockets   $= newRocket : rs

  -- update position of the ship
  (position ship) $= ((x + iX) `fmod` viewportWidth,
                      (y + iY) `fmod` viewportHeight)

  -- update position of every rocket
  forM_ rs $ \r -> do
    (x, y)   <- get $ rocketPosition r
    (iX, iY) <- get $ rocketInertia r
    (rocketPosition r) $= (x + iX, y + iY)

  -- remove Shoot action to avoid rapid fire
  (actions player) $= delete Shoot as

  -- remove old rockets
  rs <- get rockets
  filteredRockets <- removeOldRockets rs
  rockets $= filteredRockets

  postRedisplay Nothing

drawShipAt :: (GLfloat, GLfloat) -> GLfloat -> IO ()
drawShipAt (x, y) angle = preservingMatrix $ do
  translate $ Vector3 x y (0::GLfloat)
  rotate angle $ Vector3 0 0 1
  drawShip

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

drawRocketAt :: (GLfloat, GLfloat) -> GLfloat -> IO ()
drawRocketAt (x, y) angle = preservingMatrix $ do
  translate $ Vector3 x y (0::GLfloat)
  rotate angle $ Vector3 0 0 1
  drawRocket

drawRocket :: IO ()
drawRocket = preservingMatrix $ do
  lineWidth $= 2.5
  rotate (90::GLfloat) $ Vector3 0 0 1
  translate $ Vector3 0 (-3) (0::GLfloat)
  color $ Color3 1 1 (1::GLfloat)
  renderPrimitive Lines $ do
    vertex $ Vertex2 0 (0::GLfloat)
    vertex $ Vertex2 0 (1::GLfloat)

makeRocket :: GLfloat -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO RocketState
makeRocket angle position inertia = do
  ip <- newIORef position
  a  <- newIORef angle
  p  <- newIORef position
  i  <- newIORef inertia
  return $ RocketState {
      rocketInitialPosition = ip,
      rocketAngle = a,
      rocketPosition = p,
      rocketInertia = i
    }

removeOldRockets :: [RocketState] -> IO [RocketState]
removeOldRockets = filterM $ \r -> do
  (x, y) <- get $ rocketPosition r
  (initX, initY) <- get $ rocketInitialPosition r
  let dx = abs (x - initX)
      dy = abs (y - initY)
  return (dx <= viewportWidth && dy <= viewportHeight)
