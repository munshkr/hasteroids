module Types where

import Data.IORef (IORef)
import Graphics.Rendering.OpenGL (GLfloat)

data ShipState = ShipState {
    angle    :: IORef GLfloat,
    position :: IORef (GLfloat, GLfloat),
    inertia  :: IORef (GLfloat, GLfloat)
  } deriving (Eq)

data PlayerState = PlayerState {
    actions :: IORef [PlayerAction]
  } deriving (Eq)

data RocketState = RocketState {
    rocketInitialPosition :: IORef (GLfloat, GLfloat),
    rocketAngle           :: IORef GLfloat,
    rocketPosition        :: IORef (GLfloat, GLfloat),
    rocketInertia         :: IORef (GLfloat, GLfloat)
  } deriving (Eq)

data PlayerAction = TurnLeft | TurnRight | Accelerate | Shoot
  deriving (Eq, Show)

fmod :: RealFrac a => a -> a -> a
fmod n 0 = n / 0
fmod n m = n - d * m
  where d = fromIntegral (truncate (n / m))
