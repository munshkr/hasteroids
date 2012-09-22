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

data PlayerAction = TurnLeft | TurnRight | Accelerate
  deriving (Eq, Show)
