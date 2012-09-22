module Keyboard (keyboardMouse) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.List (nub, delete)
import Data.IORef

import Types

keyboardMouse :: PlayerState -> KeyboardMouseCallback
keyboardMouse player key Down _ _
  | key == Char '\ESC' = leaveMainLoop
  | otherwise = do
    as <- get $ actions player
    (actions player) $= case (actionFor key) of
      Just action -> nub (action : as)
      Nothing -> as

keyboardMouse player key Up _ _ = do
  as <- get $ actions player
  (actions player) $= case (actionFor key) of
    Just action -> delete action as
    Nothing -> as

actionFor :: Key -> Maybe PlayerAction
actionFor (SpecialKey KeyLeft)  = Just TurnLeft
actionFor (SpecialKey KeyRight) = Just TurnRight
actionFor (SpecialKey KeyUp)    = Just Accelerate
actionFor _ = Nothing
