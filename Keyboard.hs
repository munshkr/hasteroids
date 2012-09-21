module Keyboard (keyboardMouse) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.List (nub, delete)
import Data.IORef

import Types

keyboardMouse :: IORef [Action] -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse actions key Down _ _
  | key == Char '\ESC' = leaveMainLoop
  | otherwise = do
    as <- get actions
    actions $= case (actionFor key) of
      Just action -> nub (action : as)
      Nothing -> as
    --printState actions key Down
keyboardMouse actions key Up _ _ = do
  as <- get actions
  actions $= case (actionFor key) of
    Just action -> delete action as
    Nothing -> as
  --printState actions key Up

actionFor :: Key -> Maybe Action
actionFor (SpecialKey KeyLeft)  = Just TurnLeft
actionFor (SpecialKey KeyRight) = Just TurnRight
actionFor (SpecialKey KeyUp)    = Just Accelerate
actionFor (SpecialKey KeyDown)  = Just Desaccelerate
actionFor _ = Nothing

printState :: IORef [Action] -> Key -> KeyState -> IO ()
printState actions key state = do
  as <- get actions
  putStrLn $ show (as, key, state)
