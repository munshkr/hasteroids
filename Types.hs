module Types where

data Action = TurnLeft
            | TurnRight
            | Accelerate
            | Desaccelerate
     deriving (Eq, Show)
