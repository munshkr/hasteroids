module Text (renderText, renderChar, TextPosition, TextSize) where

import Graphics.Rendering.OpenGL
import Control.Monad (forM_)

type TextPosition = (GLfloat, GLfloat)
type TextSize = GLfloat

renderText _ _ _ = return ()
{-
renderText :: TextPosition -> TextSize -> String -> IO ()
renderText _ _ [] = return ()
renderText pos size (x:xs) =
-}

renderChar :: TextPosition -> TextSize -> Char -> IO ()
renderChar pos size c = preservingMatrix $ do
  translate $ Vector3 (fst pos) (snd pos) 0
  scale size size size
  renderPrimitive Lines $ do
  forM_ (charVertices c) $ \(x, y) -> vertex $ Vertex2 x y

charVertices :: Char -> [(GLfloat, GLfloat)]
charVertices _ =
  [ (0, 0), (w, 0),
    (w, 0), (w, h),
    (w, h), (0, h),
    (0, h), (0, 0) ]
    where h = charHeight
          w = charWidth

charHeight = 1.0
charWidth  = 0.6
