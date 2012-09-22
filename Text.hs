module Text (renderText, renderChar, TextPosition, TextSize) where

import Graphics.Rendering.OpenGL
import Control.Monad (forM_)

type TextPosition = (GLfloat, GLfloat)
type TextSize = GLfloat

renderText :: TextPosition -> TextSize -> String -> IO ()
renderText _ _ _ = return ()

renderChar :: TextPosition -> TextSize -> Char -> IO ()
renderChar pos size c = preservingMatrix $ do
  translate $ Vector3 (fst pos) (snd pos) 0
  scale size size size
  renderRawChar c

renderRawChar :: Char -> IO ()
renderRawChar c = do
  renderPrimitive Lines $ do
  forM_ (rawCharVertices c) $ \(x, y) -> vertex $ Vertex2 x y

rawCharVertices :: Char -> [(GLfloat, GLfloat)]
rawCharVertices c = let h = rawCharHeight
                        w = rawCharWidth
                 in case c of
  '0' -> [ (0,0), (w,0), (w,0), (w,h), (w,h), (0,h), (0,h), (0,0), (0,0), (w,h) ]
  '1' -> [ (w,0), (w,h) ]
  '2' -> [ (0,h), (w,h), (w,h), (w,h/2), (w,h/2), (0,h/2), (0,h/2), (0,0), (0,0), (w,0) ]
  '3' -> [ (0,h), (w,h), (0,h/2), (w,h/2), (0,0), (w,0), (w,h), (w,0) ]
  '4' -> [ (0,h), (0,h/2), (0,h/2), (w,h/2), (w,h), (w,0) ]
  _   -> [ (0,0), (w,0), (w,0), (w,h), (w,h), (0,h), (0,h), (0,0) ]

rawCharHeight = 1.0
rawCharWidth  = 0.6
