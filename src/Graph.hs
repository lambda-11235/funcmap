
module Graph where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Matrix (Matrix (Matrix))

-- | Draws a graph on a drawing area.
graph :: DrawingArea           -- ^ The drawing area to graph on.
         -> [(Double, Double)] -- ^ The (x,y) points to graph.
         -> (Double, Double)   -- ^ The minimum and maximum x values to graph.
         -> IO ()
graph da ps xrange = do dw <- widgetGetDrawWindow da
                        size <- widgetGetSize da
                        drawWindowClear dw
                        renderWithDrawable dw $ renderGraph ps xrange size

-- | Renders a graph.
renderGraph :: [(Double, Double)] -> (Double, Double) -> (Int, Int) -> Render ()
renderGraph ps xrange size = do drawNumLines size
                                drawLines' ps xrange size

drawNumLines :: (Int, Int) -> Render ()
drawNumLines (w, h) = let w' = fromIntegral w
                          h' = fromIntegral h
                          a1 = w' / 16
                          a2 = 15/16 * w'
                      in
                       do moveTo a1 0
                          lineTo a1 (fromIntegral h)
                          moveTo a2 0
                          lineTo a2 (fromIntegral h)
                          stroke

drawLines' :: [(Double, Double)] -> (Double, Double) -> (Int, Int) -> Render ()
drawLines' ps (xmin, xmax) (w, h) = do mapM drawLine ps
                                       stroke
  where
    drawLine (x, y) | isNaN x || isNaN y = return ()
                    | isInfinite x || isInfinite y = return ()
                    | otherwise = let w' = fromIntegral w
                                      h' = fromIntegral h
                                      a1 = w' / 16
                                      a2 = 15/16 * w'
                                      x' = (x - xmin) * h'/(xmax - xmin)
                                      y' = (y - xmin) * h'/(xmax - xmin)
                                  in
                                   do setMatrix (Matrix 1 0 0 (-1) 0 h')
                                      moveTo a1 x'
                                      lineTo a2 y'
