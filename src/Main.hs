{-|
Module: Main
Description: The main module.
Copyright: (c) Taran Lynn, 2015
License: GPL-2

Sets up the main window and starts execution.
-}

module Main where

import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk

import AST
import Graph
import Parser

main = do initGUI

          window <- windowNew
          formulaLabel <- labelNew $ Just "Formula"
          formula <- entryNew
          xminLabel <- labelNew $ Just "Minimum x Value"
          xmin <- entryNew
          xmaxLabel <- labelNew $ Just "Maximum x Value"
          xmax <- entryNew
          xstepLabel <- labelNew $ Just "Change in x"
          xstep <- entryNew
          da <- drawingAreaNew

          entrySetText formula "sin(x)"
          entrySetText xmin "-5"
          entrySetText xmax "5"
          entrySetText xstep "0.1"

          table <- tableNew 1 4 False
          tableAttach table formulaLabel 0 1 0 1 [] [] 5 1
          tableAttach table formula 1 2 0 1 [Expand, Fill] [] 5 1
          tableAttach table xminLabel 0 1 1 2 [] [] 5 1
          tableAttach table xmin 1 2 1 2 [Expand, Fill] [] 5 1
          tableAttach table xmaxLabel 0 1 2 3 [] [] 5 1
          tableAttach table xmax 1 2 2 3 [Expand, Fill] [] 5 1
          tableAttach table xstepLabel 0 1 3 4 [] [] 5 1
          tableAttach table xstep 1 2 3 4 [Expand, Fill] [] 5 1
          tableAttach table da 0 2 4 5 [Expand, Fill] [Expand, Fill] 5 1

          on window deleteEvent (liftIO mainQuit >> return False)

          set window [containerChild := table, windowTitle := "FuncMap"]

          on da exposeEvent $ do liftIO $ updateGraph formula xmin xmax xstep da
                                 return True

          on formula entryActivated $ updateGraph formula xmin xmax xstep da
          on xmin entryActivated $ updateGraph formula xmin xmax xstep da
          on xmax entryActivated $ updateGraph formula xmin xmax xstep da
          on xstep entryActivated $ updateGraph formula xmin xmax xstep da

          widgetShowAll window
          mainGUI

-- | Updates the graph, or clears it if an error occurs.
updateGraph :: Entry -> Entry -> Entry -> Entry -> DrawingArea -> IO ()
updateGraph formula xmin xmax xstep da = do
  formula' <- entryGetText formula
  xmin' <- entryGetText xmin
  xmax' <- entryGetText xmax
  xstep' <- entryGetText xstep
  updateGraph' (parseAST formula') (maybeRead xmin') (maybeRead xmax') (maybeRead xstep')
    where
      updateGraph' (Right ast) (Just xmin) (Just xmax) (Just xstep)
        = let xrange = [xmin,(xmin+xstep)..xmax]
              yrange = map (evalAST ast) xrange
              ps = zip xrange yrange
          in
           graph da ps (xmin, xmax)
      updateGraph' _ _ _ _ = do dw <- widgetGetDrawWindow da
                                drawWindowClear dw

-- | Tries to read a string.
maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
                 [(x, "")] -> Just x
                 _ -> Nothing
