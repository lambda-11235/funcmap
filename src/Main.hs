{-|
Module: Main
Description: The main module.
Copyright: (c) Taran Lynn, 2015
License: GPL-2

Sets up the main window and starts execution.
-}

module Main where

import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks

import AST
import Graph
import Menu
import Parser
import Save
import Utils (maybeRead)

-- | Initial values for the graph.
initGraphVals = ("sin(x)", "-5", "5", "0.1")

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

          (menu, saveHandler) <- setupMenu

          let (a, b, c, d) = initGraphVals in
           do entrySetText formula a
              entrySetText xmin b
              entrySetText xmax c
              entrySetText xstep d

          table <- tableNew 1 4 False
          tableAttach table menu 0 2 0 1 [Expand, Fill] [] 5 1
          tableAttach table formulaLabel 0 1 1 2 [] [] 5 1
          tableAttach table formula 1 2 1 2 [Expand, Fill] [] 5 1
          tableAttach table xminLabel 0 1 2 3 [] [] 5 1
          tableAttach table xmin 1 2 2 3 [Expand, Fill] [] 5 1
          tableAttach table xmaxLabel 0 1 3 4 [] [] 5 1
          tableAttach table xmax 1 2 3 4 [Expand, Fill] [] 5 1
          tableAttach table xstepLabel 0 1 4 5 [] [] 5 1
          tableAttach table xstep 1 2 4 5 [Expand, Fill] [] 5 1
          tableAttach table da 0 2 5 6 [Expand, Fill] [Expand, Fill] 5 1

          on window deleteEvent (liftIO mainQuit >> return False)

          set window [containerChild := table, windowTitle := "FuncMap"]

          (exposeHandler, exposeFire) <- newAddHandler
          (entryHandler, entryFire) <- newAddHandler

          on da exposeEvent $ do liftIO $ exposeFire ()
                                 return True

          let entryFire' = do formulaText <- (entryGetText formula)
                              xminText <- (entryGetText xmin)
                              xmaxText <- (entryGetText xmax)
                              xstepText <- (entryGetText xstep)
                              entryFire (formulaText, xminText, xmaxText, xstepText)


          on formula entryActivated $ entryFire'
          on xmin entryActivated $ entryFire'
          on xmax entryActivated $ entryFire'
          on xstep entryActivated $ entryFire'

          network <- compile (setupNetwork exposeHandler saveHandler entryHandler da)
          actuate network

          widgetShowAll window
          mainGUI

setupNetwork :: AddHandler ()
                -> AddHandler ()
                -> AddHandler (String, String, String, String)
                -> DrawingArea
                -> MomentIO ()
setupNetwork exposeHandler saveHandler entryHandler da = do
  eExpose <- fromAddHandler exposeHandler
  eSave <- fromAddHandler saveHandler
  eEntry <- fromAddHandler entryHandler

  bExpose <- stepper () eExpose
  bEntry <- stepper initGraphVals eEntry

  let bUpdate = bExpose *> bEntry
      eSave' = bEntry <@ eSave

  eUpdate <- changes bUpdate
  reactimate $ (\(a, b, c, d) -> savePNG a b c d) <$> eSave'
  reactimate' $ (fmap (\(a, b, c, d) -> updateGraph a b c d da)) <$> eUpdate


-- | Updates the graph, or clears it if an error occurs.
updateGraph :: String -> String -> String -> String -> DrawingArea -> IO ()
updateGraph formula xmin xmax xstep da = do
  updateGraph' (parseAST formula) (maybeRead xmin) (maybeRead xmax) (maybeRead xstep)
    where
      updateGraph' (Right ast) (Just xmin) (Just xmax) (Just xstep)
        = let xrange = [xmin,(xmin+xstep)..xmax]
              yrange = map (evalAST ast) xrange
              ps = zip xrange yrange
          in
           graph da ps (xmin, xmax)
      updateGraph' _ _ _ _ = do dw <- widgetGetDrawWindow da
                                drawWindowClear dw
