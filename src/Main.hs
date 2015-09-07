{-|
Module: Main
Description: The main module.
Copyright: (c) Taran Lynn, 2015
License: GPL-2

Sets up the main window and starts execution.
-}

module Main where

import Control.Monad.Trans(liftIO)
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.UI.Gtk
import System.Directory (getCurrentDirectory)

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

          menu <- setupMenu formula xmin xmax xstep

          entrySetText formula "sin(x)"
          entrySetText xmin "-5"
          entrySetText xmax "5"
          entrySetText xstep "0.1"

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

          on da exposeEvent $ do liftIO $ updateGraph formula xmin xmax xstep da
                                 return True

          on formula entryActivated $ updateGraph formula xmin xmax xstep da
          on xmin entryActivated $ updateGraph formula xmin xmax xstep da
          on xmax entryActivated $ updateGraph formula xmin xmax xstep da
          on xstep entryActivated $ updateGraph formula xmin xmax xstep da

          widgetShowAll window
          mainGUI

setupMenu :: Entry -> Entry -> Entry -> Entry -> IO Widget
setupMenu formula xmin xmax xstep = do
  menu <- actionGroupNew "Menu"


  file <- actionNew "File" "File" Nothing Nothing
  actionGroupAddAction menu file

  save <- actionNew "Save" "Save As" (Just "Save image as PNG") Nothing
  on save actionActivated $ savePNG formula xmin xmax xstep
  actionGroupAddAction menu save

  manager <- uiManagerNew
  uiManagerAddUiFromString manager uiDecl
  uiManagerInsertActionGroup manager menu 0
  mMenu <- uiManagerGetWidget manager "/ui/menubar"

  case mMenu of
   Just aMenu -> return aMenu
   Nothing -> error "Cannot create menubar"

uiDecl :: String
uiDecl = "<ui>\
\           <menubar>\
\             <menu action=\"File\">\
\               <menuitem action=\"Save\" />\
\             </menu>\
\           </menubar>\
\         </ui>"

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

-- | Saves the graph as a PNG.
savePNG :: Entry -> Entry -> Entry -> Entry -> IO ()
savePNG formula xmin xmax xstep = do
  formula' <- entryGetText formula
  xmin' <- entryGetText xmin
  xmax' <- entryGetText xmax
  xstep' <- entryGetText xstep
  savePNG' (parseAST formula') (maybeRead xmin') (maybeRead xmax') (maybeRead xstep')
    where
      savePNG' (Right ast) (Just xmin) (Just xmax) (Just xstep)
        = let xrange = [xmin,(xmin+xstep)..xmax]
              yrange = map (evalAST ast) xrange
              ps = zip xrange yrange
          in
           do file <- getSaveFile
              case file of
               (Just name) -> cairoSave name ps (xmin, xmax)
               Nothing -> return ()
      savePNG' _ _ _ _ = return ()

-- | Retrieves a PNG file to save to.
getSaveFile :: IO (Maybe String)
getSaveFile = do fcd <- (fileChooserDialogNew (Just "Save As") Nothing
                         FileChooserActionSave [("Cancel", ResponseCancel),
                                                ("Save", ResponseAccept)])

                 filter <- fileFilterNew
                 fileFilterSetName filter "PNG"
                 fileFilterAddMimeType filter "image/png"
                 fileChooserAddFilter fcd filter

                 dir <- getCurrentDirectory
                 fileChooserSetCurrentFolder fcd dir

                 response <- dialogRun fcd
                 if response == ResponseAccept then
                   do mFileName <- fileChooserGetFilename fcd
                      case mFileName of
                       (Just name) -> do widgetDestroy fcd
                                         return (Just name)
                       Nothing -> do widgetDestroy fcd
                                     return Nothing
                 else
                   do widgetDestroy fcd
                      return Nothing

-- | Does the actual writing to the file using cairo.
cairoSave file ps xrange = do
 let w = 800
     h = 600
     graph = renderGraph ps xrange (w, h)
 surf <- Cairo.createImageSurface Cairo.FormatARGB32 w h
 Cairo.renderWith surf graph
 Cairo.surfaceWriteToPNG surf file

-- | Tries to read a string.
maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
                 [(x, "")] -> Just x
                 _ -> Nothing
