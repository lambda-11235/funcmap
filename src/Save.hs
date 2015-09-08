{-|
Module: Save
Description: Functions to save graphs to files.
Copyright: (c) Taran Lynn, 2015
License: GPL-2

This module contains functions to save graphs to image files.
-}

module Save (savePNG) where

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import System.Directory (getCurrentDirectory)

import AST
import Graph
import Parser
import Utils (maybeRead)

-- | Saves the graph as a PNG.
savePNG :: String     -- ^ The formula to graph.
           -> String  -- ^ The minimum x value.
           -> String  -- ^ The maximum x value.
           -> String  -- ^ The steps between x values.
           -> IO ()
savePNG formula xmin xmax xstep = do
  savePNG' (parseAST formula) (maybeRead xmin) (maybeRead xmax) (maybeRead xstep)
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
 surf <- createImageSurface FormatARGB32 w h
 renderWith surf graph
 surfaceWriteToPNG surf file
