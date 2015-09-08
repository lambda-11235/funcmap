{-|
Module: Menu
Description: Sets up the menubar.
Copyright: (c) Taran Lynn, 2015
License: GPL-2

This module contains functions to setup a menubar
-}

module Menu (setupMenu) where

import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks

setupMenu :: IO (Widget, AddHandler ())
setupMenu = do
  menu <- actionGroupNew "Menu"

  file <- actionNew "File" "File" Nothing Nothing
  actionGroupAddAction menu file

  save <- actionNew "Save" "Save As" (Just "Save image as PNG") Nothing
  (saveHandler, saveFire) <- newAddHandler
  on save actionActivated $ saveFire ()
  actionGroupAddAction menu save

  manager <- uiManagerNew
  uiManagerAddUiFromString manager uiDecl
  uiManagerInsertActionGroup manager menu 0
  mMenu <- uiManagerGetWidget manager "/ui/menubar"

  case mMenu of
   Just aMenu -> return (aMenu, saveHandler)
   Nothing -> error "Cannot create menubar"

uiDecl :: String
uiDecl = "<ui>\
\           <menubar>\
\             <menu action=\"File\">\
\               <menuitem action=\"Save\" />\
\             </menu>\
\           </menubar>\
\         </ui>"
