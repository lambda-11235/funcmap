{-|
Module: Utils
Description: Utility functions.
Copyright: (c) Taran Lynn, 2015
License: GPL-2

This module contains utility functions not tied to the other modules.
-}

module Utils where

-- | Tries to read a value from a string.
maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
                 [(x, "")] -> Just x
                 _ -> Nothing
