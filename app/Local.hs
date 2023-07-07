{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase #-}
module Local where

import qualified GI.Gtk as Gtk
import Data.GI.Base

import Data.Text (Text)
import qualified Data.Text.IO as T

getBuilderObj :: GObject o'
              =>  Gtk.Builder 
              -> Text                  -- Object's Id
              -> (ManagedPtr o' -> o') -- Object's Type
              -> IO (Maybe o')
getBuilderObj builder name gtkConstr = #getObject builder name >>= \case 
  Just obj -> castTo gtkConstr obj
  Nothing -> do
    T.putStrLn $ "Object named '" <> name <> "' could not be found."
    return Nothing
