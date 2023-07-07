{-# LANGUAGE OverloadedLabels, OverloadedStrings, ImplicitParams #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified GI.Gio as G

import Data.Text (Text, pack)
import qualified Data.Text.IO as T

import Control.Applicative
import Control.Monad
import System.Environment
import System.Exit

import qualified Paths_session_quit as Paths
import Local

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "com.github.ThwyIgo.session-quit"
                             , On #activate (activate ?self) ]

  argv <- liftA2 (:) getProgName getArgs
  status <- #run app (Just argv)

  when (status /= 0) $
    exitWith $ ExitFailure (fromIntegral status)
  
activate :: Gtk.Application -> G.ApplicationActivateCallback
activate app = do
  builder <- Gtk.builderNewFromFile . pack =<< Paths.getDataFileName "resources/appWindow.glade"

  Just win <- getBuilderObj builder "appWindow" Gtk.ApplicationWindow
  win `set` [ Gtk.windowApplication := app ]

  #showAll win
