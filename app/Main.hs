{-# LANGUAGE OverloadedLabels, OverloadedStrings, ImplicitParams #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified GI.Gio as G

import Data.Text (Text, pack)
import System.Process.Typed

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import System.Exit
import System.XDG

import qualified Paths_session_quit as Paths
import Local

applicationName :: Text
applicationName = "Session quit"

configDir, configFile :: IO FilePath
configDir = (++ "session-quit/") <$> getConfigHome
configFile = (++ "session-quit.cfg") <$> configDir

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
  builder <- Gtk.builderNewFromFile . pack =<< Paths.getDataFileName "resources/appWindow.ui"
  config <- loadConfig =<< configFile
  connectCallbackSymbols builder (signals config)

  Just win <- getBuilderObj builder "appWindow" Gtk.ApplicationWindow
  win `set` [ Gtk.windowApplication := app ]
  winName <- fromMaybe applicationName <$> #getTitle win

  titleBar <- getBuilderObj builder "headerTitleBar" Gtk.HeaderBar
  maybe (return ()) (`set` [ Gtk.headerBarTitle := winName ]) titleBar
  Gtk.windowSetTitlebar win titleBar

  #showAll win

-- Signals

signals :: Map.Map String String ->  [(Text, IO ())]
signals config =
  [ ("on_buttonCustom_clicked"   , runScript "Custom")
  , ("on_buttonLock_clicked"     , mapM_ runScript ["NoBacklight", "Lock"])
  , ("on_buttonLogout_clicked"   , runScript "Logout")
  , ("on_buttonPoweroff_clicked" , runScript "Shutdown")
  , ("on_buttonHibernate_clicked", mapM_ runScript ["Hibernate", "Lock"])
  , ("on_buttonSuspend_clicked"  , mapM_ runScript ["Suspend", "Lock"])
  , ("on_buttonRestart_clicked"  , runScript "Restart")
  ]
  where
  runScript script = runProcess_ . shell $ config Map.! script
