{-# LANGUAGE OverloadedLabels, OverloadedStrings, ImplicitParams #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified GI.Gio as G

import Data.Text (Text, pack)
import System.Process.Typed

import Control.Applicative
import Control.Monad
import Data.Maybe
import System.Environment
import System.Exit

import qualified Paths_session_quit as Paths
import Local

applicationName :: Text
applicationName = "Session quit"

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
  connectCallbackSymbols builder signals

  Just win <- getBuilderObj builder "appWindow" Gtk.ApplicationWindow
  win `set` [ Gtk.windowApplication := app ]
  winName <- fromMaybe applicationName <$> #getTitle win
  
  titleBar <- getBuilderObj builder "headerTitleBar" Gtk.HeaderBar
  maybe (return ()) (`set` [ Gtk.headerBarTitle := winName ]) titleBar 
  Gtk.windowSetTitlebar win titleBar

  #showAll win

-- Signals

signals :: [(Text, IO ())]
signals = [ ("on_buttonCustom_clicked", onButtonCustomClicked)
          , ("on_buttonLock_clicked", onButtonLockClicked)
          , ("on_buttonLogout_clicked", onButtonLogoutClicked)
          , ("on_buttonPoweroff_clicked", onButtonPoweroffClicked)
          , ("on_buttonHibernate_clicked", onButtonHibernateClicked)
          , ("on_buttonSuspend_clicked", onButtonSuspendClicked)
          , ("on_buttonRestart_clicked", onButtonRestartClicked)
          ]
          
onButtonCustomClicked :: IO ()
onButtonCustomClicked = putStrLn "Custom button clicked!"

onButtonLockClicked :: IO ()
onButtonLockClicked = runProcess_ "slock" >> exitSuccess

onButtonLogoutClicked :: IO ()
onButtonLogoutClicked = runProcess_ . shell $
  {- Kills a process with the same name as the window manager.
     This doesn't always work; Cinnamon, for example, will output "Mutter",
     which doesn't match the process name. -}
  unwords [ "pkill -f \"$("
          , "xprop -id"
          , "$(xprop -root -notype"
          , "| awk '$1==\"_NET_SUPPORTING_WM_CHECK:\"{print $5}')"
          , " -notype -f _NET_WM_NAME 8t"
          , "| grep '_NET_WM_NAME = '"
          , "| cut --delimiter=' ' --fields 3"
          , "| cut --delimiter='\"' --fields 2"
          , ")\""
          ]

onButtonPoweroffClicked :: IO ()
onButtonPoweroffClicked = runProcess_ "shutdown now"

onButtonHibernateClicked :: IO ()
onButtonHibernateClicked = runProcess_ "systemctl hibernate" >> exitSuccess

onButtonSuspendClicked :: IO ()
onButtonSuspendClicked = runProcess_ "systemctl suspend" >> exitSuccess

onButtonRestartClicked :: IO ()
onButtonRestartClicked = runProcess_ "reboot"
