{-# LANGUAGE OverloadedLabels, OverloadedStrings, ImplicitParams, LambdaCase #-}
module Main (main) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified GI.Gio as G
import qualified GI.GdkX11 as GdkX11
import qualified Graphics.X11.Xlib as Xlib
import qualified Graphics.X11.Xlib.Extras as XlibE
import Foreign.C.Types (CULong(..))
import Foreign.Ptr

import Data.Text (pack)
import System.Process.Typed
import System.Environment.XDG.BaseDir

import Control.Exception
import Control.Monad
import qualified Data.Map as Map
import Data.Functor
import System.Environment
import System.Exit

import qualified Paths_session_quit as Paths
import Local

foreign import ccall "gdk_x11_surface_get_xid" c_gdk_x11_surface_get_xid :: Ptr GdkX11.X11Surface -> IO CULong

configDir, configFile :: IO FilePath
configDir = getUserConfigDir "session-quit/"
configFile = configDir <&> (++ "session-quit.cfg")

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
  scope <- new Gtk.BuilderCScope []
  config <- (loadConfig =<< configFile) `catch` warnConfig
  unless (Map.null config) $ do
    registerSignals scope config

    builder <- new Gtk.Builder [ #scope := scope ]
    #addFromFile builder =<< Paths.getDataFileName "resources/appWindow.ui"

    Just win <- getBuilderObj builder "appWindow" Gtk.ApplicationWindow
    win `set` [ #application := app ]

    titleBar <- getBuilderObj builder "headerTitleBar" Gtk.HeaderBar
    #setTitlebar win titleBar

    #realize win
    setTypeHintDialog =<< Gtk.toWindow win
    #present win
  where
    warnConfig :: ConfigError -> IO (Map.Map a b)
    warnConfig e = do
      cfgPath <- configFile
      txtBuffer <- new Gtk.TextBuffer [ #text := pack $ "Error loading " ++ cfgPath ++ '\n' : show e ]
      txtView   <- new Gtk.TextView   [ #buffer        := txtBuffer
                                      , #editable      := False
                                      , #cursorVisible := False
                                      ]
      win <- new Gtk.Window [ #application   := app
                            , #title         := "Error"
                            , #modal         := True
                            , #resizable     := True
                            , #child         := txtView
                            , #defaultWidth  := 500
                            , #defaultHeight := 200
                            , On #closeRequest $ throwIO e >> return True
                            ]

      #realize win
      setTypeHintDialog win
      #present win
      return Map.empty

-- type-hint can only be set after window is realized
setTypeHintDialog :: Gtk.Window -> IO ()
setTypeHintDialog win = do
  Gtk.nativeGetSurface win >>= \case
    Nothing -> ioError $ userError "Can't get native surface. Window is probably not realized."
    Just surface -> do
      castTo GdkX11.X11Surface surface >>= \case
        Nothing -> return ()
        Just x11Surface -> do
          xid <- withManagedPtr x11Surface c_gdk_x11_surface_get_xid
          dpy <- Xlib.openDisplay ""
          let window = fromIntegral xid
          atomWinType <- Xlib.internAtom dpy "_NET_WM_WINDOW_TYPE" False
          atomDialog <- Xlib.internAtom dpy "_NET_WM_WINDOW_TYPE_DIALOG" False
          atomAtom <- Xlib.internAtom dpy "ATOM" False
          XlibE.changeProperty32 dpy window atomWinType atomAtom XlibE.propModeReplace [fromIntegral atomDialog]

          let scr = Xlib.defaultScreen dpy
          let sw = fromIntegral $ Xlib.displayWidth dpy scr :: Int
          let sh = fromIntegral $ Xlib.displayHeight dpy scr :: Int

          (ww32, wh32) <- Gtk.windowGetDefaultSize win
          let ww = fromIntegral ww32 :: Int
          let wh = fromIntegral wh32 :: Int
          let x = (sw - ww) `div` 2
          let y = (sh - wh) `div` 2

          Xlib.moveWindow dpy window (fromIntegral x) (fromIntegral y)

          Xlib.flush dpy
          Xlib.closeDisplay dpy

registerSignals :: Gtk.BuilderCScope -> Map.Map String String -> IO ()
registerSignals scope config = do
  let runScript script = do
        cmd <- case config Map.!? script of
           Just s -> return s
           Nothing -> (Map.! script) <$> loadConfig "resources/defaultConfig.cfg"
        runProcess_ (shell cmd)

  let handlers =
        [ ("on_buttonCustom_clicked",    runScript "Custom")
        , ("on_buttonLock_clicked",      mapM_ runScript ["NoBacklight", "Lock"])
        , ("on_buttonLogout_clicked",    runScript "Logout")
        , ("on_buttonPoweroff_clicked",  runScript "Shutdown")
        , ("on_buttonHibernate_clicked", mapM_ runScript ["Hibernate", "Lock"])
        , ("on_buttonSuspend_clicked",   mapM_ runScript ["Suspend", "Lock"])
        , ("on_buttonRestart_clicked",   runScript "Restart")
        ]

  forM_ handlers $ \(name, action) ->
    #addCallbackSymbol scope name (action >> exitSuccess)
