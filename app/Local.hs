{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase, ViewPatterns #-}
module Local where

import qualified GI.Gtk as Gtk
import Data.GI.Base

import Data.Text (Text)
import qualified Data.Text.IO as T

import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Data.Map as Map
import Data.Char
import System.Directory
import System.IO
import Foreign.Ptr

import qualified Paths_session_quit as Paths

-- Get an Object and cast it to a Type
getBuilderObj :: GObject o'
              => Gtk.Builder
              -> Text                  -- Object's Id
              -> (ManagedPtr o' -> o') -- Object's Type
              -> IO (Maybe o')
getBuilderObj builder name gtkConstr = Gtk.builderGetObject builder name >>= \case
  Just obj -> castTo gtkConstr obj
  Nothing -> do
    T.hPutStr stderr $ "Object named '" <> name <> "' could not be found."
    return Nothing

connectCallbackSymbols :: Gtk.Builder
                       -> [(Text, IO ())] -- [(SignalName, CallbackFunction)]
                       -> IO ()
connectCallbackSymbols builder signalsList = do
  mapM_ (uncurry $ #addCallbackSymbol builder) signalsList
  #connectSignals builder nullPtr

{- Gets the configuration from configPath. Create the default configuration if
   configPath doesn't exist -}
loadConfig :: FilePath -> MaybeT IO (Map.Map String String)
loadConfig configPath =
  let defaultConfig = readFile =<< Paths.getDataFileName "resources/defaultConfig.cfg"
      createConfig = doesFileExist configPath >>= \b -> unless b $
        writeFile configPath =<< defaultConfig
        
      config = createConfig >> readFile configPath
  in MaybeT $ parseConfig <$> config

parseConfig :: String -> Maybe (Map.Map String String)
parseConfig config =
  let parsed = do
        line <- lines config
        case line of
          (all isSpace -> True) -> []
          ('#':_) -> []
          l -> return $ drop 1 <$> break (== '=') l
  in if all ((`elem` buttonNames) . fst) parsed
     then Just $ Map.fromList parsed
     else Nothing

buttonNames :: [String]
buttonNames = [ "Custom"
              , "Lock"
              , "Logout"
              , "Shutdown"
              , "Hibernate"
              , "Suspend"
              , "Restart"
              ]
