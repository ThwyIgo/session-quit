{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase, ViewPatterns #-}
module Local (getBuilderObj, connectCallbackSymbols, loadConfig, buttonNames) where

import qualified GI.Gtk as Gtk
import Data.GI.Base

import Data.Text (Text)
import qualified Data.Text.IO as T

import Control.Monad
import qualified Data.Map as Map
import Data.Char
import System.Directory
import System.IO
import System.FilePath
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
loadConfig :: FilePath -> IO (Map.Map String String)
loadConfig configPath =
  let defaultConfig = readFile =<< Paths.getDataFileName "resources/defaultConfig.cfg"
  in do
    createDirectoryIfMissing True $ takeDirectory configPath
    doesFileExist configPath >>= flip unless
      (writeFile configPath =<< defaultConfig)

    config <- readFile configPath
    case parseConfig config of
      Right c -> return c
      Left errors -> ioError . userError . unlines $
        map (\(i, msg) -> "Error in line " ++ show i ++ ": " ++ msg) errors

data ParseResult = Parsed (String, String) | Error (Int, String)
isParsed :: ParseResult -> Bool
isParsed (Parsed _) = True
isParsed _ = False

parseConfig :: String -> Either [(Int, String)] (Map.Map String String)
parseConfig config =
  let parsed = do
        line <- zip [1..] $ lines config
        case line of
          (_, all isSpace -> True) -> []
          (_, '#':_) -> []
          (i, elem '=' -> False) -> [Error (i, "Non empty line doesn't contain '='")]
          (i, l) -> case drop 1 <$> break (== '=') l of
            (key@((`elem` buttonNames) -> False), _) ->
              [Error (i, '\"' : key ++ "\" " ++ "isn't a valid key")]
            (_, all isSpace -> True) -> [Error (i, "Empty value")]
            kv -> [Parsed kv]
      failed = filter (not . isParsed) parsed
  in case failed of
    [] -> Right . Map.fromList $ map (\(Parsed x) -> x) parsed
    a -> Left $ map (\(Error x) -> x) a

buttonNames :: [String]
buttonNames = [ "NoBacklight"
              , "Custom"
              , "Lock"
              , "Logout"
              , "Shutdown"
              , "Hibernate"
              , "Suspend"
              , "Restart"
              ]
