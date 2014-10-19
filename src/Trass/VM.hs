{-# LANGUAGE TypeFamilies #-}
module Trass.VM where

import Data.List
import Data.Monoid
import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import System.Exit
import System.FilePath
import System.Process
import System.Unix.Directory (withTemporaryDirectory)
import System.Posix.Files
import System.Posix.IO (handleToFd)

import Trass.VM.Config
import Trass.Config.Util

class MonadIO m => MonadVM m where
  data VM m :: *

  withVM        :: MonadIO n => VM m -> m a -> n a
  create        :: FilePath -> TrassConfig -> IO (Maybe (VM m))
  clone         :: FilePath -> VM m -> IO (Maybe (VM m))
  start         :: m Bool
  stop          :: m Bool
  destroy       :: m Bool
  sendFile      :: FilePath -> FilePath -> m (Maybe ExitCode)
  sendDirectory :: FilePath -> FilePath -> m (Maybe ExitCode)
  execute       :: [String] -> Maybe String -> Maybe FilePath -> Command -> m (Maybe ExitCode)

attachMany :: MonadVM m => [String] -> Maybe String -> Maybe FilePath -> Commands -> m (Maybe ExitCode)
attachMany _ _ _ (Commands []) = return (Just ExitSuccess)
attachMany env user wd (Commands (cmd:cmds)) = do
  mc <- execute env user wd cmd
  case mc of
    Just ExitSuccess -> attachMany env user Nothing (Commands cmds)
    _ -> return mc

prepareContainer :: MonadVM m => FilePath -> TrassConfig -> IO (Maybe (VM m))
prepareContainer path cfg@TrassConfig{..} = do
  mvm <- liftIO $ create path cfg
  case mvm of
    Nothing -> return Nothing
    Just vm -> do
      withVM vm $ do
        start
        attachMany env Nothing Nothing (trassUserConfigPrepare trassConfigUser)
        attachMany env user (Just homeDir) trassConfigPrepare
        stop
      return (Just vm)
  where
    user    = Text.unpack <$> trassUserConfigUsername trassConfigUser
    env     = map (Text.unpack . getTextValue) $ getCommands trassConfigEnvironment
    homeDir = fromMaybe "" $ trassUserConfigHome trassConfigUser
    -- env = getCommands (trassConfigEnvironment cfg) <> [TextValue $ "USER=" <> fromMaybe "root" (trassUserConfigUsername trassConfigUser)]

withTempClone :: MonadVM m => VM m -> m a -> IO (Maybe a)
withTempClone baseVM todo = do
  withTemporaryDirectory "trass_vm_clone." $ \tempdir -> do
    setFileMode tempdir accessModes
    mvm <- liftIO $ clone (tempdir </> "vm") baseVM
    case mvm of
      Nothing -> return Nothing
      Just vm -> withVM vm $ do
        start
        x <- todo
        stop
        destroy
        return (Just x)

submit :: MonadVM m => VM m -> FilePath -> [FilePath] -> TrassConfig -> IO (Maybe ExitCode)
submit baseVM submitFile taskDirs TrassConfig{..} = do
  mmcode <- withTempClone baseVM $ do
    -- merge and copy multiple task directories
    forM_ taskDirs $ \taskDir -> sendDirectory taskDir $ taskDir'
    -- copy submitted file
    sendFile submitFile $ taskDir' </> submitFile'

    -- run commands
    mcode <- attachMany env user (Just taskDir') . mconcat . map (fromMaybe mempty) $
                [ trassSubmissionConfigBeforeInstall
                , trassSubmissionConfigInstall
                , trassSubmissionConfigBeforeScript
                , trassSubmissionConfigScript ]

    -- run after* commands
    case mcode of
      Just code -> do
        attachMany env user Nothing . fromMaybe mempty $
          case code of
            ExitSuccess -> trassSubmissionConfigAfterSuccess
            _           -> trassSubmissionConfigAfterFailure
        attachMany env user Nothing $ fromMaybe mempty trassSubmissionConfigAfterScript
      _ -> return Nothing

    return mcode
  return $ join mmcode
  where
    user        = Text.unpack <$> trassUserConfigUsername trassConfigUser
    env         = map (Text.unpack . getTextValue) $ getCommands trassConfigEnvironment
    homeDir     = fromMaybe "" $ trassUserConfigHome trassConfigUser
    taskDir'    = homeDir </> fromMaybe "trass_task_dir" trassConfigTaskDir -- FIXME
    submitFile' = fromMaybe "trass_submit_file" trassSubmissionConfigFile   -- FIXME

    TrassSubmissionConfig{..} = trassConfigSubmission

tryVM :: MonadVM m => VM m -> TrassConfig -> IO (Maybe ExitCode)
tryVM baseVM TrassConfig{..} = join <$> do
  withTempClone baseVM $ do
    execute env user Nothing (TextValue "sh")
  where
    user = Text.unpack <$> trassUserConfigUsername trassConfigUser
    env  = map (Text.unpack . getTextValue) $ getCommands trassConfigEnvironment

