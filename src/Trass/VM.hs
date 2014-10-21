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

data VMStatus
  = VMCreating
  | VMStarting
  | VMPreparingUser
  | VMPreparing
  | VMStopping
  | VMFailed
  | VMReady
  deriving (Read, Show)

data CloneStatus
  = CloneCloning
  | CloneStarting
  | CloneExecuting
  | CloneStopping
  | CloneDestroying
  | CloneFailed
  | CloneFinished
  deriving (Show, Read)

data SubmissionStatus
  = SubmissionCopying
  | SubmissionRunningBeforeInstall
  | SubmissionRunningInstall
  | SubmissionRunningBeforeScript
  | SubmissionRunningScript
  | SubmissionRunningAfterFailure
  | SubmissionRunningAfterSuccess
  | SubmissionRunningAfterScript
  | SubmissionFinished
  deriving (Show, Read)

attachMany :: MonadVM m => [String] -> Maybe String -> Maybe FilePath -> Commands -> m (Maybe ExitCode)
attachMany _ _ _ (Commands []) = return (Just ExitSuccess)
attachMany env user wd (Commands (cmd:cmds)) = do
  mc <- execute env user wd cmd
  case mc of
    Just ExitSuccess -> attachMany env user Nothing (Commands cmds)
    _ -> return mc

prepareContainer :: MonadVM m => FilePath -> TrassConfig -> (VMStatus -> IO ()) -> IO (Maybe (VM m))
prepareContainer path cfg@TrassConfig{..} handler = do
  handler VMCreating
  mvm <- liftIO $ create path cfg
  case mvm of
    Nothing -> do
      handler VMFailed
      return Nothing
    Just vm -> do
      handler VMStarting
      withVM vm $ do
        start
        liftIO $ handler VMPreparingUser
        attachMany env Nothing Nothing (trassUserConfigPrepare trassConfigUser)
        liftIO $ handler VMPreparing
        attachMany env user (Just homeDir) trassConfigPrepare
        liftIO $ handler VMStopping
        stop
      handler VMReady
      return (Just vm)
  where
    user    = Text.unpack <$> trassUserConfigUsername trassConfigUser
    env     = map (Text.unpack . getTextValue) $ getCommands trassConfigEnvironment
    homeDir = fromMaybe "" $ trassUserConfigHome trassConfigUser
    -- env = getCommands (trassConfigEnvironment cfg) <> [TextValue $ "USER=" <> fromMaybe "root" (trassUserConfigUsername trassConfigUser)]

withTempClone :: MonadVM m => VM m -> (CloneStatus -> IO ()) -> m a -> IO (Maybe a)
withTempClone baseVM handler todo = do
  handler CloneCloning
  withTemporaryDirectory "trass_vm_clone." $ \tempdir -> do
    setFileMode tempdir accessModes
    mvm <- liftIO $ clone (tempdir </> "vm") baseVM
    case mvm of
      Nothing -> do
        handler CloneFailed
        return Nothing
      Just vm -> withVM vm $ do
        liftIO $ handler CloneStarting
        start
        liftIO $ handler CloneExecuting
        x <- todo
        liftIO $ handler CloneStopping
        stop
        liftIO $ handler CloneDestroying
        destroy
        liftIO $ handler CloneFinished
        return (Just x)

submit :: MonadVM m => VM m -> FilePath -> [FilePath] -> TrassConfig -> (Either CloneStatus SubmissionStatus -> IO ()) -> IO (Maybe ExitCode)
submit baseVM submitFile taskDirs TrassConfig{..} handler = do
  mmcode <- withTempClone baseVM (handler . Left) $ do
    liftIO $ handler $ Right SubmissionCopying
    -- merge and copy multiple task directories
    forM_ taskDirs $ \taskDir -> sendDirectory taskDir $ taskDir'
    -- copy submitted file
    sendFile submitFile $ taskDir' </> submitFile'

    -- FIXME
    liftIO $ handler $ Right SubmissionRunningBeforeInstall
    -- run commands
    mcode <- attachMany' (Just taskDir') $
                [ (SubmissionRunningBeforeInstall,  trassSubmissionConfigBeforeInstall)
                , (SubmissionRunningInstall,        trassSubmissionConfigInstall)
                , (SubmissionRunningBeforeScript,   trassSubmissionConfigBeforeScript)
                , (SubmissionRunningScript,         trassSubmissionConfigScript) ]

    -- run after* commands
    case mcode of
      Just code -> do
        attachMany' Nothing $
          case code of
            ExitSuccess -> [ (SubmissionRunningAfterSuccess, trassSubmissionConfigAfterSuccess) ]
            _           -> [ (SubmissionRunningAfterFailure, trassSubmissionConfigAfterFailure) ]
        attachMany' Nothing $ [ (SubmissionRunningAfterScript, trassSubmissionConfigAfterScript) ]
      _ -> return Nothing

    return mcode
  return $ join mmcode
  where
    attachMany' dir []            = attachMany env user dir mempty
    attachMany' dir ((s, cs):css) = do
      liftIO $ handler $ Right s
      mcode <- attachMany env user dir (fromMaybe mempty cs)
      case mcode of
        Just ExitSuccess -> attachMany' dir css
        _ -> return mcode

    user        = Text.unpack <$> trassUserConfigUsername trassConfigUser
    env         = map (Text.unpack . getTextValue) $ getCommands trassConfigEnvironment
    homeDir     = fromMaybe "" $ trassUserConfigHome trassConfigUser
    taskDir'    = homeDir </> fromMaybe "trass_task_dir" trassConfigTaskDir -- FIXME
    submitFile' = fromMaybe "trass_submit_file" trassSubmissionConfigFile   -- FIXME

    TrassSubmissionConfig{..} = trassConfigSubmission

tryVM :: MonadVM m => VM m -> TrassConfig -> IO (Maybe ExitCode)
tryVM baseVM TrassConfig{..} = join <$> do
  withTempClone baseVM print $ do
    execute env user Nothing (TextValue "sh")
  where
    user = Text.unpack <$> trassUserConfigUsername trassConfigUser
    env  = map (Text.unpack . getTextValue) $ getCommands trassConfigEnvironment

