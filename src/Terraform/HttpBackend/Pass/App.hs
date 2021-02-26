{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Terraform.HttpBackend.Pass.App where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Text (Text)
import qualified Data.Text as Text
import Shelly (shelly)
import qualified Shelly
import System.Directory (doesFileExist)
import Terraform.HttpBackend.Pass.Crypt (MonadPass (..))
import Terraform.HttpBackend.Pass.Env (Env (..))
import Terraform.HttpBackend.Pass.Git (MonadGit (..))

newtype AppT m a = AppT {unAppT :: ReaderT Env m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

instance MonadIO m => MonadGit (AppT m) where
  gitAdd path = runGit_ ["add", path]
  gitCommit message = runGit_ ["commit", "-m", message]
  gitPush = runGit_ ["push"]
  gitPull = runGit_ ["pull", "--rebase"]
  gitRm path = runGit_ ["rm", path]

runGit_ :: (MonadIO m, MonadReader Env m) => [Text] -> m ()
runGit_ args = do
  Env {..} <- ask
  shelly $ Shelly.run_ "git" (["-C", Text.pack directory] ++ args)

instance (Monad m, MonadIO m) => MonadPass (AppT m) where
  encrypt name secret = do
    Env {..} <- ask
    shelly $ do
      Shelly.setenv "PASSWORD_STORE_DIR" (Text.pack directory)
      Shelly.setStdin secret
      Shelly.run_ "pass" ["insert", "-m", name]
  decrypt name = do
    Env {..} <- ask
    shelly $ do
      Shelly.setenv "PASSWORD_STORE_DIR" (Text.pack directory)
      Shelly.run "pass" [name]
  purge name = do
    Env {..} <- ask
    shelly $ do
      Shelly.setenv "PASSWORD_STORE_DIR" (Text.pack directory)
      Shelly.run_ "pass" ["rm", name]
  exists name = do
    Env {..} <- ask
    liftIO $ doesFileExist (directory <> "/" <> Text.unpack name)

runAppT :: Env -> AppT m a -> m a
runAppT env (AppT r) = runReaderT r env
