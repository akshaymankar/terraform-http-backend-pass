module Terraform.HttpBackend.Pass.Git where

import Data.Text (Text)

class MonadGit m where
  gitAdd :: Text -> m ()
  gitCommit :: Text -> m ()
  gitPush :: m ()
  gitPull :: m ()
  gitRm :: Text -> m ()
