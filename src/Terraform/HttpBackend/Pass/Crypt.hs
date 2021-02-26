module Terraform.HttpBackend.Pass.Crypt where

import Data.Text (Text)

class MonadPass m where
  encrypt :: Text -> Text -> m ()
  decrypt :: Text -> m Text
  purge :: Text -> m ()
