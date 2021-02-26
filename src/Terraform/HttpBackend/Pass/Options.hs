{-# LANGUAGE DeriveGeneric #-}

module Terraform.HttpBackend.Pass.Options where

import GHC.Generics (Generic)
import Options.Applicative
import Options.Generic (ParseRecord)

data Options = Options {repositoryPath :: FilePath, port :: Int}
  deriving (Generic)

instance ParseRecord Options
