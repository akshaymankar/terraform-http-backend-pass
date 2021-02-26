{-# LANGUAGE RecordWildCards #-}
module Terraform.HttpBackend.Pass.Env where

import Terraform.HttpBackend.Pass.Options (Options(..))
newtype Env = Env { directory :: FilePath}

mkEnv :: Options -> Env
mkEnv Options {..}=  Env { directory = repositoryPath }
