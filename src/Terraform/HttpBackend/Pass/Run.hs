{-# LANGUAGE OverloadedStrings #-}

module Terraform.HttpBackend.Pass.Run where

import Data.Function ((&))
import qualified Network.Wai.Handler.Warp as Warp
import Options.Generic
import qualified Servant.Server as Servant
import qualified Terraform.HttpBackend.Pass.Api as Api
import Terraform.HttpBackend.Pass.App (runAppT)
import Terraform.HttpBackend.Pass.Env (Env, mkEnv)
import qualified Terraform.HttpBackend.Pass.Options as Options

run :: IO ()
run = do
  opts <- getRecord "Terraform HTTP Backend using Pass and Git"
  let env = mkEnv opts
  Warp.run (Options.port opts) (Servant.serve Api.api (hoistServer env))

hoistServer :: Env -> Servant.ServerT Api.Api Servant.Handler
hoistServer env = Servant.hoistServer Api.api (runAppT env) Api.server
