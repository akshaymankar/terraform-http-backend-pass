{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Terraform.HttpBackend.Pass.Api where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Servant.API.Generic (ToServant, ToServantApi)
import qualified Servant.Server.Generic as Servant
import Terraform.HttpBackend.Pass.App (AppT)
import Terraform.HttpBackend.Pass.Crypt (MonadPass (..))
import Terraform.HttpBackend.Pass.Git (MonadGit (..))

type GetState = "state" :> Capture "name" Text :> Get '[PlainText] Text

type UpdateState = "state" :> Capture "name" Text :> ReqBody '[PlainText] Text :> PostNoContent

type DeleteState = "state" :> Capture "name" Text :> Delete '[PlainText] Text

type Api = GetState :<|> UpdateState :<|> DeleteState

api :: Proxy Api
api = Proxy

server :: (Monad m, MonadPass m, MonadGit m) => ServerT Api m
server =
  getStateImpl
    :<|> updateStateImpl
    :<|> purgeStateImpl

-- TODO: Gracefully return 404 when the file doesn't exist
getStateImpl :: (Monad m, MonadGit m, MonadPass m) => Text -> m Text
getStateImpl name = do
  gitPull
  decrypt (name <> "/terraform.tfstate")

updateStateImpl :: (Monad m, MonadPass m, MonadGit m) => Text -> Text -> m NoContent
updateStateImpl name tfstate = do
  gitPull
  let path = stateFilePath name
  -- Also commits
  encrypt path tfstate

  gitPush
  pure NoContent

purgeStateImpl :: (MonadGit m, MonadPass m, Monad m) => Text -> m Text
purgeStateImpl name = do
  tfstate <- getStateImpl name
  gitPush
  pure tfstate

stateFilePath :: Text -> Text
stateFilePath name = name <> "/terraform.tfstate"
