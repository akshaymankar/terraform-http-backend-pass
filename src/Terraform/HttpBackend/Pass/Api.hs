{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Terraform.HttpBackend.Pass.Api where

import Control.Monad (unless)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Servant.API.Generic (ToServant, ToServantApi)
import qualified Servant.Server.Generic as Servant
import Terraform.HttpBackend.Pass.App (AppT)
import Terraform.HttpBackend.Pass.Crypt (MonadPass (..))
import Terraform.HttpBackend.Pass.Git (MonadGit (..))

type GetState = "state" :> Capture "name" Text :> UVerb 'GET '[PlainText] [WithStatus 200 Text, WithStatus 404 Text]

type UpdateState = "state" :> Capture "name" Text :> ReqBody '[PlainText] Text :> PostNoContent

type DeleteState = "state" :> Capture "name" Text :> DeleteNoContent

type Api = GetState :<|> UpdateState :<|> DeleteState

api :: Proxy Api
api = Proxy

server :: (Monad m, MonadPass m, MonadGit m) => ServerT Api m
server =
  getStateImpl
    :<|> updateStateImpl
    :<|> purgeStateImpl

getStateImpl :: (Monad m, MonadGit m, MonadPass m) => Text -> m (Union '[WithStatus 200 Text, WithStatus 404 Text])
getStateImpl name = do
  gitPull
  let path = stateFilePath name
  stateExists <- exists path
  if stateExists
    then respond =<< (WithStatus @200 <$> decrypt path)
    else respond (WithStatus @404 ("Not found!" :: Text))

updateStateImpl :: (Monad m, MonadPass m, MonadGit m) => Text -> Text -> m NoContent
updateStateImpl name tfstate = do
  gitPull
  let path = stateFilePath name
  -- Also commits
  encrypt path tfstate

  gitPush
  pure NoContent

purgeStateImpl :: (MonadGit m, MonadPass m, Monad m) => Text -> m NoContent
purgeStateImpl name = do
  gitPull
  let path = stateFilePath name
  stateExists <- exists path
  unless stateExists $ do
    purge (stateFilePath name)
    gitPush
  pure NoContent

stateFilePath :: Text -> Text
stateFilePath name = name <> "/terraform.tfstate"
