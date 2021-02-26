{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Terraform.HttpBackend.Pass.Api where

import Control.Monad (unless)
import Data.Aeson (ToJSON (..), Value, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import GHC.Generics (Generic)
import Servant
import Servant.API.Generic (ToServant, ToServantApi)
import qualified Servant.Server.Generic as Servant
import Terraform.HttpBackend.Pass.App (AppT)
import Terraform.HttpBackend.Pass.Crypt (MonadPass (..))
import Terraform.HttpBackend.Pass.Git (MonadGit (..))

data StateNotFound = StateNotFound

instance HasStatus StateNotFound where
  type StatusOf StateNotFound = 404

instance ToJSON StateNotFound where
  toJSON _ = Aeson.object ["error" .= ("state not found" :: Text)]

newtype StateCorrupt = StateCorrupt {err :: String}

instance HasStatus StateCorrupt where
  type StatusOf StateCorrupt = 500

instance ToJSON StateCorrupt where
  toJSON (StateCorrupt err) =
    Aeson.object
      [ "error" .= ("state corrupt" :: Text),
        "message" .= err
      ]

type GetResponse = '[WithStatus 200 Value, StateNotFound, StateCorrupt]

type GetState = "state" :> Capture "name" Text :> UVerb 'GET '[JSON] GetResponse

type UpdateState = "state" :> Capture "name" Text :> ReqBody '[JSON] Value :> PostNoContent

type DeleteState = "state" :> Capture "name" Text :> DeleteNoContent

type Api = GetState :<|> UpdateState :<|> DeleteState

api :: Proxy Api
api = Proxy

server :: (Monad m, MonadPass m, MonadGit m) => ServerT Api m
server =
  getStateImpl
    :<|> updateStateImpl
    :<|> purgeStateImpl

getStateImpl :: (Monad m, MonadGit m, MonadPass m) => Text -> m (Union GetResponse)
getStateImpl name = do
  gitPull
  let path = stateFilePath name
  stateExists <- exists path
  if stateExists
    then do
      eitherState <-
        Aeson.eitherDecode @Value . LBS.fromStrict . Text.encodeUtf8
          <$> decrypt path
      case eitherState of
        Left err -> respond $ StateCorrupt err
        Right state -> respond (WithStatus @200 state)
    else respond StateNotFound

updateStateImpl :: (Monad m, MonadPass m, MonadGit m) => Text -> Value -> m NoContent
updateStateImpl name tfstate = do
  gitPull
  let path = stateFilePath name
  -- Also commits
  encrypt path $ LText.toStrict $ Aeson.encodeToLazyText tfstate

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
