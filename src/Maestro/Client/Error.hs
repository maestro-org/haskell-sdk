{-# LANGUAGE DeriveAnyClass #-}

module Maestro.Client.Error
  (
    ApiError (..)
  , MaestroError (..)
  , fromServantClientError
  ) where

import           Control.Exception    (Exception (..), SomeException (..))
import           Data.Aeson           (decode)
import           Data.ByteString      (toStrict)
import           Data.Either          (fromRight)
import           Data.Function        ((&))
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8')
import           Deriving.Aeson
import           Maestro.Types.Common (LowerFirst)
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Types
import           Servant.Client

-- | In cases of failure, at times, Maestro returns a JSON object with an error message.
data ApiError = ApiError
  { -- | The error type (corresponding to status code). Optional as it is not always present.
    apiErrorError   :: !(Maybe Text)
  , -- | The error message.
    apiErrorMessage :: !Text
  , -- | The HTTP status code. Optional as it is not always present.
    apiErrorCode    :: !(Maybe Word)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "apiError", LowerFirst]] ApiError

-- | Maestro errors.
data MaestroError =
    MaestroError !Text
    -- ^ Other Maestro error.
  | MaestroBadRequest !Text
    -- ^ 400 status code.
  | MaestroApiKeyMissing !Text
    -- ^ 401 status code.
  | MaestroNotFound
    -- ^ 404 status code.
  | MaestroUnsupportedMediaType
    -- ^ 415 status code.
  | MaestroUsageLimitReached
    -- ^ 429 status code.
  | MaestroInternalError !Text
    -- ^ 500 status code.
  | ServantClientError !ClientError
    -- ^ When `ClientError` is not of form `FailureResponse`.
  deriving (Eq, Show, Exception)

fromServantClientError :: ClientError -> MaestroError
fromServantClientError e = let sce = ServantClientError e in case e of
  FailureResponse _bUrl (Response s _ _ body)
    | s == status400 ->
        MaestroBadRequest (withMessage body)
    | s == status401 ->
        MaestroApiKeyMissing (withMessage body)
    | s == status404 ->
        MaestroNotFound
    | s == status415 ->
        MaestroUnsupportedMediaType
    | s == status429 ->
        MaestroUsageLimitReached
    | s == status500 ->
        MaestroInternalError (withMessage body)
    | otherwise ->
        MaestroError (withMessage body)
  ConnectionError se -> case fromException @Client.HttpException se of
    Just he -> case he of
      Client.HttpExceptionRequest req content -> ServantClientError $ ConnectionError $ SomeException $ Client.HttpExceptionRequest req { Client.requestHeaders = (\(h, v) -> if h == "api-key" then (h, "hidden") else (h, v)) <$> Client.requestHeaders req } content
      _anyOther -> sce
    Nothing -> sce
  _anyOtherFailure -> sce
  where
    withMessage body =
      case decode body of
        Just (ae :: ApiError) -> apiErrorMessage ae
        Nothing               ->
          case decode body of
            Just (m :: Text) -> m
            Nothing          -> toStrict body & decodeUtf8' & fromRight mempty
