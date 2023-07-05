{-# LANGUAGE DeriveAnyClass #-}

module Maestro.Client.Error
  (
    ApiError (..)
  , MaestroError (..)
  , fromServantClientError
  ) where

import           Control.Exception       (Exception)
import           Data.Aeson              (decode)
import           Data.Text               (Text)
import           Deriving.Aeson
import           Maestro.Types.V0.Common (LowerFirst)
import           Network.HTTP.Types
import           Servant.Client

-- | In cases of failure, at times, Maestro returns a JSON object with an error message.
data ApiError = ApiError
  { -- | The error type (corresponding to status code). Optional as it is not always present.
    _apiErrorError   :: !(Maybe Text)
  , -- | The error message.
    _apiErrorMessage :: !Text
  , -- | The HTTP status code. Optional as it is not always present.
    _apiErrorCode    :: !(Maybe Word)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_apiError", LowerFirst]] ApiError

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
fromServantClientError e = case e of
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
  _anyOtherFailure -> ServantClientError e
  where
    withMessage body =
      case decode body of
        Just (ae :: ApiError) -> _apiErrorMessage ae
        Nothing               ->
          case decode body of
            Just (m :: Text) -> m
            Nothing          -> mempty
