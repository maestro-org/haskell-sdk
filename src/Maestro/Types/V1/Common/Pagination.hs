-- | Module to define types demanded by cursor based pagination to be used by other types defined in @Maestro.Types.V1@.

module Maestro.Types.V1.Common.Pagination (
    NextCursor (..)
  , HasCursor (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Kind    (Type)
import           Data.String  (IsString)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Servant.API  (FromHttpApiData, ToHttpApiData)

-- | Type to denote for cursor to be returned in a paginated endpoint.
newtype NextCursor = NextCursor Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, IsString)

-- | Is the endpoint paged?
class (Monoid (CursorData a)) => HasCursor a where
  -- | What is the type of the main data in question?
  type CursorData a :: Type
  -- | Get the next cursor from the value of the given type @a@.
  getNextCursor :: a -> Maybe NextCursor
  -- | Get the main data from the value of the given type @a@.
  getCursorData :: a -> CursorData a
