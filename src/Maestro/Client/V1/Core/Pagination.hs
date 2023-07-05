module Maestro.Client.V1.Core.Pagination where

import           Data.Default.Class
import           Data.Kind           (Type)
import           Data.Maybe          (isNothing)
import           Data.Proxy          (Proxy (..))
import           Data.Text           (Text)
import           Servant.API         (QueryParam, (:>))
import           Servant.Client.Core (Client, HasClient, clientWithRoute,
                                      hoistClientMonad)

--  | Pagination parameters.
data Cursor = Cursor
  { resultPerPage :: !Int          -- ^ Total result to have per page.
  , cursor        :: !(Maybe Text) -- ^ Cursor.
  }

-- | Maximum number of result per page.
maxResultsPerPage :: Int
maxResultsPerPage = 100

instance Default Cursor where
  def = Cursor maxResultsPerPage Nothing

-- | Is the endpoint paged?
class (Monoid (CursorData a)) => HasCursor a where
  type CursorData a :: Type
  getNextCursor :: a -> Maybe Text
  getCursorData :: a -> CursorData a

-- Utility for querying all results from a paged endpoint.
allPages :: (Monad m, HasCursor a) => (Cursor -> m a) -> m (CursorData a)
allPages act = fetch Nothing
  where
    fetch cursor = do
      xs <- act $ Cursor maxResultsPerPage cursor
      let nextCursor = getNextCursor xs
          cursorData = getCursorData xs
      if isNothing nextCursor then
        pure cursorData
      else do
        next <- fetch nextCursor
        pure $ cursorData <> next  -- Note: In case of list, concatenation takes linear time in the number of elements of the first list, thus, `cursorData` should come before.

data Pagination

type PaginationApi api =
     QueryParam "count" Int
  :> QueryParam "cursor" Text
  :> api

instance HasClient m api => HasClient m (Pagination :> api) where
  type Client m (Pagination :> api) = Cursor -> Client m api

  clientWithRoute pm _ req Cursor {..} =
    clientWithRoute
      pm
      (Proxy @(PaginationApi api))
      req
      (Just resultPerPage)
      cursor

  hoistClientMonad pm _pa hst subClient = hoistClientMonad pm (Proxy @api) hst . subClient
