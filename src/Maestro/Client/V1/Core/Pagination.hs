module Maestro.Client.V1.Core.Pagination where

import           Data.Default.Class
import           Data.Maybe                         (isNothing)
import           Data.Proxy                         (Proxy (..))
import           Maestro.Types.V1.Common            (IsTimestamped (getTimestampedData),
                                                     TimestampedData)
import           Maestro.Types.V1.Common.Pagination
import           Servant.API                        (QueryParam, (:>))
import           Servant.Client.Core                (Client, HasClient,
                                                     clientWithRoute,
                                                     hoistClientMonad)

--  | Pagination parameters.
data Cursor = Cursor
  { resultPerPage :: !Int                -- ^ Total result to have per page.
  , cursor        :: !(Maybe NextCursor) -- ^ Cursor.
  }

-- | Maximum number of result per page.
maxResultsPerPage :: Int
maxResultsPerPage = 100

instance Default Cursor where
  def = Cursor maxResultsPerPage Nothing

-- Utility for querying all results from a paged endpoint.
allPages :: (Monad m, HasCursor a) => (Cursor -> m a) -> m (TimestampedData a)
allPages act = fetch Nothing
  where
    fetch cursor = do
      xs <- act $ Cursor maxResultsPerPage cursor
      let nextCursor = getNextCursor xs
          cursorData = getTimestampedData xs
      if isNothing nextCursor then
        pure cursorData
      else do
        next <- fetch nextCursor
        pure $ cursorData <> next  -- Note: In case of list, concatenation takes linear time in the number of elements of the first list, thus, `cursorData` should come before.

data Pagination

type PaginationApi api =
     QueryParam "count" Int
  :> QueryParam "cursor" NextCursor
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
