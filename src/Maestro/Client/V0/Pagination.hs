{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Maestro.Client.V0.Pagination where

import           Data.Default.Class
import           Data.Proxy          (Proxy (..))
import           Servant.API         (QueryParam, (:>))
import           Servant.Client.Core (Client, HasClient, clientWithRoute,
                                      hoistClientMonad)

--  | Pagination parameters
data Page = Page
  { resultPerPage :: !Int -- ^  Total result per page
  , pageNumber    :: !Int  -- ^ Page Number
  }

-- | maximum number of result
maxPageResult :: Int
maxPageResult = 100

instance Default Page where
  def = Page maxPageResult 1

page :: Int -> Page
page n
  | n >= 1 = Page maxPageResult n
  | otherwise = error "Page number not in range [1..]"

-- Utility for querying all results from a paged endpoint.
allPages :: (Monad m, Foldable t, Monoid (t a)) => (Page -> m (t a)) -> m (t a)
allPages act = fetch 1
  where
    fetch pageNo = do
      xs <- act $ Page maxPageResult pageNo
      if length xs < maxPageResult then
        pure xs
      else do
        next <- fetch $ pageNo + 1
        pure $ xs <> next  -- Note: In case of list, concatenation takes linear time in the number of elements of the first list, thus, `xs` should come before.

data Pagination

type PaginationApi api =
  QueryParam "count" Int
  :> QueryParam "page"  Int
  :> api

instance HasClient m api => HasClient m (Pagination :> api) where
  type Client m (Pagination :> api) = Page -> Client m api

  clientWithRoute pm _ req Page{..} =
    clientWithRoute
      pm
      (Proxy @(PaginationApi api))
      req
      (Just resultPerPage)
      (Just pageNumber)

  hoistClientMonad pm _pa hst subClient = hoistClientMonad pm (Proxy @api)  hst . subClient
