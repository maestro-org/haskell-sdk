{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Maestro.Util.Pagination where

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


page :: Int -> Page
page n
  | n >= 1 = Page maxPageResult n
  | otherwise = error "Page number not in range [1..]"


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
