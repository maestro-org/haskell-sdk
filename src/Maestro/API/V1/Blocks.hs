module Maestro.API.V1.Blocks where

import           Maestro.Types.V1
import           Servant.API
import           Servant.API.Generic

data BlocksAPI route = BlocksAPI
  { -- | Get details of the specified block by hash
    blockByHash
      :: route
      :- Capture "hash_or_height" BlockHash
      :> Get '[JSON] TimestampedBlockDetails
  -- | Get details of the specified block by height
  , blockByHeight
      :: route
      :- Capture "hash_or_height" BlockHeight
      :> Get '[JSON] TimestampedBlockDetails
  }
  deriving (Generic)
