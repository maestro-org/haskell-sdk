-- | Module to query for /"blocks"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/blocks).

module Maestro.Client.V1.Blocks
  ( blockDetailsByHash,
    blockDetailsByHeight,
  ) where

import           Maestro.API.V1              (_blocks)
import           Maestro.API.V1.Blocks
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Maestro.Types.V1
import           Servant.API.Generic
import           Servant.Client

txClient :: MaestroEnv 'V1 -> BlocksAPI (AsClientT IO)
txClient = fromServant . _blocks . apiV1Client

-- | Returns the complete block information given a block hash.
blockDetailsByHash ::
  -- | The Maestro Environment.
  MaestroEnv 'V1 ->
  -- | Block hash in hex.
  BlockHash ->
  IO TimestampedBlockDetails
blockDetailsByHash = _blockByHash . txClient

-- | Returns the complete block information given a block height.
blockDetailsByHeight ::
  -- | The Maestro Environment.
  MaestroEnv 'V1 ->
  -- | Block height.
  BlockHeight ->
  IO TimestampedBlockDetails
blockDetailsByHeight = _blockByHeight . txClient
