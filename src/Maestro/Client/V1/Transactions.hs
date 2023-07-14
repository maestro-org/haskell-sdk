-- | Module to query for /"transactions"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/transactions).

module Maestro.Client.V1.Transactions
  ( outputsByReferences,
  ) where

import           Maestro.API.V1              (_transactions)
import           Maestro.API.V1.Transactions
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Maestro.Types.V1
import           Servant.API.Generic
import           Servant.Client

txClient :: MaestroEnv 'V1 -> TransactionsAPI (AsClientT IO)
txClient = fromServant . _transactions . apiV1Client

-- | Returns outputs for given output references.
outputsByReferences ::
  -- | The Maestro Environment.
  MaestroEnv 'V1 ->
  -- | Try find and include the corresponding datums for datum hashes.
  Maybe Bool ->
  -- | Include the CBOR encodings of the transaction outputs in the response.
  Maybe Bool ->
  -- | The pagination attributes.
  Cursor ->
  -- | Output references.
  [OutputReference] ->
  IO PaginatedUtxo
outputsByReferences = _txOutputs . txClient
