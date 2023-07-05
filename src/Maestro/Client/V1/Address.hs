module Maestro.Client.V1.Address where

import           Data.Text              (Text)
import           Maestro.API.V1
import           Maestro.API.V1.Address
import           Maestro.Client.Env
import           Maestro.Client.V1.Core
import           Maestro.Types.V1
import           Servant.API.Generic
import           Servant.Client

addressClient :: MaestroEnv -> AddressAPI (AsClientT IO)
addressClient = fromServant . _address . apiV1Client

-- | Returns list of utxos for multiple addresses
utxosAtMultiAddresses ::
  -- | The Maestro Environment
  MaestroEnv ->
  -- | Query param to include the corresponding datums for datum hashes
  Maybe Bool ->
  -- | Query Param to include the CBOR encodings of the transaction outputs in the response
  Maybe Bool ->
  -- | The pagination attributes
  Cursor ->
  -- | List of Address in bech32 format to fetch utxo from
  [Text] ->
  IO Utxos
utxosAtMultiAddresses = _addressesUtxos . addressClient
