module Maestro.Client.Transaction
  ( submitAndMonitorTx
  , submitTx
  , txCbor
  , txAddress
  , txUtxo
  ) where

import qualified Data.ByteString         as BS
import           Data.Text               (Text)
import           GHC.Natural             (Natural)
import           Maestro.API             (_tx)
import           Maestro.API.Transaction
import           Maestro.Client
import           Maestro.Client.Env
import           Maestro.Types.Common
import           Servant.API.Generic
import           Servant.Client

type CborEncodedByte = BS.ByteString
data Tx
type TxIndex = Natural

type ResolveDatum = Bool
type WithCbor = Bool

--
txClient :: MaestroEnv -> TxAPI (AsClientT IO)
txClient = fromServant . _tx . apiClient

-- |
-- Submit a signed and serialized transaction to the network.
-- A transaction submited with this endpoint will be monitored by Maestro.
--
submitAndMonitorTx :: MaestroEnv      -- ^ The Maestro Environment
                   -> CborEncodedByte -- ^ CBOR encoded Transaction
                   -> IO Text
submitAndMonitorTx = _monitorTxSubmit . txClient

-- |
-- Submit a signed and serialized transaction to the network.
-- Interaction with this endpoint is identical to IOG's Cardano Submit API and will not be monitored by Maestro.
--
submitTx :: MaestroEnv        -- ^ The Maestro Environment
         -> CborEncodedByte   -- ^ CBOR encoded Transaction
         -> IO Text
submitTx  = _submitTx . txClient

-- |
-- Returns hex-encoded CBOR bytes of a transaction
--
txCbor :: MaestroEnv  -- ^ The Maestro Environment
       -> (HashStringOf Tx)      -- ^ Hex Encoded Transaction Hash
       -> IO TxCbor   -- ^ hex-encoded CBOR bytes of a transaction
txCbor = _txCborApi . txClient

-- |
-- Returns the address specified in the given transaction output
--
txAddress :: MaestroEnv     -- ^ The Maestro Environment
          -> (HashStringOf Tx)         -- ^ Hex Encoded Transaction Hash
          -> TxIndex        -- ^ The Transaction Output Index
          -> IO TxAddress
txAddress = _txAddressApi . txClient

-- |
-- Returns the specified transaction output of a transaction output reference
--
txUtxo :: MaestroEnv            -- ^ The MaestroEnv
       -> (HashStringOf Tx)                -- ^ Hex encoded transaction hash
       -> TxIndex               -- ^ The Transaction Output Index
       -> Maybe ResolveDatum    -- ^ Try find and include the corresponding datums for datum hashes
       -> Maybe WithCbor        -- ^ Include the CBOR encodings of the transaction outputs in the response
       -> IO Utxo
txUtxo = _txUtxo . txClient
