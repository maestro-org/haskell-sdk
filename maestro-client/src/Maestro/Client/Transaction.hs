module Maestro.Client.Transaction where

import           Data.Text               (Text)
import           Maestro.API             (_tx)
import           Maestro.API.Transaction
import           Maestro.Client
import           Maestro.Client.Env
import           Servant.API
import           Servant.Client

type CborEncodedText = Text
type TxHash = Text
type TxIndex = Int

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
                   -> CborEncodedText -- ^ CBOR encoded Transaction
                   -> IO Text
submitAndMonitorTx = _monitorTxSubmit . txClient

-- |
-- Submit a signed and serialized transaction to the network.
-- Interaction with this endpoint is identical to IOG's Cardano Submit API and will not be monitored by Maestro.
--
submitTx :: MaestroEnv        -- ^ The Maestro Environment
         -> CborEncodedText   -- ^ CBOR encoded Transaction
         -> IO Text
submitTx  = _submitTx . txClient

-- |
-- Returns hex-encoded CBOR bytes of a transaction
--
txCbor :: MaestroEnv  -- ^ The Maestro Environment
       -> TxHash      -- ^ Hex Encoded Transaction Hash
       -> IO TxCbor   -- ^ hex-encoded CBOR bytes of a transaction
txCbor = _txCbor . txClient

-- |
-- Returns the address specified in the given transaction output
--
txAddress :: MaestroEnv     -- ^ The Maestro Environment
          -> TxHash         -- ^ Hex Encoded Transaction Hash
          -> TxIndex        -- ^ The Transaction Output Index
          -> IO TxAddress
txAddress = _txAddress . txClient

-- |
-- Returns the specified transaction output of a transaction output reference
--
txUtxo :: MaestroEnv            -- ^ The MaestroEnv
       -> TxHash                -- ^ Hex encoded transaction hash
       -> TxIndex               -- ^ The Transaction Output Index
       -> Maybe ResolveDatum    -- ^ Try find and include the corresponding datums for datum hashes
       -> Maybe WithCbor        -- ^ Include the CBOR encodings of the transaction outputs in the response
       -> IO Utxo
txUtxo = _txUtxo . txClient
