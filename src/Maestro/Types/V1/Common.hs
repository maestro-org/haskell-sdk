-- | Common (shared) types used which are not specific to single category of endpoints.

module Maestro.Types.V1.Common
  ( PaymentCredentialAddress,
    StakingCredentialAddress,
    RewardAddress,
    PoolId,
    TaggedText (..),
    NonAdaNativeToken (..),
    AssetUnit (..),
    Asset (..),
    IsUtxo (..),
    UtxoWithSlot (..),
    PaginatedUtxoWithSlot (..),
    module Maestro.Types.Common,
    module Maestro.Types.V1.Common.Pagination,
    module Maestro.Types.V1.Common.Timestamped
  ) where

import           Data.Aeson                          (FromJSON (..),
                                                      ToJSON (..), Value (..),
                                                      withText)
import           Data.Coerce                         (coerce)
import           Data.String                         (IsString)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (splitAt)
import           Deriving.Aeson
import           GHC.TypeLits                        (Symbol)
import           Maestro.Types.Common
import           Maestro.Types.V1.Common.Pagination
import           Maestro.Types.V1.Common.Timestamped
import           Servant.API                         (FromHttpApiData (..),
                                                      ToHttpApiData (..))

-- | Phantom datatype to be used with, say `Bech32StringOf` to represent Bech32 representation of payment credential of an address.
data PaymentCredentialAddress

-- | Phantom datatype to be used with, say `Bech32StringOf` to represent Bech32 representation of staking credential of an address.
data StakingCredentialAddress

-- | Phantom datatype to be used with, say `Bech32StringOf` to represent Bech32 representation of stake address (See [CIP-19](https://cips.cardano.org/cips/cip19/) for more details).
data RewardAddress

-- | Phantom datatype to be used with, say `Bech32StringOf` to represent Bech32 representation of a pool id.
data PoolId

-- | Wrapper around `Text` type with mentioned description of it.
newtype TaggedText (description :: Symbol) = TaggedText Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, IsString)

-- | Type to denote for native tokens (besides ada).
data NonAdaNativeToken = NonAdaNativeToken !PolicyId !TokenName
  deriving stock (Eq, Ord, Show)

instance ToHttpApiData NonAdaNativeToken where
  toUrlPiece (NonAdaNativeToken policyId tokenName) = coerce policyId <> coerce tokenName

-- | Given asset name is either /lovelace/ or concatenation of hex encoded policy ID and hex encoded asset name for native asset.
data AssetUnit = Lovelace
               -- ^ Lovelace.
               | UserMintedToken !NonAdaNativeToken
               -- ^ For non-ada native-tokens.
  deriving stock (Eq, Ord)

instance Show AssetUnit where
  show Lovelace                            = "lovelace"
  show (UserMintedToken nonAdaNativeToken) = show nonAdaNativeToken

instance FromJSON AssetUnit where
  parseJSON = withText "AssetUnit" $ \t ->
    if t == "lovelace" then pure Lovelace
    else
      let (policyId, tokenName) = T.splitAt 56 t
      in pure $ UserMintedToken $ NonAdaNativeToken (coerce policyId) (coerce tokenName)

instance ToJSON AssetUnit where
  toJSON Lovelace                  = String "lovelace"
  toJSON (UserMintedToken nonAdaNativeToken) = String $ toUrlPiece nonAdaNativeToken

-- | Representation of asset in an UTxO.
data Asset = Asset
  { assetAmount :: !Integer
  -- ^ Amount of the asset.
  , assetUnit   :: !AssetUnit
  -- ^ See `AssetUnit`.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "asset", CamelToSnake]] Asset

-- | To get basic details from an UTxO.
class IsUtxo a where
  getAddress :: a -> Bech32StringOf Address
  getAssets :: a -> [Asset]
  getDatum :: a -> Maybe DatumOption
  getTxHash :: a -> TxHash
  getIndex :: a -> TxIndex
  getReferenceScript :: a -> Maybe Script

-- | Transaction output.
data UtxoWithSlot = UtxoWithSlot
  { utxoWithSlotAddress         :: !(Bech32StringOf Address),
  -- ^ UTxO's address.
    utxoWithSlotAssets          :: ![Asset],
  -- ^ UTxO's assets.
    utxoWithSlotDatum           :: !(Maybe DatumOption),
  -- ^ UTxO's datum.
    utxoWithSlotIndex           :: !TxIndex,
  -- ^ UTxO's transaction index.
    utxoWithSlotReferenceScript :: !(Maybe Script),
  -- ^ UTxO's script.
    utxoWithSlotTxHash          :: !TxHash,
  -- ^ UTxO's transaction hash.
    utxoWithSlotSlot            :: !SlotNo,
  -- ^ Absolute slot of block which produced the UTxO.
    utxoWithSlotTxoutCbor       :: !(Maybe (HexStringOf TxOutCbor))
  -- ^ Hex encoded transaction output CBOR bytes.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "utxoWithSlot", CamelToSnake]] UtxoWithSlot

instance IsUtxo UtxoWithSlot where
  getAddress = utxoWithSlotAddress
  getAssets = utxoWithSlotAssets
  getDatum = utxoWithSlotDatum
  getTxHash = utxoWithSlotTxHash
  getIndex = utxoWithSlotIndex
  getReferenceScript = utxoWithSlotReferenceScript

-- | A paginated response of transaction outputs.
data PaginatedUtxoWithSlot = PaginatedUtxoWithSlot
  { paginatedUtxoWithSlotData        :: ![UtxoWithSlot],
  -- ^ List of UTxOs.
    paginatedUtxoWithSlotLastUpdated :: !LastUpdated,
  -- ^ See `LastUpdated`.
    paginatedUtxoWithSlotNextCursor  :: !(Maybe NextCursor)
  -- ^ See `NextCursor`
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "paginatedUtxoWithSlot", CamelToSnake]] PaginatedUtxoWithSlot

instance IsTimestamped PaginatedUtxoWithSlot where
  type TimestampedData PaginatedUtxoWithSlot = [UtxoWithSlot]
  getTimestampedData = paginatedUtxoWithSlotData
  getTimestamp = paginatedUtxoWithSlotLastUpdated

instance HasCursor PaginatedUtxoWithSlot where
  getNextCursor = paginatedUtxoWithSlotNextCursor
