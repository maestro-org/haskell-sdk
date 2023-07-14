-- | Common (shared) types used which are not specific to single category of endpoints.

module Maestro.Types.V1.Common
  ( PaymentCredentialAddress,
    StakingCredentialAddress,
    RewardAddress,
    TaggedText (..),
    NonAdaNativeToken (..),
    AssetUnit (..),
    Asset (..),
    v1AssetToV0,
    IsUtxo (..),
    UtxoWithSlot (..),
    v1UtxoWithSlotToV0,
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
import qualified Maestro.Types.V0                    as V0 (Asset (..),
                                                            Utxo (..))
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
  { _assetAmount :: !Integer
  -- ^ Amount of the asset.
  , _assetUnit   :: !AssetUnit
  -- ^ See `AssetUnit`.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_asset", CamelToSnake]] Asset

-- | Convert @V1@ API version `Asset` type into corresponding @V0@ type.
v1AssetToV0 :: Asset -> V0.Asset
v1AssetToV0 Asset {..} = V0.Asset {
    V0._assetQuantity = _assetAmount
  , V0._assetUnit = case _assetUnit of
      Lovelace -> "lovelace"
      UserMintedToken (NonAdaNativeToken policyId tokenName) -> coerce policyId <> "#" <> coerce tokenName
  }

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
  { _utxoWithSlotAddress         :: !(Bech32StringOf Address),
  -- ^ UTxO's address.
    _utxoWithSlotAssets          :: ![Asset],
  -- ^ UTxO's assets.
    _utxoWithSlotDatum           :: !(Maybe DatumOption),
  -- ^ UTxO's datum.
    _utxoWithSlotIndex           :: !TxIndex,
  -- ^ UTxO's transaction index.
    _utxoWithSlotReferenceScript :: !(Maybe Script),
  -- ^ UTxO's script.
    _utxoWithSlotTxHash          :: !TxHash,
  -- ^ UTxO's transaction hash.
    _utxoWithSlotSlot            :: !SlotNo,
  -- ^ Absolute slot of block which produced the UTxO.
    _utxoWithSlotTxoutCbor       :: !(Maybe (HexStringOf TxOutCbor))
  -- ^ Hex encoded transaction output CBOR bytes.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_utxoWithSlot", CamelToSnake]] UtxoWithSlot

instance IsUtxo UtxoWithSlot where
  getAddress = _utxoWithSlotAddress
  getAssets = _utxoWithSlotAssets
  getDatum = _utxoWithSlotDatum
  getTxHash = _utxoWithSlotTxHash
  getIndex = _utxoWithSlotIndex
  getReferenceScript = _utxoWithSlotReferenceScript

-- | Convert @V1@ API version UTxO (with slot) type into corresponding @V0@ type.
v1UtxoWithSlotToV0 :: UtxoWithSlot -> V0.Utxo
v1UtxoWithSlotToV0 UtxoWithSlot {..} = V0.Utxo {
    V0._utxoAddress = _utxoWithSlotAddress
  , V0._utxoAssets = map v1AssetToV0 _utxoWithSlotAssets
  , V0._utxoDatum = _utxoWithSlotDatum
  , V0._utxoIndex = coerce _utxoWithSlotIndex
  , V0._utxoReferenceScript = _utxoWithSlotReferenceScript
  , V0._utxoTxHash = coerce _utxoWithSlotTxHash
  , V0._utxoTxoutCbor = _utxoWithSlotTxoutCbor
  }

-- | A paginated response of transaction outputs.
data PaginatedUtxoWithSlot = PaginatedUtxoWithSlot
  { _paginatedUtxoWithSlotData        :: ![UtxoWithSlot],
  -- ^ List of UTxOs.
    _paginatedUtxoWithSlotLastUpdated :: !LastUpdated,
  -- ^ See `LastUpdated`.
    _paginatedUtxoWithSlotNextCursor  :: !(Maybe NextCursor)
  -- ^ See `NextCursor`
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_paginatedUtxoWithSlot", CamelToSnake]] PaginatedUtxoWithSlot

instance IsTimestamped PaginatedUtxoWithSlot where
  type TimestampedData PaginatedUtxoWithSlot = [UtxoWithSlot]
  getTimestampedData = _paginatedUtxoWithSlotData
  getTimestamp = _paginatedUtxoWithSlotLastUpdated

instance HasCursor PaginatedUtxoWithSlot where
  getNextCursor = _paginatedUtxoWithSlotNextCursor
