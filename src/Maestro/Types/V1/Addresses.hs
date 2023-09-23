-- | Module to define types for /\"Addresses\"/ category of endpoints defined at [docs.gomaestro.org](https://docs.gomaestro.org/docs/category/addresses).

module Maestro.Types.V1.Addresses (
    AddressToDecode,
    NetworkId (..),
    PaymentCredKind (..),
    PaymentCredential (..),
    StakingCredKind (..),
    CertIndex (..),
    ChainPointer (..),
    StakingCredential (..),
    AddressInfo (..),
    OutputReferenceObject (..),
    PaginatedOutputReferenceObject (..),
  ) where

import           Deriving.Aeson
import           GHC.Natural             (Natural)
import           Maestro.Types.V1.Common
import           Servant.API             (FromHttpApiData, ToHttpApiData)

-- | Address to decode. Given address should be in either Bech32 or Hex or Base58 format. Base58 is for Byron addresses whereas others use Bech32.
type AddressToDecode = "Bech32/Hex/Base58 encoded address"

-- | Denotes network for the entity in question, such as address.
data NetworkId = NIDMainnet | NIDTestnet
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "NID", LowerFirst]] NetworkId

-- | Denotes kind of a payment credential.
data PaymentCredKind = PCKKey | PCKScript
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "PCK", LowerFirst]] PaymentCredKind

-- | Payment credential, the payment part of a Cardano address.
data PaymentCredential = PaymentCredential
  { paymentCredentialBech32:: !(Bech32StringOf PaymentCredentialAddress)
  -- ^ Bech32-encoding of the credential key hash or script hash.
  , paymentCredentialHex    :: !(HexStringOf PaymentCredentialAddress)
  -- ^ Hex-encoding of the script or key credential.
  , paymentCredentialKind   :: !PaymentCredKind
  -- ^ See `PaymentCredKind`.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "paymentCredential", CamelToSnake]] PaymentCredential

-- | Denotes kind of a staking credential.
data StakingCredKind = SCKKey | SCKScript | SCKPointer
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "SCK", LowerFirst]] StakingCredKind

-- | To understand it, see it's use in `ChainPointer` datatype.
newtype CertIndex = CertIndex Natural
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, Real, Integral, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

-- | In an address, a chain pointer refers to a point of the chain containing a stake key registration certificate. A point is identified by 3 coordinates, as listed in the type.
data ChainPointer = ChainPointer
  { chainPointerSlot      :: !SlotNo
  -- ^ An absolute slot number.
  , chainPointerTxIndex   :: !TxIndex
  -- ^ A transaction index (within that slot).
  , chainPointerCertIndex :: !CertIndex
  -- ^ A (delegation) certificate index (within that transaction).
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "chainPointer", CamelToSnake]] ChainPointer

-- | Payment credential, the payment part of a Cardano address.
data StakingCredential = StakingCredential
  { stakingCredentialBech32:: !(Maybe (Bech32StringOf StakingCredentialAddress))
  -- ^ Bech32-encoding of the credential key hash or script hash.
  , stakingCredentialHex   :: !(Maybe (HexStringOf StakingCredentialAddress))
  -- ^ Hex-encoding of the script or key credential.
  , stakingCredentialKind  :: !StakingCredKind
  -- ^ See `StakingCredKind`.
  , stakingCredentialPointer  :: !(Maybe ChainPointer)
  -- ^ See `ChainPointer`.
  , stakingCredentialRewardAddress :: !(Maybe (Bech32StringOf RewardAddress))
  -- ^ See `RewardAddress`.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "stakingCredential", CamelToSnake]] StakingCredential

-- | Information decoded from a Cardano address.
data AddressInfo = AddressInfo
  { addressInfoHex         :: !(HexStringOf Address)
  -- ^ Hexadecimal format encoding of the given address.
  , addressInfoBech32      :: !(Maybe (Bech32StringOf Address))
  -- ^ Bech32 representation of the given address. Present for Shelly & stake addresses whereas byron addresses are encoded in Base58.
  , addressInfoNetwork     :: !(Maybe NetworkId)
  -- ^ See `NetworkId`.
  , addressInfoPaymentCred :: !(Maybe PaymentCredential)
  -- ^ See `PaymentCredential`.
  , addressInfoStakingCred :: !(Maybe StakingCredential)
  -- ^ See `StakingCredential`.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "addressInfo", CamelToSnake]] AddressInfo

-- | Output reference of an UTxO. This is different from `OutputReference` type as later JSON representation is a string whereas this has an object format.
data OutputReferenceObject = OutputReferenceObject
  { outputReferenceObjectIndex  :: !TxIndex
  , outputReferenceObjectTxHash :: !TxHash
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "outputReferenceObject", CamelToSnake]] OutputReferenceObject

-- | UTxO IDs for all the unspent transaction outputs at an address.
data PaginatedOutputReferenceObject = PaginatedOutputReferenceObject
  { paginatedOutputReferenceObjectData        :: ![OutputReferenceObject]
  -- ^ See `OutputReferenceObject`.
  , paginatedOutputReferenceObjectLastUpdated :: !LastUpdated
  -- ^ See `LastUpdated`.
  , paginatedOutputReferenceObjectNextCursor  :: !(Maybe NextCursor)
  -- ^ See `NextCursor`.
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "paginatedOutputReferenceObject", CamelToSnake]] PaginatedOutputReferenceObject

instance IsTimestamped PaginatedOutputReferenceObject where
  type TimestampedData PaginatedOutputReferenceObject = [OutputReferenceObject]
  getTimestampedData = paginatedOutputReferenceObjectData
  getTimestamp = paginatedOutputReferenceObjectLastUpdated

instance HasCursor PaginatedOutputReferenceObject where
  getNextCursor = paginatedOutputReferenceObjectNextCursor
