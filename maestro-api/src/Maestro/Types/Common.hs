module Maestro.Types.Common where

import qualified Data.Aeson     as Aeson
import           Deriving.Aeson

data MaestroDatum = MaestroDatum
  { _maestroDatumBytes :: !String
  , _maestroDatumHash  :: !String
  , _maestroDatumJson  :: !Aeson.Value
  , _maestroDatumType  :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_maestroDatum", CamelToSnake]] MaestroDatum


data MaestroRefScript = MaestroRefScript
  { _maestroRefScriptBytes :: !String
  , _maestroRefScriptHash  :: !String
  , _maestroRefScriptJson  :: !Aeson.Value
  , _maestroRefScriptType  :: !String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_maestroRefScript", CamelToSnake]] MaestroRefScript

data MaestroAsset =  MaestroAsset
  { _maestroAssetQuantity :: !Integer
  , _maestroAssetUnit     :: !(Maybe String)
  , _maestroAssetName     :: !(Maybe String)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "_maestroAsset", CamelToSnake]] MaestroAsset

data MaestroOrder = ASC | DESC

instance Show MaestroOrder where
  show (ASC)  = "asc"
  show (DESC) = "desc"
