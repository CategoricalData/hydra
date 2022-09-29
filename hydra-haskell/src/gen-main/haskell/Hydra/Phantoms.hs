-- | Phantom types for use in model definitions

module Hydra.Phantoms where

import qualified Hydra.Core as Core
import qualified Hydra.Evaluation as Evaluation
import Data.Map
import Data.Set

newtype Case a 
  = Case {
    unCase :: Core.FieldName}
  deriving (Eq, Ord, Read, Show)

_Case = (Core.Name "hydra/phantoms.Case")

newtype Datum a 
  = Datum {
    unDatum :: (Core.Term Evaluation.Meta)}
  deriving (Eq, Ord, Read, Show)

_Datum = (Core.Name "hydra/phantoms.Datum")

data Definition a 
  = Definition {
    definitionName :: Core.Name,
    definitionDatum :: (Datum a)}
  deriving (Eq, Ord, Read, Show)

_Definition = (Core.Name "hydra/phantoms.Definition")

_Definition_name = (Core.FieldName "name")

_Definition_datum = (Core.FieldName "datum")

data Reference a 
  = Reference {}
  deriving (Eq, Ord, Read, Show)

_Reference = (Core.Name "hydra/phantoms.Reference")