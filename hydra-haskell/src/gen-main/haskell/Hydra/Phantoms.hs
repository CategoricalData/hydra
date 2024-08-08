-- | Phantom types for use with Hydra DSLs

module Hydra.Phantoms where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | An association of a field name (as in a case statement) with a phantom type
newtype TCase a = 
  TCase {
    unTCase :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_TCase = (Core.Name "hydra/phantoms.TCase")

_TCase_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "a"),
  Core.lambdaTypeBody = Core._Name_type_}))

-- | An association with a named term (element) with a phantom type
data TElement a = 
  TElement {
    tElementName :: Core.Name,
    tElementTerm :: (TTerm a)}
  deriving (Eq, Ord, Read, Show)

_TElement = (Core.Name "hydra/phantoms.TElement")

_TElement_name = (Core.Name "name")

_TElement_term = (Core.Name "term")

_TElement_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "a"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/phantoms.TElement"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "name"),
        Core.fieldTypeType = Core._Name_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "term"),
        Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _TTerm_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))}))}]}))}))

-- | An association with a term-level field with a phantom type
newtype TField a = 
  TField {
    unTField :: Core.Field}
  deriving (Eq, Ord, Read, Show)

_TField = (Core.Name "hydra/phantoms.TField")

_TField_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "a"),
  Core.lambdaTypeBody = Core._Field_type_}))

-- | An association of a term with a phantom type
newtype TTerm a = 
  TTerm {
    unTTerm :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_TTerm = (Core.Name "hydra/phantoms.TTerm")

_TTerm_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "a"),
  Core.lambdaTypeBody = Core._Term_type_}))