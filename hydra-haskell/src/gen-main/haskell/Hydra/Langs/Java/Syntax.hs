-- | A Java syntax module. Based on the Oracle Java SE 12 BNF:
-- |   https://docs.oracle.com/javase/specs/jls/se12/html/jls-19.html
-- | Note: all *WithComments types were added manually, rather than derived from the BNF, which does not allow for comments.

module Hydra.Langs.Java.Syntax where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype Identifier = 
  Identifier {
    unIdentifier :: String}
  deriving (Eq, Ord, Read, Show)

_Identifier = (Core.Name "hydra/langs/java/syntax.Identifier")

_Identifier_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype TypeIdentifier = 
  TypeIdentifier {
    unTypeIdentifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_TypeIdentifier = (Core.Name "hydra/langs/java/syntax.TypeIdentifier")

_TypeIdentifier_type_ = _Identifier_type_

data Literal = 
  LiteralNull  |
  LiteralInteger IntegerLiteral |
  LiteralFloatingPoint FloatingPointLiteral |
  LiteralBoolean Bool |
  LiteralCharacter Int |
  LiteralString StringLiteral
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra/langs/java/syntax.Literal")

_Literal_null = (Core.Name "null")

_Literal_integer = (Core.Name "integer")

_Literal_floatingPoint = (Core.Name "floatingPoint")

_Literal_boolean = (Core.Name "boolean")

_Literal_character = (Core.Name "character")

_Literal_string = (Core.Name "string")

_Literal_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.Literal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "null"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "integer"),
      Core.fieldTypeType = _IntegerLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "floatingPoint"),
      Core.fieldTypeType = _FloatingPointLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "character"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringLiteral_type_}]}))

-- | Note: this is an approximation which ignores encoding
newtype IntegerLiteral = 
  IntegerLiteral {
    unIntegerLiteral :: Integer}
  deriving (Eq, Ord, Read, Show)

_IntegerLiteral = (Core.Name "hydra/langs/java/syntax.IntegerLiteral")

_IntegerLiteral_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))

-- | Note: this is an approximation which ignores encoding
newtype FloatingPointLiteral = 
  FloatingPointLiteral {
    unFloatingPointLiteral :: Double}
  deriving (Eq, Ord, Read, Show)

_FloatingPointLiteral = (Core.Name "hydra/langs/java/syntax.FloatingPointLiteral")

_FloatingPointLiteral_type_ = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeBigfloat))

-- | Note: this is an approximation which ignores encoding
newtype StringLiteral = 
  StringLiteral {
    unStringLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_StringLiteral = (Core.Name "hydra/langs/java/syntax.StringLiteral")

_StringLiteral_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data Type = 
  TypePrimitive PrimitiveTypeWithAnnotations |
  TypeReference ReferenceType
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra/langs/java/syntax.Type")

_Type_primitive = (Core.Name "primitive")

_Type_reference = (Core.Name "reference")

_Type_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.Type"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primitive"),
      Core.fieldTypeType = _PrimitiveTypeWithAnnotations_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "reference"),
      Core.fieldTypeType = _ReferenceType_type_}]}))

data PrimitiveTypeWithAnnotations = 
  PrimitiveTypeWithAnnotations {
    primitiveTypeWithAnnotationsType :: PrimitiveType,
    primitiveTypeWithAnnotationsAnnotations :: [Annotation]}
  deriving (Eq, Ord, Read, Show)

_PrimitiveTypeWithAnnotations = (Core.Name "hydra/langs/java/syntax.PrimitiveTypeWithAnnotations")

_PrimitiveTypeWithAnnotations_type = (Core.Name "type")

_PrimitiveTypeWithAnnotations_annotations = (Core.Name "annotations")

_PrimitiveTypeWithAnnotations_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.PrimitiveTypeWithAnnotations"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _PrimitiveType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)}]}))

data PrimitiveType = 
  PrimitiveTypeNumeric NumericType |
  PrimitiveTypeBoolean 
  deriving (Eq, Ord, Read, Show)

_PrimitiveType = (Core.Name "hydra/langs/java/syntax.PrimitiveType")

_PrimitiveType_numeric = (Core.Name "numeric")

_PrimitiveType_boolean = (Core.Name "boolean")

_PrimitiveType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.PrimitiveType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "numeric"),
      Core.fieldTypeType = _NumericType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data NumericType = 
  NumericTypeIntegral IntegralType |
  NumericTypeFloatingPoint FloatingPointType
  deriving (Eq, Ord, Read, Show)

_NumericType = (Core.Name "hydra/langs/java/syntax.NumericType")

_NumericType_integral = (Core.Name "integral")

_NumericType_floatingPoint = (Core.Name "floatingPoint")

_NumericType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.NumericType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "integral"),
      Core.fieldTypeType = _IntegralType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "floatingPoint"),
      Core.fieldTypeType = _FloatingPointType_type_}]}))

data IntegralType = 
  IntegralTypeByte  |
  IntegralTypeShort  |
  IntegralTypeInt  |
  IntegralTypeLong  |
  IntegralTypeChar 
  deriving (Eq, Ord, Read, Show)

_IntegralType = (Core.Name "hydra/langs/java/syntax.IntegralType")

_IntegralType_byte = (Core.Name "byte")

_IntegralType_short = (Core.Name "short")

_IntegralType_int = (Core.Name "int")

_IntegralType_long = (Core.Name "long")

_IntegralType_char = (Core.Name "char")

_IntegralType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.IntegralType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "byte"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "short"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "int"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "long"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "char"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data FloatingPointType = 
  FloatingPointTypeFloat  |
  FloatingPointTypeDouble 
  deriving (Eq, Ord, Read, Show)

_FloatingPointType = (Core.Name "hydra/langs/java/syntax.FloatingPointType")

_FloatingPointType_float = (Core.Name "float")

_FloatingPointType_double = (Core.Name "double")

_FloatingPointType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.FloatingPointType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "float"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "double"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data ReferenceType = 
  ReferenceTypeClassOrInterface ClassOrInterfaceType |
  ReferenceTypeVariable TypeVariable |
  ReferenceTypeArray ArrayType
  deriving (Eq, Ord, Read, Show)

_ReferenceType = (Core.Name "hydra/langs/java/syntax.ReferenceType")

_ReferenceType_classOrInterface = (Core.Name "classOrInterface")

_ReferenceType_variable = (Core.Name "variable")

_ReferenceType_array = (Core.Name "array")

_ReferenceType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ReferenceType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classOrInterface"),
      Core.fieldTypeType = _ClassOrInterfaceType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _TypeVariable_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _ArrayType_type_}]}))

data ClassOrInterfaceType = 
  ClassOrInterfaceTypeClass ClassType |
  ClassOrInterfaceTypeInterface InterfaceType
  deriving (Eq, Ord, Read, Show)

_ClassOrInterfaceType = (Core.Name "hydra/langs/java/syntax.ClassOrInterfaceType")

_ClassOrInterfaceType_class = (Core.Name "class")

_ClassOrInterfaceType_interface = (Core.Name "interface")

_ClassOrInterfaceType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassOrInterfaceType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _ClassType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "interface"),
      Core.fieldTypeType = _InterfaceType_type_}]}))

data ClassType = 
  ClassType {
    classTypeAnnotations :: [Annotation],
    classTypeQualifier :: ClassTypeQualifier,
    classTypeIdentifier :: TypeIdentifier,
    classTypeArguments :: [TypeArgument]}
  deriving (Eq, Ord, Read, Show)

_ClassType = (Core.Name "hydra/langs/java/syntax.ClassType")

_ClassType_annotations = (Core.Name "annotations")

_ClassType_qualifier = (Core.Name "qualifier")

_ClassType_identifier = (Core.Name "identifier")

_ClassType_arguments = (Core.Name "arguments")

_ClassType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifier"),
      Core.fieldTypeType = _ClassTypeQualifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _TypeIdentifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = (Core.TypeList _TypeArgument_type_)}]}))

data ClassTypeQualifier = 
  ClassTypeQualifierNone  |
  ClassTypeQualifierPackage PackageName |
  ClassTypeQualifierParent ClassOrInterfaceType
  deriving (Eq, Ord, Read, Show)

_ClassTypeQualifier = (Core.Name "hydra/langs/java/syntax.ClassTypeQualifier")

_ClassTypeQualifier_none = (Core.Name "none")

_ClassTypeQualifier_package = (Core.Name "package")

_ClassTypeQualifier_parent = (Core.Name "parent")

_ClassTypeQualifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassTypeQualifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "none"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "package"),
      Core.fieldTypeType = _PackageName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parent"),
      Core.fieldTypeType = _ClassOrInterfaceType_type_}]}))

newtype InterfaceType = 
  InterfaceType {
    unInterfaceType :: ClassType}
  deriving (Eq, Ord, Read, Show)

_InterfaceType = (Core.Name "hydra/langs/java/syntax.InterfaceType")

_InterfaceType_type_ = _ClassType_type_

data TypeVariable = 
  TypeVariable {
    typeVariableAnnotations :: [Annotation],
    typeVariableIdentifier :: TypeIdentifier}
  deriving (Eq, Ord, Read, Show)

_TypeVariable = (Core.Name "hydra/langs/java/syntax.TypeVariable")

_TypeVariable_annotations = (Core.Name "annotations")

_TypeVariable_identifier = (Core.Name "identifier")

_TypeVariable_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TypeVariable"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _TypeIdentifier_type_}]}))

data ArrayType = 
  ArrayType {
    arrayTypeDims :: Dims,
    arrayTypeVariant :: ArrayType_Variant}
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra/langs/java/syntax.ArrayType")

_ArrayType_dims = (Core.Name "dims")

_ArrayType_variant = (Core.Name "variant")

_ArrayType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ArrayType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dims"),
      Core.fieldTypeType = _Dims_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variant"),
      Core.fieldTypeType = _ArrayType_Variant_type_}]}))

data ArrayType_Variant = 
  ArrayType_VariantPrimitive PrimitiveTypeWithAnnotations |
  ArrayType_VariantClassOrInterface ClassOrInterfaceType |
  ArrayType_VariantVariable TypeVariable
  deriving (Eq, Ord, Read, Show)

_ArrayType_Variant = (Core.Name "hydra/langs/java/syntax.ArrayType.Variant")

_ArrayType_Variant_primitive = (Core.Name "primitive")

_ArrayType_Variant_classOrInterface = (Core.Name "classOrInterface")

_ArrayType_Variant_variable = (Core.Name "variable")

_ArrayType_Variant_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ArrayType.Variant"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primitive"),
      Core.fieldTypeType = _PrimitiveTypeWithAnnotations_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classOrInterface"),
      Core.fieldTypeType = _ClassOrInterfaceType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _TypeVariable_type_}]}))

newtype Dims = 
  Dims {
    unDims :: [[Annotation]]}
  deriving (Eq, Ord, Read, Show)

_Dims = (Core.Name "hydra/langs/java/syntax.Dims")

_Dims_type_ = (Core.TypeList (Core.TypeList _Annotation_type_))

data TypeParameter = 
  TypeParameter {
    typeParameterModifiers :: [TypeParameterModifier],
    typeParameterIdentifier :: TypeIdentifier,
    typeParameterBound :: (Maybe TypeBound)}
  deriving (Eq, Ord, Read, Show)

_TypeParameter = (Core.Name "hydra/langs/java/syntax.TypeParameter")

_TypeParameter_modifiers = (Core.Name "modifiers")

_TypeParameter_identifier = (Core.Name "identifier")

_TypeParameter_bound = (Core.Name "bound")

_TypeParameter_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TypeParameter"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _TypeParameterModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _TypeIdentifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bound"),
      Core.fieldTypeType = (Core.TypeOptional _TypeBound_type_)}]}))

newtype TypeParameterModifier = 
  TypeParameterModifier {
    unTypeParameterModifier :: Annotation}
  deriving (Eq, Ord, Read, Show)

_TypeParameterModifier = (Core.Name "hydra/langs/java/syntax.TypeParameterModifier")

_TypeParameterModifier_type_ = _Annotation_type_

data TypeBound = 
  TypeBoundVariable TypeVariable |
  TypeBoundClassOrInterface TypeBound_ClassOrInterface
  deriving (Eq, Ord, Read, Show)

_TypeBound = (Core.Name "hydra/langs/java/syntax.TypeBound")

_TypeBound_variable = (Core.Name "variable")

_TypeBound_classOrInterface = (Core.Name "classOrInterface")

_TypeBound_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TypeBound"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _TypeVariable_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classOrInterface"),
      Core.fieldTypeType = _TypeBound_ClassOrInterface_type_}]}))

data TypeBound_ClassOrInterface = 
  TypeBound_ClassOrInterface {
    typeBound_ClassOrInterfaceType :: ClassOrInterfaceType,
    typeBound_ClassOrInterfaceAdditional :: [AdditionalBound]}
  deriving (Eq, Ord, Read, Show)

_TypeBound_ClassOrInterface = (Core.Name "hydra/langs/java/syntax.TypeBound.ClassOrInterface")

_TypeBound_ClassOrInterface_type = (Core.Name "type")

_TypeBound_ClassOrInterface_additional = (Core.Name "additional")

_TypeBound_ClassOrInterface_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TypeBound.ClassOrInterface"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _ClassOrInterfaceType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "additional"),
      Core.fieldTypeType = (Core.TypeList _AdditionalBound_type_)}]}))

newtype AdditionalBound = 
  AdditionalBound {
    unAdditionalBound :: InterfaceType}
  deriving (Eq, Ord, Read, Show)

_AdditionalBound = (Core.Name "hydra/langs/java/syntax.AdditionalBound")

_AdditionalBound_type_ = _InterfaceType_type_

data TypeArgument = 
  TypeArgumentReference ReferenceType |
  TypeArgumentWildcard Wildcard
  deriving (Eq, Ord, Read, Show)

_TypeArgument = (Core.Name "hydra/langs/java/syntax.TypeArgument")

_TypeArgument_reference = (Core.Name "reference")

_TypeArgument_wildcard = (Core.Name "wildcard")

_TypeArgument_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TypeArgument"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "reference"),
      Core.fieldTypeType = _ReferenceType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "wildcard"),
      Core.fieldTypeType = _Wildcard_type_}]}))

data Wildcard = 
  Wildcard {
    wildcardAnnotations :: [Annotation],
    wildcardWildcard :: (Maybe WildcardBounds)}
  deriving (Eq, Ord, Read, Show)

_Wildcard = (Core.Name "hydra/langs/java/syntax.Wildcard")

_Wildcard_annotations = (Core.Name "annotations")

_Wildcard_wildcard = (Core.Name "wildcard")

_Wildcard_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.Wildcard"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "wildcard"),
      Core.fieldTypeType = (Core.TypeOptional _WildcardBounds_type_)}]}))

data WildcardBounds = 
  WildcardBoundsExtends ReferenceType |
  WildcardBoundsSuper ReferenceType
  deriving (Eq, Ord, Read, Show)

_WildcardBounds = (Core.Name "hydra/langs/java/syntax.WildcardBounds")

_WildcardBounds_extends = (Core.Name "extends")

_WildcardBounds_super = (Core.Name "super")

_WildcardBounds_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.WildcardBounds"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "extends"),
      Core.fieldTypeType = _ReferenceType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "super"),
      Core.fieldTypeType = _ReferenceType_type_}]}))

data ModuleName = 
  ModuleName {
    moduleNameIdentifier :: Identifier,
    moduleNameName :: (Maybe ModuleName)}
  deriving (Eq, Ord, Read, Show)

_ModuleName = (Core.Name "hydra/langs/java/syntax.ModuleName")

_ModuleName_identifier = (Core.Name "identifier")

_ModuleName_name = (Core.Name "name")

_ModuleName_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ModuleName"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = (Core.TypeOptional _ModuleName_type_)}]}))

newtype PackageName = 
  PackageName {
    unPackageName :: [Identifier]}
  deriving (Eq, Ord, Read, Show)

_PackageName = (Core.Name "hydra/langs/java/syntax.PackageName")

_PackageName_type_ = (Core.TypeList _Identifier_type_)

data TypeName = 
  TypeName {
    typeNameIdentifier :: TypeIdentifier,
    typeNameQualifier :: (Maybe PackageOrTypeName)}
  deriving (Eq, Ord, Read, Show)

_TypeName = (Core.Name "hydra/langs/java/syntax.TypeName")

_TypeName_identifier = (Core.Name "identifier")

_TypeName_qualifier = (Core.Name "qualifier")

_TypeName_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TypeName"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _TypeIdentifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifier"),
      Core.fieldTypeType = (Core.TypeOptional _PackageOrTypeName_type_)}]}))

data ExpressionName = 
  ExpressionName {
    expressionNameQualifier :: (Maybe AmbiguousName),
    expressionNameIdentifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_ExpressionName = (Core.Name "hydra/langs/java/syntax.ExpressionName")

_ExpressionName_qualifier = (Core.Name "qualifier")

_ExpressionName_identifier = (Core.Name "identifier")

_ExpressionName_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ExpressionName"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifier"),
      Core.fieldTypeType = (Core.TypeOptional _AmbiguousName_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_}]}))

newtype MethodName = 
  MethodName {
    unMethodName :: Identifier}
  deriving (Eq, Ord, Read, Show)

_MethodName = (Core.Name "hydra/langs/java/syntax.MethodName")

_MethodName_type_ = _Identifier_type_

newtype PackageOrTypeName = 
  PackageOrTypeName {
    unPackageOrTypeName :: [Identifier]}
  deriving (Eq, Ord, Read, Show)

_PackageOrTypeName = (Core.Name "hydra/langs/java/syntax.PackageOrTypeName")

_PackageOrTypeName_type_ = (Core.TypeList _Identifier_type_)

newtype AmbiguousName = 
  AmbiguousName {
    unAmbiguousName :: [Identifier]}
  deriving (Eq, Ord, Read, Show)

_AmbiguousName = (Core.Name "hydra/langs/java/syntax.AmbiguousName")

_AmbiguousName_type_ = (Core.TypeList _Identifier_type_)

data CompilationUnit = 
  CompilationUnitOrdinary OrdinaryCompilationUnit |
  CompilationUnitModular ModularCompilationUnit
  deriving (Eq, Ord, Read, Show)

_CompilationUnit = (Core.Name "hydra/langs/java/syntax.CompilationUnit")

_CompilationUnit_ordinary = (Core.Name "ordinary")

_CompilationUnit_modular = (Core.Name "modular")

_CompilationUnit_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.CompilationUnit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ordinary"),
      Core.fieldTypeType = _OrdinaryCompilationUnit_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modular"),
      Core.fieldTypeType = _ModularCompilationUnit_type_}]}))

data OrdinaryCompilationUnit = 
  OrdinaryCompilationUnit {
    ordinaryCompilationUnitPackage :: (Maybe PackageDeclaration),
    ordinaryCompilationUnitImports :: [ImportDeclaration],
    ordinaryCompilationUnitTypes :: [TypeDeclarationWithComments]}
  deriving (Eq, Ord, Read, Show)

_OrdinaryCompilationUnit = (Core.Name "hydra/langs/java/syntax.OrdinaryCompilationUnit")

_OrdinaryCompilationUnit_package = (Core.Name "package")

_OrdinaryCompilationUnit_imports = (Core.Name "imports")

_OrdinaryCompilationUnit_types = (Core.Name "types")

_OrdinaryCompilationUnit_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.OrdinaryCompilationUnit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "package"),
      Core.fieldTypeType = (Core.TypeOptional _PackageDeclaration_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "imports"),
      Core.fieldTypeType = (Core.TypeList _ImportDeclaration_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "types"),
      Core.fieldTypeType = (Core.TypeList _TypeDeclarationWithComments_type_)}]}))

data ModularCompilationUnit = 
  ModularCompilationUnit {
    modularCompilationUnitImports :: [ImportDeclaration],
    modularCompilationUnitModule :: ModuleDeclaration}
  deriving (Eq, Ord, Read, Show)

_ModularCompilationUnit = (Core.Name "hydra/langs/java/syntax.ModularCompilationUnit")

_ModularCompilationUnit_imports = (Core.Name "imports")

_ModularCompilationUnit_module = (Core.Name "module")

_ModularCompilationUnit_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ModularCompilationUnit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "imports"),
      Core.fieldTypeType = (Core.TypeList _ImportDeclaration_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "module"),
      Core.fieldTypeType = _ModuleDeclaration_type_}]}))

data PackageDeclaration = 
  PackageDeclaration {
    packageDeclarationModifiers :: [PackageModifier],
    packageDeclarationIdentifiers :: [Identifier]}
  deriving (Eq, Ord, Read, Show)

_PackageDeclaration = (Core.Name "hydra/langs/java/syntax.PackageDeclaration")

_PackageDeclaration_modifiers = (Core.Name "modifiers")

_PackageDeclaration_identifiers = (Core.Name "identifiers")

_PackageDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.PackageDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _PackageModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifiers"),
      Core.fieldTypeType = (Core.TypeList _Identifier_type_)}]}))

newtype PackageModifier = 
  PackageModifier {
    unPackageModifier :: Annotation}
  deriving (Eq, Ord, Read, Show)

_PackageModifier = (Core.Name "hydra/langs/java/syntax.PackageModifier")

_PackageModifier_type_ = _Annotation_type_

data ImportDeclaration = 
  ImportDeclarationSingleType SingleTypeImportDeclaration |
  ImportDeclarationTypeImportOnDemand TypeImportOnDemandDeclaration |
  ImportDeclarationSingleStaticImport SingleStaticImportDeclaration |
  ImportDeclarationStaticImportOnDemand StaticImportOnDemandDeclaration
  deriving (Eq, Ord, Read, Show)

_ImportDeclaration = (Core.Name "hydra/langs/java/syntax.ImportDeclaration")

_ImportDeclaration_singleType = (Core.Name "singleType")

_ImportDeclaration_typeImportOnDemand = (Core.Name "typeImportOnDemand")

_ImportDeclaration_singleStaticImport = (Core.Name "singleStaticImport")

_ImportDeclaration_staticImportOnDemand = (Core.Name "staticImportOnDemand")

_ImportDeclaration_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ImportDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "singleType"),
      Core.fieldTypeType = _SingleTypeImportDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeImportOnDemand"),
      Core.fieldTypeType = _TypeImportOnDemandDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "singleStaticImport"),
      Core.fieldTypeType = _SingleStaticImportDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "staticImportOnDemand"),
      Core.fieldTypeType = _StaticImportOnDemandDeclaration_type_}]}))

newtype SingleTypeImportDeclaration = 
  SingleTypeImportDeclaration {
    unSingleTypeImportDeclaration :: TypeName}
  deriving (Eq, Ord, Read, Show)

_SingleTypeImportDeclaration = (Core.Name "hydra/langs/java/syntax.SingleTypeImportDeclaration")

_SingleTypeImportDeclaration_type_ = _TypeName_type_

newtype TypeImportOnDemandDeclaration = 
  TypeImportOnDemandDeclaration {
    unTypeImportOnDemandDeclaration :: PackageOrTypeName}
  deriving (Eq, Ord, Read, Show)

_TypeImportOnDemandDeclaration = (Core.Name "hydra/langs/java/syntax.TypeImportOnDemandDeclaration")

_TypeImportOnDemandDeclaration_type_ = _PackageOrTypeName_type_

data SingleStaticImportDeclaration = 
  SingleStaticImportDeclaration {
    singleStaticImportDeclarationTypeName :: TypeName,
    singleStaticImportDeclarationIdentifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_SingleStaticImportDeclaration = (Core.Name "hydra/langs/java/syntax.SingleStaticImportDeclaration")

_SingleStaticImportDeclaration_typeName = (Core.Name "typeName")

_SingleStaticImportDeclaration_identifier = (Core.Name "identifier")

_SingleStaticImportDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.SingleStaticImportDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeName"),
      Core.fieldTypeType = _TypeName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_}]}))

newtype StaticImportOnDemandDeclaration = 
  StaticImportOnDemandDeclaration {
    unStaticImportOnDemandDeclaration :: TypeName}
  deriving (Eq, Ord, Read, Show)

_StaticImportOnDemandDeclaration = (Core.Name "hydra/langs/java/syntax.StaticImportOnDemandDeclaration")

_StaticImportOnDemandDeclaration_type_ = _TypeName_type_

data TypeDeclaration = 
  TypeDeclarationClass ClassDeclaration |
  TypeDeclarationInterface InterfaceDeclaration |
  TypeDeclarationNone 
  deriving (Eq, Ord, Read, Show)

_TypeDeclaration = (Core.Name "hydra/langs/java/syntax.TypeDeclaration")

_TypeDeclaration_class = (Core.Name "class")

_TypeDeclaration_interface = (Core.Name "interface")

_TypeDeclaration_none = (Core.Name "none")

_TypeDeclaration_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TypeDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _ClassDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "interface"),
      Core.fieldTypeType = _InterfaceDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "none"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TypeDeclarationWithComments = 
  TypeDeclarationWithComments {
    typeDeclarationWithCommentsValue :: TypeDeclaration,
    typeDeclarationWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_TypeDeclarationWithComments = (Core.Name "hydra/langs/java/syntax.TypeDeclarationWithComments")

_TypeDeclarationWithComments_value = (Core.Name "value")

_TypeDeclarationWithComments_comments = (Core.Name "comments")

_TypeDeclarationWithComments_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TypeDeclarationWithComments"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _TypeDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "comments"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString))}]}))

data ModuleDeclaration = 
  ModuleDeclaration {
    moduleDeclarationAnnotations :: [Annotation],
    moduleDeclarationOpen :: Bool,
    moduleDeclarationIdentifiers :: [Identifier],
    moduleDeclarationDirectives :: [[ModuleDirective]]}
  deriving (Eq, Ord, Read, Show)

_ModuleDeclaration = (Core.Name "hydra/langs/java/syntax.ModuleDeclaration")

_ModuleDeclaration_annotations = (Core.Name "annotations")

_ModuleDeclaration_open = (Core.Name "open")

_ModuleDeclaration_identifiers = (Core.Name "identifiers")

_ModuleDeclaration_directives = (Core.Name "directives")

_ModuleDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ModuleDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "open"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifiers"),
      Core.fieldTypeType = (Core.TypeList _Identifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeList (Core.TypeList _ModuleDirective_type_))}]}))

data ModuleDirective = 
  ModuleDirectiveRequires ModuleDirective_Requires |
  ModuleDirectiveExports ModuleDirective_ExportsOrOpens |
  ModuleDirectiveOpens ModuleDirective_ExportsOrOpens |
  ModuleDirectiveUses TypeName |
  ModuleDirectiveProvides ModuleDirective_Provides
  deriving (Eq, Ord, Read, Show)

_ModuleDirective = (Core.Name "hydra/langs/java/syntax.ModuleDirective")

_ModuleDirective_requires = (Core.Name "requires")

_ModuleDirective_exports = (Core.Name "exports")

_ModuleDirective_opens = (Core.Name "opens")

_ModuleDirective_uses = (Core.Name "uses")

_ModuleDirective_provides = (Core.Name "provides")

_ModuleDirective_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ModuleDirective"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "requires"),
      Core.fieldTypeType = _ModuleDirective_Requires_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "exports"),
      Core.fieldTypeType = _ModuleDirective_ExportsOrOpens_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "opens"),
      Core.fieldTypeType = _ModuleDirective_ExportsOrOpens_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "uses"),
      Core.fieldTypeType = _TypeName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "provides"),
      Core.fieldTypeType = _ModuleDirective_Provides_type_}]}))

data ModuleDirective_Requires = 
  ModuleDirective_Requires {
    moduleDirective_RequiresModifiers :: [RequiresModifier],
    moduleDirective_RequiresModule :: ModuleName}
  deriving (Eq, Ord, Read, Show)

_ModuleDirective_Requires = (Core.Name "hydra/langs/java/syntax.ModuleDirective.Requires")

_ModuleDirective_Requires_modifiers = (Core.Name "modifiers")

_ModuleDirective_Requires_module = (Core.Name "module")

_ModuleDirective_Requires_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ModuleDirective.Requires"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _RequiresModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "module"),
      Core.fieldTypeType = _ModuleName_type_}]}))

data ModuleDirective_ExportsOrOpens = 
  ModuleDirective_ExportsOrOpens {
    moduleDirective_ExportsOrOpensPackage :: PackageName,
    -- | At least one module
    moduleDirective_ExportsOrOpensModules :: [ModuleName]}
  deriving (Eq, Ord, Read, Show)

_ModuleDirective_ExportsOrOpens = (Core.Name "hydra/langs/java/syntax.ModuleDirective.ExportsOrOpens")

_ModuleDirective_ExportsOrOpens_package = (Core.Name "package")

_ModuleDirective_ExportsOrOpens_modules = (Core.Name "modules")

_ModuleDirective_ExportsOrOpens_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ModuleDirective.ExportsOrOpens"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "package"),
      Core.fieldTypeType = _PackageName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modules"),
      Core.fieldTypeType = (Core.TypeList _ModuleName_type_)}]}))

data ModuleDirective_Provides = 
  ModuleDirective_Provides {
    moduleDirective_ProvidesTo :: TypeName,
    -- | At least one type
    moduleDirective_ProvidesWith :: [TypeName]}
  deriving (Eq, Ord, Read, Show)

_ModuleDirective_Provides = (Core.Name "hydra/langs/java/syntax.ModuleDirective.Provides")

_ModuleDirective_Provides_to = (Core.Name "to")

_ModuleDirective_Provides_with = (Core.Name "with")

_ModuleDirective_Provides_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ModuleDirective.Provides"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "to"),
      Core.fieldTypeType = _TypeName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "with"),
      Core.fieldTypeType = (Core.TypeList _TypeName_type_)}]}))

data RequiresModifier = 
  RequiresModifierTransitive  |
  RequiresModifierStatic 
  deriving (Eq, Ord, Read, Show)

_RequiresModifier = (Core.Name "hydra/langs/java/syntax.RequiresModifier")

_RequiresModifier_transitive = (Core.Name "transitive")

_RequiresModifier_static = (Core.Name "static")

_RequiresModifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.RequiresModifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "transitive"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "static"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data ClassDeclaration = 
  ClassDeclarationNormal NormalClassDeclaration |
  ClassDeclarationEnum EnumDeclaration
  deriving (Eq, Ord, Read, Show)

_ClassDeclaration = (Core.Name "hydra/langs/java/syntax.ClassDeclaration")

_ClassDeclaration_normal = (Core.Name "normal")

_ClassDeclaration_enum = (Core.Name "enum")

_ClassDeclaration_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "normal"),
      Core.fieldTypeType = _NormalClassDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enum"),
      Core.fieldTypeType = _EnumDeclaration_type_}]}))

data NormalClassDeclaration = 
  NormalClassDeclaration {
    normalClassDeclarationModifiers :: [ClassModifier],
    normalClassDeclarationIdentifier :: TypeIdentifier,
    normalClassDeclarationParameters :: [TypeParameter],
    normalClassDeclarationExtends :: (Maybe ClassType),
    normalClassDeclarationImplements :: [InterfaceType],
    normalClassDeclarationBody :: ClassBody}
  deriving (Eq, Ord, Read, Show)

_NormalClassDeclaration = (Core.Name "hydra/langs/java/syntax.NormalClassDeclaration")

_NormalClassDeclaration_modifiers = (Core.Name "modifiers")

_NormalClassDeclaration_identifier = (Core.Name "identifier")

_NormalClassDeclaration_parameters = (Core.Name "parameters")

_NormalClassDeclaration_extends = (Core.Name "extends")

_NormalClassDeclaration_implements = (Core.Name "implements")

_NormalClassDeclaration_body = (Core.Name "body")

_NormalClassDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.NormalClassDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _ClassModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _TypeIdentifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parameters"),
      Core.fieldTypeType = (Core.TypeList _TypeParameter_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "extends"),
      Core.fieldTypeType = (Core.TypeOptional _ClassType_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implements"),
      Core.fieldTypeType = (Core.TypeList _InterfaceType_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _ClassBody_type_}]}))

data ClassModifier = 
  ClassModifierAnnotation Annotation |
  ClassModifierPublic  |
  ClassModifierProtected  |
  ClassModifierPrivate  |
  ClassModifierAbstract  |
  ClassModifierStatic  |
  ClassModifierFinal  |
  ClassModifierStrictfp 
  deriving (Eq, Ord, Read, Show)

_ClassModifier = (Core.Name "hydra/langs/java/syntax.ClassModifier")

_ClassModifier_annotation = (Core.Name "annotation")

_ClassModifier_public = (Core.Name "public")

_ClassModifier_protected = (Core.Name "protected")

_ClassModifier_private = (Core.Name "private")

_ClassModifier_abstract = (Core.Name "abstract")

_ClassModifier_static = (Core.Name "static")

_ClassModifier_final = (Core.Name "final")

_ClassModifier_strictfp = (Core.Name "strictfp")

_ClassModifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassModifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotation"),
      Core.fieldTypeType = _Annotation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "public"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "protected"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "private"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "abstract"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "static"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "final"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "strictfp"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype ClassBody = 
  ClassBody {
    unClassBody :: [ClassBodyDeclarationWithComments]}
  deriving (Eq, Ord, Read, Show)

_ClassBody = (Core.Name "hydra/langs/java/syntax.ClassBody")

_ClassBody_type_ = (Core.TypeList _ClassBodyDeclarationWithComments_type_)

data ClassBodyDeclaration = 
  ClassBodyDeclarationClassMember ClassMemberDeclaration |
  ClassBodyDeclarationInstanceInitializer InstanceInitializer |
  ClassBodyDeclarationStaticInitializer StaticInitializer |
  ClassBodyDeclarationConstructorDeclaration ConstructorDeclaration
  deriving (Eq, Ord, Read, Show)

_ClassBodyDeclaration = (Core.Name "hydra/langs/java/syntax.ClassBodyDeclaration")

_ClassBodyDeclaration_classMember = (Core.Name "classMember")

_ClassBodyDeclaration_instanceInitializer = (Core.Name "instanceInitializer")

_ClassBodyDeclaration_staticInitializer = (Core.Name "staticInitializer")

_ClassBodyDeclaration_constructorDeclaration = (Core.Name "constructorDeclaration")

_ClassBodyDeclaration_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassBodyDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classMember"),
      Core.fieldTypeType = _ClassMemberDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "instanceInitializer"),
      Core.fieldTypeType = _InstanceInitializer_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "staticInitializer"),
      Core.fieldTypeType = _StaticInitializer_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constructorDeclaration"),
      Core.fieldTypeType = _ConstructorDeclaration_type_}]}))

data ClassBodyDeclarationWithComments = 
  ClassBodyDeclarationWithComments {
    classBodyDeclarationWithCommentsValue :: ClassBodyDeclaration,
    classBodyDeclarationWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_ClassBodyDeclarationWithComments = (Core.Name "hydra/langs/java/syntax.ClassBodyDeclarationWithComments")

_ClassBodyDeclarationWithComments_value = (Core.Name "value")

_ClassBodyDeclarationWithComments_comments = (Core.Name "comments")

_ClassBodyDeclarationWithComments_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassBodyDeclarationWithComments"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _ClassBodyDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "comments"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString))}]}))

data ClassMemberDeclaration = 
  ClassMemberDeclarationField FieldDeclaration |
  ClassMemberDeclarationMethod MethodDeclaration |
  ClassMemberDeclarationClass ClassDeclaration |
  ClassMemberDeclarationInterface InterfaceDeclaration |
  ClassMemberDeclarationNone 
  deriving (Eq, Ord, Read, Show)

_ClassMemberDeclaration = (Core.Name "hydra/langs/java/syntax.ClassMemberDeclaration")

_ClassMemberDeclaration_field = (Core.Name "field")

_ClassMemberDeclaration_method = (Core.Name "method")

_ClassMemberDeclaration_class = (Core.Name "class")

_ClassMemberDeclaration_interface = (Core.Name "interface")

_ClassMemberDeclaration_none = (Core.Name "none")

_ClassMemberDeclaration_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassMemberDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "field"),
      Core.fieldTypeType = _FieldDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "method"),
      Core.fieldTypeType = _MethodDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _ClassDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "interface"),
      Core.fieldTypeType = _InterfaceDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "none"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data FieldDeclaration = 
  FieldDeclaration {
    fieldDeclarationModifiers :: [FieldModifier],
    fieldDeclarationUnannType :: UnannType,
    fieldDeclarationVariableDeclarators :: [VariableDeclarator]}
  deriving (Eq, Ord, Read, Show)

_FieldDeclaration = (Core.Name "hydra/langs/java/syntax.FieldDeclaration")

_FieldDeclaration_modifiers = (Core.Name "modifiers")

_FieldDeclaration_unannType = (Core.Name "unannType")

_FieldDeclaration_variableDeclarators = (Core.Name "variableDeclarators")

_FieldDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.FieldDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _FieldModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unannType"),
      Core.fieldTypeType = _UnannType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variableDeclarators"),
      Core.fieldTypeType = (Core.TypeList _VariableDeclarator_type_)}]}))

data FieldModifier = 
  FieldModifierAnnotation Annotation |
  FieldModifierPublic  |
  FieldModifierProtected  |
  FieldModifierPrivate  |
  FieldModifierStatic  |
  FieldModifierFinal  |
  FieldModifierTransient  |
  FieldModifierVolatile 
  deriving (Eq, Ord, Read, Show)

_FieldModifier = (Core.Name "hydra/langs/java/syntax.FieldModifier")

_FieldModifier_annotation = (Core.Name "annotation")

_FieldModifier_public = (Core.Name "public")

_FieldModifier_protected = (Core.Name "protected")

_FieldModifier_private = (Core.Name "private")

_FieldModifier_static = (Core.Name "static")

_FieldModifier_final = (Core.Name "final")

_FieldModifier_transient = (Core.Name "transient")

_FieldModifier_volatile = (Core.Name "volatile")

_FieldModifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.FieldModifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotation"),
      Core.fieldTypeType = _Annotation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "public"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "protected"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "private"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "static"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "final"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "transient"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "volatile"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data VariableDeclarator = 
  VariableDeclarator {
    variableDeclaratorId :: VariableDeclaratorId,
    variableDeclaratorInitializer :: (Maybe VariableInitializer)}
  deriving (Eq, Ord, Read, Show)

_VariableDeclarator = (Core.Name "hydra/langs/java/syntax.VariableDeclarator")

_VariableDeclarator_id = (Core.Name "id")

_VariableDeclarator_initializer = (Core.Name "initializer")

_VariableDeclarator_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.VariableDeclarator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "id"),
      Core.fieldTypeType = _VariableDeclaratorId_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "initializer"),
      Core.fieldTypeType = (Core.TypeOptional _VariableInitializer_type_)}]}))

data VariableDeclaratorId = 
  VariableDeclaratorId {
    variableDeclaratorIdIdentifier :: Identifier,
    variableDeclaratorIdDims :: (Maybe Dims)}
  deriving (Eq, Ord, Read, Show)

_VariableDeclaratorId = (Core.Name "hydra/langs/java/syntax.VariableDeclaratorId")

_VariableDeclaratorId_identifier = (Core.Name "identifier")

_VariableDeclaratorId_dims = (Core.Name "dims")

_VariableDeclaratorId_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.VariableDeclaratorId"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dims"),
      Core.fieldTypeType = (Core.TypeOptional _Dims_type_)}]}))

data VariableInitializer = 
  VariableInitializerExpression Expression |
  VariableInitializerArrayInitializer ArrayInitializer
  deriving (Eq, Ord, Read, Show)

_VariableInitializer = (Core.Name "hydra/langs/java/syntax.VariableInitializer")

_VariableInitializer_expression = (Core.Name "expression")

_VariableInitializer_arrayInitializer = (Core.Name "arrayInitializer")

_VariableInitializer_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.VariableInitializer"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arrayInitializer"),
      Core.fieldTypeType = _ArrayInitializer_type_}]}))

-- | A Type which does not allow annotations
newtype UnannType = 
  UnannType {
    unUnannType :: Type}
  deriving (Eq, Ord, Read, Show)

_UnannType = (Core.Name "hydra/langs/java/syntax.UnannType")

_UnannType_type_ = _Type_type_

-- | A ClassType which does not allow annotations
newtype UnannClassType = 
  UnannClassType {
    unUnannClassType :: ClassType}
  deriving (Eq, Ord, Read, Show)

_UnannClassType = (Core.Name "hydra/langs/java/syntax.UnannClassType")

_UnannClassType_type_ = _ClassType_type_

data MethodDeclaration = 
  MethodDeclaration {
    -- | Note: simple methods cannot have annotations
    methodDeclarationAnnotations :: [Annotation],
    methodDeclarationModifiers :: [MethodModifier],
    methodDeclarationHeader :: MethodHeader,
    methodDeclarationBody :: MethodBody}
  deriving (Eq, Ord, Read, Show)

_MethodDeclaration = (Core.Name "hydra/langs/java/syntax.MethodDeclaration")

_MethodDeclaration_annotations = (Core.Name "annotations")

_MethodDeclaration_modifiers = (Core.Name "modifiers")

_MethodDeclaration_header = (Core.Name "header")

_MethodDeclaration_body = (Core.Name "body")

_MethodDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _MethodModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "header"),
      Core.fieldTypeType = _MethodHeader_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _MethodBody_type_}]}))

data MethodModifier = 
  MethodModifierAnnotation Annotation |
  MethodModifierPublic  |
  MethodModifierProtected  |
  MethodModifierPrivate  |
  MethodModifierAbstract  |
  MethodModifierStatic  |
  MethodModifierFinal  |
  MethodModifierSynchronized  |
  MethodModifierNative  |
  MethodModifierStrictfb 
  deriving (Eq, Ord, Read, Show)

_MethodModifier = (Core.Name "hydra/langs/java/syntax.MethodModifier")

_MethodModifier_annotation = (Core.Name "annotation")

_MethodModifier_public = (Core.Name "public")

_MethodModifier_protected = (Core.Name "protected")

_MethodModifier_private = (Core.Name "private")

_MethodModifier_abstract = (Core.Name "abstract")

_MethodModifier_static = (Core.Name "static")

_MethodModifier_final = (Core.Name "final")

_MethodModifier_synchronized = (Core.Name "synchronized")

_MethodModifier_native = (Core.Name "native")

_MethodModifier_strictfb = (Core.Name "strictfb")

_MethodModifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodModifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotation"),
      Core.fieldTypeType = _Annotation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "public"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "protected"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "private"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "abstract"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "static"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "final"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "synchronized"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "native"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "strictfb"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data MethodHeader = 
  MethodHeader {
    methodHeaderParameters :: [TypeParameter],
    methodHeaderResult :: Result,
    methodHeaderDeclarator :: MethodDeclarator,
    methodHeaderThrows :: (Maybe Throws)}
  deriving (Eq, Ord, Read, Show)

_MethodHeader = (Core.Name "hydra/langs/java/syntax.MethodHeader")

_MethodHeader_parameters = (Core.Name "parameters")

_MethodHeader_result = (Core.Name "result")

_MethodHeader_declarator = (Core.Name "declarator")

_MethodHeader_throws = (Core.Name "throws")

_MethodHeader_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodHeader"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parameters"),
      Core.fieldTypeType = (Core.TypeList _TypeParameter_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "result"),
      Core.fieldTypeType = _Result_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "declarator"),
      Core.fieldTypeType = _MethodDeclarator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "throws"),
      Core.fieldTypeType = (Core.TypeOptional _Throws_type_)}]}))

data Result = 
  ResultType UnannType |
  ResultVoid 
  deriving (Eq, Ord, Read, Show)

_Result = (Core.Name "hydra/langs/java/syntax.Result")

_Result_type = (Core.Name "type")

_Result_void = (Core.Name "void")

_Result_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.Result"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _UnannType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "void"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data MethodDeclarator = 
  MethodDeclarator {
    methodDeclaratorIdentifier :: Identifier,
    methodDeclaratorReceiverParameter :: (Maybe ReceiverParameter),
    methodDeclaratorFormalParameters :: [FormalParameter]}
  deriving (Eq, Ord, Read, Show)

_MethodDeclarator = (Core.Name "hydra/langs/java/syntax.MethodDeclarator")

_MethodDeclarator_identifier = (Core.Name "identifier")

_MethodDeclarator_receiverParameter = (Core.Name "receiverParameter")

_MethodDeclarator_formalParameters = (Core.Name "formalParameters")

_MethodDeclarator_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodDeclarator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "receiverParameter"),
      Core.fieldTypeType = (Core.TypeOptional _ReceiverParameter_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "formalParameters"),
      Core.fieldTypeType = (Core.TypeList _FormalParameter_type_)}]}))

data ReceiverParameter = 
  ReceiverParameter {
    receiverParameterAnnotations :: [Annotation],
    receiverParameterUnannType :: UnannType,
    receiverParameterIdentifier :: (Maybe Identifier)}
  deriving (Eq, Ord, Read, Show)

_ReceiverParameter = (Core.Name "hydra/langs/java/syntax.ReceiverParameter")

_ReceiverParameter_annotations = (Core.Name "annotations")

_ReceiverParameter_unannType = (Core.Name "unannType")

_ReceiverParameter_identifier = (Core.Name "identifier")

_ReceiverParameter_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ReceiverParameter"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unannType"),
      Core.fieldTypeType = _UnannType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = (Core.TypeOptional _Identifier_type_)}]}))

data FormalParameter = 
  FormalParameterSimple FormalParameter_Simple |
  FormalParameterVariableArity VariableArityParameter
  deriving (Eq, Ord, Read, Show)

_FormalParameter = (Core.Name "hydra/langs/java/syntax.FormalParameter")

_FormalParameter_simple = (Core.Name "simple")

_FormalParameter_variableArity = (Core.Name "variableArity")

_FormalParameter_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.FormalParameter"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simple"),
      Core.fieldTypeType = _FormalParameter_Simple_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variableArity"),
      Core.fieldTypeType = _VariableArityParameter_type_}]}))

data FormalParameter_Simple = 
  FormalParameter_Simple {
    formalParameter_SimpleModifiers :: [VariableModifier],
    formalParameter_SimpleType :: UnannType,
    formalParameter_SimpleId :: VariableDeclaratorId}
  deriving (Eq, Ord, Read, Show)

_FormalParameter_Simple = (Core.Name "hydra/langs/java/syntax.FormalParameter.Simple")

_FormalParameter_Simple_modifiers = (Core.Name "modifiers")

_FormalParameter_Simple_type = (Core.Name "type")

_FormalParameter_Simple_id = (Core.Name "id")

_FormalParameter_Simple_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.FormalParameter.Simple"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _VariableModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _UnannType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "id"),
      Core.fieldTypeType = _VariableDeclaratorId_type_}]}))

data VariableArityParameter = 
  VariableArityParameter {
    variableArityParameterModifiers :: VariableModifier,
    variableArityParameterType :: UnannType,
    variableArityParameterAnnotations :: [Annotation],
    variableArityParameterIdentifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_VariableArityParameter = (Core.Name "hydra/langs/java/syntax.VariableArityParameter")

_VariableArityParameter_modifiers = (Core.Name "modifiers")

_VariableArityParameter_type = (Core.Name "type")

_VariableArityParameter_annotations = (Core.Name "annotations")

_VariableArityParameter_identifier = (Core.Name "identifier")

_VariableArityParameter_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.VariableArityParameter"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = _VariableModifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _UnannType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_}]}))

data VariableModifier = 
  VariableModifierAnnotation Annotation |
  VariableModifierFinal 
  deriving (Eq, Ord, Read, Show)

_VariableModifier = (Core.Name "hydra/langs/java/syntax.VariableModifier")

_VariableModifier_annotation = (Core.Name "annotation")

_VariableModifier_final = (Core.Name "final")

_VariableModifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.VariableModifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotation"),
      Core.fieldTypeType = _Annotation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "final"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype Throws = 
  Throws {
    unThrows :: [ExceptionType]}
  deriving (Eq, Ord, Read, Show)

_Throws = (Core.Name "hydra/langs/java/syntax.Throws")

_Throws_type_ = (Core.TypeList _ExceptionType_type_)

data ExceptionType = 
  ExceptionTypeClass ClassType |
  ExceptionTypeVariable TypeVariable
  deriving (Eq, Ord, Read, Show)

_ExceptionType = (Core.Name "hydra/langs/java/syntax.ExceptionType")

_ExceptionType_class = (Core.Name "class")

_ExceptionType_variable = (Core.Name "variable")

_ExceptionType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ExceptionType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _ClassType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _TypeVariable_type_}]}))

data MethodBody = 
  MethodBodyBlock Block |
  MethodBodyNone 
  deriving (Eq, Ord, Read, Show)

_MethodBody = (Core.Name "hydra/langs/java/syntax.MethodBody")

_MethodBody_block = (Core.Name "block")

_MethodBody_none = (Core.Name "none")

_MethodBody_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodBody"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "block"),
      Core.fieldTypeType = _Block_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "none"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype InstanceInitializer = 
  InstanceInitializer {
    unInstanceInitializer :: Block}
  deriving (Eq, Ord, Read, Show)

_InstanceInitializer = (Core.Name "hydra/langs/java/syntax.InstanceInitializer")

_InstanceInitializer_type_ = _Block_type_

newtype StaticInitializer = 
  StaticInitializer {
    unStaticInitializer :: Block}
  deriving (Eq, Ord, Read, Show)

_StaticInitializer = (Core.Name "hydra/langs/java/syntax.StaticInitializer")

_StaticInitializer_type_ = _Block_type_

data ConstructorDeclaration = 
  ConstructorDeclaration {
    constructorDeclarationModifiers :: [ConstructorModifier],
    constructorDeclarationConstructor :: ConstructorDeclarator,
    constructorDeclarationThrows :: (Maybe Throws),
    constructorDeclarationBody :: ConstructorBody}
  deriving (Eq, Ord, Read, Show)

_ConstructorDeclaration = (Core.Name "hydra/langs/java/syntax.ConstructorDeclaration")

_ConstructorDeclaration_modifiers = (Core.Name "modifiers")

_ConstructorDeclaration_constructor = (Core.Name "constructor")

_ConstructorDeclaration_throws = (Core.Name "throws")

_ConstructorDeclaration_body = (Core.Name "body")

_ConstructorDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ConstructorDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _ConstructorModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constructor"),
      Core.fieldTypeType = _ConstructorDeclarator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "throws"),
      Core.fieldTypeType = (Core.TypeOptional _Throws_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _ConstructorBody_type_}]}))

data ConstructorModifier = 
  ConstructorModifierAnnotation Annotation |
  ConstructorModifierPublic  |
  ConstructorModifierProtected  |
  ConstructorModifierPrivate 
  deriving (Eq, Ord, Read, Show)

_ConstructorModifier = (Core.Name "hydra/langs/java/syntax.ConstructorModifier")

_ConstructorModifier_annotation = (Core.Name "annotation")

_ConstructorModifier_public = (Core.Name "public")

_ConstructorModifier_protected = (Core.Name "protected")

_ConstructorModifier_private = (Core.Name "private")

_ConstructorModifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ConstructorModifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotation"),
      Core.fieldTypeType = _Annotation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "public"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "protected"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "private"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data ConstructorDeclarator = 
  ConstructorDeclarator {
    constructorDeclaratorParameters :: [TypeParameter],
    constructorDeclaratorName :: SimpleTypeName,
    constructorDeclaratorReceiverParameter :: (Maybe ReceiverParameter),
    constructorDeclaratorFormalParameters :: [FormalParameter]}
  deriving (Eq, Ord, Read, Show)

_ConstructorDeclarator = (Core.Name "hydra/langs/java/syntax.ConstructorDeclarator")

_ConstructorDeclarator_parameters = (Core.Name "parameters")

_ConstructorDeclarator_name = (Core.Name "name")

_ConstructorDeclarator_receiverParameter = (Core.Name "receiverParameter")

_ConstructorDeclarator_formalParameters = (Core.Name "formalParameters")

_ConstructorDeclarator_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ConstructorDeclarator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parameters"),
      Core.fieldTypeType = (Core.TypeList _TypeParameter_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _SimpleTypeName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "receiverParameter"),
      Core.fieldTypeType = (Core.TypeOptional _ReceiverParameter_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "formalParameters"),
      Core.fieldTypeType = (Core.TypeList _FormalParameter_type_)}]}))

newtype SimpleTypeName = 
  SimpleTypeName {
    unSimpleTypeName :: TypeIdentifier}
  deriving (Eq, Ord, Read, Show)

_SimpleTypeName = (Core.Name "hydra/langs/java/syntax.SimpleTypeName")

_SimpleTypeName_type_ = _TypeIdentifier_type_

data ConstructorBody = 
  ConstructorBody {
    constructorBodyInvocation :: (Maybe ExplicitConstructorInvocation),
    constructorBodyStatements :: [BlockStatement]}
  deriving (Eq, Ord, Read, Show)

_ConstructorBody = (Core.Name "hydra/langs/java/syntax.ConstructorBody")

_ConstructorBody_invocation = (Core.Name "invocation")

_ConstructorBody_statements = (Core.Name "statements")

_ConstructorBody_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ConstructorBody"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "invocation"),
      Core.fieldTypeType = (Core.TypeOptional _ExplicitConstructorInvocation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "statements"),
      Core.fieldTypeType = (Core.TypeList _BlockStatement_type_)}]}))

data ExplicitConstructorInvocation = 
  ExplicitConstructorInvocation {
    explicitConstructorInvocationTypeArguments :: [TypeArgument],
    explicitConstructorInvocationArguments :: [Expression],
    explicitConstructorInvocationVariant :: ExplicitConstructorInvocation_Variant}
  deriving (Eq, Ord, Read, Show)

_ExplicitConstructorInvocation = (Core.Name "hydra/langs/java/syntax.ExplicitConstructorInvocation")

_ExplicitConstructorInvocation_typeArguments = (Core.Name "typeArguments")

_ExplicitConstructorInvocation_arguments = (Core.Name "arguments")

_ExplicitConstructorInvocation_variant = (Core.Name "variant")

_ExplicitConstructorInvocation_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ExplicitConstructorInvocation"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeArguments"),
      Core.fieldTypeType = (Core.TypeList _TypeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variant"),
      Core.fieldTypeType = _ExplicitConstructorInvocation_Variant_type_}]}))

data ExplicitConstructorInvocation_Variant = 
  ExplicitConstructorInvocation_VariantThis  |
  ExplicitConstructorInvocation_VariantSuper (Maybe ExpressionName) |
  ExplicitConstructorInvocation_VariantPrimary Primary
  deriving (Eq, Ord, Read, Show)

_ExplicitConstructorInvocation_Variant = (Core.Name "hydra/langs/java/syntax.ExplicitConstructorInvocation.Variant")

_ExplicitConstructorInvocation_Variant_this = (Core.Name "this")

_ExplicitConstructorInvocation_Variant_super = (Core.Name "super")

_ExplicitConstructorInvocation_Variant_primary = (Core.Name "primary")

_ExplicitConstructorInvocation_Variant_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ExplicitConstructorInvocation.Variant"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "this"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "super"),
      Core.fieldTypeType = (Core.TypeOptional _ExpressionName_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primary"),
      Core.fieldTypeType = _Primary_type_}]}))

data EnumDeclaration = 
  EnumDeclaration {
    enumDeclarationModifiers :: [ClassModifier],
    enumDeclarationIdentifier :: TypeIdentifier,
    enumDeclarationImplements :: [InterfaceType],
    enumDeclarationBody :: EnumBody}
  deriving (Eq, Ord, Read, Show)

_EnumDeclaration = (Core.Name "hydra/langs/java/syntax.EnumDeclaration")

_EnumDeclaration_modifiers = (Core.Name "modifiers")

_EnumDeclaration_identifier = (Core.Name "identifier")

_EnumDeclaration_implements = (Core.Name "implements")

_EnumDeclaration_body = (Core.Name "body")

_EnumDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.EnumDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _ClassModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _TypeIdentifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implements"),
      Core.fieldTypeType = (Core.TypeList _InterfaceType_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _EnumBody_type_}]}))

newtype EnumBody = 
  EnumBody {
    unEnumBody :: [EnumBody_Element]}
  deriving (Eq, Ord, Read, Show)

_EnumBody = (Core.Name "hydra/langs/java/syntax.EnumBody")

_EnumBody_type_ = (Core.TypeList _EnumBody_Element_type_)

data EnumBody_Element = 
  EnumBody_Element {
    enumBody_ElementConstants :: [EnumConstant],
    enumBody_ElementBodyDeclarations :: [ClassBodyDeclaration]}
  deriving (Eq, Ord, Read, Show)

_EnumBody_Element = (Core.Name "hydra/langs/java/syntax.EnumBody.Element")

_EnumBody_Element_constants = (Core.Name "constants")

_EnumBody_Element_bodyDeclarations = (Core.Name "bodyDeclarations")

_EnumBody_Element_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.EnumBody.Element"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constants"),
      Core.fieldTypeType = (Core.TypeList _EnumConstant_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bodyDeclarations"),
      Core.fieldTypeType = (Core.TypeList _ClassBodyDeclaration_type_)}]}))

data EnumConstant = 
  EnumConstant {
    enumConstantModifiers :: [EnumConstantModifier],
    enumConstantIdentifier :: Identifier,
    enumConstantArguments :: [[Expression]],
    enumConstantBody :: (Maybe ClassBody)}
  deriving (Eq, Ord, Read, Show)

_EnumConstant = (Core.Name "hydra/langs/java/syntax.EnumConstant")

_EnumConstant_modifiers = (Core.Name "modifiers")

_EnumConstant_identifier = (Core.Name "identifier")

_EnumConstant_arguments = (Core.Name "arguments")

_EnumConstant_body = (Core.Name "body")

_EnumConstant_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.EnumConstant"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _EnumConstantModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = (Core.TypeList (Core.TypeList _Expression_type_))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = (Core.TypeOptional _ClassBody_type_)}]}))

newtype EnumConstantModifier = 
  EnumConstantModifier {
    unEnumConstantModifier :: Annotation}
  deriving (Eq, Ord, Read, Show)

_EnumConstantModifier = (Core.Name "hydra/langs/java/syntax.EnumConstantModifier")

_EnumConstantModifier_type_ = _Annotation_type_

data InterfaceDeclaration = 
  InterfaceDeclarationNormalInterface NormalInterfaceDeclaration |
  InterfaceDeclarationAnnotationType AnnotationTypeDeclaration
  deriving (Eq, Ord, Read, Show)

_InterfaceDeclaration = (Core.Name "hydra/langs/java/syntax.InterfaceDeclaration")

_InterfaceDeclaration_normalInterface = (Core.Name "normalInterface")

_InterfaceDeclaration_annotationType = (Core.Name "annotationType")

_InterfaceDeclaration_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.InterfaceDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "normalInterface"),
      Core.fieldTypeType = _NormalInterfaceDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotationType"),
      Core.fieldTypeType = _AnnotationTypeDeclaration_type_}]}))

data NormalInterfaceDeclaration = 
  NormalInterfaceDeclaration {
    normalInterfaceDeclarationModifiers :: [InterfaceModifier],
    normalInterfaceDeclarationIdentifier :: TypeIdentifier,
    normalInterfaceDeclarationParameters :: [TypeParameter],
    normalInterfaceDeclarationExtends :: [InterfaceType],
    normalInterfaceDeclarationBody :: InterfaceBody}
  deriving (Eq, Ord, Read, Show)

_NormalInterfaceDeclaration = (Core.Name "hydra/langs/java/syntax.NormalInterfaceDeclaration")

_NormalInterfaceDeclaration_modifiers = (Core.Name "modifiers")

_NormalInterfaceDeclaration_identifier = (Core.Name "identifier")

_NormalInterfaceDeclaration_parameters = (Core.Name "parameters")

_NormalInterfaceDeclaration_extends = (Core.Name "extends")

_NormalInterfaceDeclaration_body = (Core.Name "body")

_NormalInterfaceDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.NormalInterfaceDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _InterfaceModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _TypeIdentifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parameters"),
      Core.fieldTypeType = (Core.TypeList _TypeParameter_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "extends"),
      Core.fieldTypeType = (Core.TypeList _InterfaceType_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _InterfaceBody_type_}]}))

data InterfaceModifier = 
  InterfaceModifierAnnotation Annotation |
  InterfaceModifierPublic  |
  InterfaceModifierProtected  |
  InterfaceModifierPrivate  |
  InterfaceModifierAbstract  |
  InterfaceModifierStatic  |
  InterfaceModifierStrictfb 
  deriving (Eq, Ord, Read, Show)

_InterfaceModifier = (Core.Name "hydra/langs/java/syntax.InterfaceModifier")

_InterfaceModifier_annotation = (Core.Name "annotation")

_InterfaceModifier_public = (Core.Name "public")

_InterfaceModifier_protected = (Core.Name "protected")

_InterfaceModifier_private = (Core.Name "private")

_InterfaceModifier_abstract = (Core.Name "abstract")

_InterfaceModifier_static = (Core.Name "static")

_InterfaceModifier_strictfb = (Core.Name "strictfb")

_InterfaceModifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.InterfaceModifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotation"),
      Core.fieldTypeType = _Annotation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "public"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "protected"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "private"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "abstract"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "static"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "strictfb"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype InterfaceBody = 
  InterfaceBody {
    unInterfaceBody :: [InterfaceMemberDeclaration]}
  deriving (Eq, Ord, Read, Show)

_InterfaceBody = (Core.Name "hydra/langs/java/syntax.InterfaceBody")

_InterfaceBody_type_ = (Core.TypeList _InterfaceMemberDeclaration_type_)

data InterfaceMemberDeclaration = 
  InterfaceMemberDeclarationConstant ConstantDeclaration |
  InterfaceMemberDeclarationInterfaceMethod InterfaceMethodDeclaration |
  InterfaceMemberDeclarationClass ClassDeclaration |
  InterfaceMemberDeclarationInterface InterfaceDeclaration
  deriving (Eq, Ord, Read, Show)

_InterfaceMemberDeclaration = (Core.Name "hydra/langs/java/syntax.InterfaceMemberDeclaration")

_InterfaceMemberDeclaration_constant = (Core.Name "constant")

_InterfaceMemberDeclaration_interfaceMethod = (Core.Name "interfaceMethod")

_InterfaceMemberDeclaration_class = (Core.Name "class")

_InterfaceMemberDeclaration_interface = (Core.Name "interface")

_InterfaceMemberDeclaration_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.InterfaceMemberDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constant"),
      Core.fieldTypeType = _ConstantDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "interfaceMethod"),
      Core.fieldTypeType = _InterfaceMethodDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _ClassDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "interface"),
      Core.fieldTypeType = _InterfaceDeclaration_type_}]}))

data ConstantDeclaration = 
  ConstantDeclaration {
    constantDeclarationModifiers :: [ConstantModifier],
    constantDeclarationType :: UnannType,
    constantDeclarationVariables :: [VariableDeclarator]}
  deriving (Eq, Ord, Read, Show)

_ConstantDeclaration = (Core.Name "hydra/langs/java/syntax.ConstantDeclaration")

_ConstantDeclaration_modifiers = (Core.Name "modifiers")

_ConstantDeclaration_type = (Core.Name "type")

_ConstantDeclaration_variables = (Core.Name "variables")

_ConstantDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ConstantDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _ConstantModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _UnannType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variables"),
      Core.fieldTypeType = (Core.TypeList _VariableDeclarator_type_)}]}))

data ConstantModifier = 
  ConstantModifierAnnotation Annotation |
  ConstantModifierPublic  |
  ConstantModifierStatic  |
  ConstantModifierFinal 
  deriving (Eq, Ord, Read, Show)

_ConstantModifier = (Core.Name "hydra/langs/java/syntax.ConstantModifier")

_ConstantModifier_annotation = (Core.Name "annotation")

_ConstantModifier_public = (Core.Name "public")

_ConstantModifier_static = (Core.Name "static")

_ConstantModifier_final = (Core.Name "final")

_ConstantModifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ConstantModifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotation"),
      Core.fieldTypeType = _Annotation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "public"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "static"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "final"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data InterfaceMethodDeclaration = 
  InterfaceMethodDeclaration {
    interfaceMethodDeclarationModifiers :: [InterfaceMethodModifier],
    interfaceMethodDeclarationHeader :: MethodHeader,
    interfaceMethodDeclarationBody :: MethodBody}
  deriving (Eq, Ord, Read, Show)

_InterfaceMethodDeclaration = (Core.Name "hydra/langs/java/syntax.InterfaceMethodDeclaration")

_InterfaceMethodDeclaration_modifiers = (Core.Name "modifiers")

_InterfaceMethodDeclaration_header = (Core.Name "header")

_InterfaceMethodDeclaration_body = (Core.Name "body")

_InterfaceMethodDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.InterfaceMethodDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _InterfaceMethodModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "header"),
      Core.fieldTypeType = _MethodHeader_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _MethodBody_type_}]}))

data InterfaceMethodModifier = 
  InterfaceMethodModifierAnnotation Annotation |
  InterfaceMethodModifierPublic  |
  InterfaceMethodModifierPrivate  |
  InterfaceMethodModifierAbstract  |
  InterfaceMethodModifierDefault  |
  InterfaceMethodModifierStatic  |
  InterfaceMethodModifierStrictfp 
  deriving (Eq, Ord, Read, Show)

_InterfaceMethodModifier = (Core.Name "hydra/langs/java/syntax.InterfaceMethodModifier")

_InterfaceMethodModifier_annotation = (Core.Name "annotation")

_InterfaceMethodModifier_public = (Core.Name "public")

_InterfaceMethodModifier_private = (Core.Name "private")

_InterfaceMethodModifier_abstract = (Core.Name "abstract")

_InterfaceMethodModifier_default = (Core.Name "default")

_InterfaceMethodModifier_static = (Core.Name "static")

_InterfaceMethodModifier_strictfp = (Core.Name "strictfp")

_InterfaceMethodModifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.InterfaceMethodModifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotation"),
      Core.fieldTypeType = _Annotation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "public"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "private"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "abstract"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "default"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "static"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "strictfp"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data AnnotationTypeDeclaration = 
  AnnotationTypeDeclaration {
    annotationTypeDeclarationModifiers :: [InterfaceModifier],
    annotationTypeDeclarationIdentifier :: TypeIdentifier,
    annotationTypeDeclarationBody :: AnnotationTypeBody}
  deriving (Eq, Ord, Read, Show)

_AnnotationTypeDeclaration = (Core.Name "hydra/langs/java/syntax.AnnotationTypeDeclaration")

_AnnotationTypeDeclaration_modifiers = (Core.Name "modifiers")

_AnnotationTypeDeclaration_identifier = (Core.Name "identifier")

_AnnotationTypeDeclaration_body = (Core.Name "body")

_AnnotationTypeDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.AnnotationTypeDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _InterfaceModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _TypeIdentifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _AnnotationTypeBody_type_}]}))

newtype AnnotationTypeBody = 
  AnnotationTypeBody {
    unAnnotationTypeBody :: [[AnnotationTypeMemberDeclaration]]}
  deriving (Eq, Ord, Read, Show)

_AnnotationTypeBody = (Core.Name "hydra/langs/java/syntax.AnnotationTypeBody")

_AnnotationTypeBody_type_ = (Core.TypeList (Core.TypeList _AnnotationTypeMemberDeclaration_type_))

data AnnotationTypeMemberDeclaration = 
  AnnotationTypeMemberDeclarationAnnotationType AnnotationTypeElementDeclaration |
  AnnotationTypeMemberDeclarationConstant ConstantDeclaration |
  AnnotationTypeMemberDeclarationClass ClassDeclaration |
  AnnotationTypeMemberDeclarationInterface InterfaceDeclaration
  deriving (Eq, Ord, Read, Show)

_AnnotationTypeMemberDeclaration = (Core.Name "hydra/langs/java/syntax.AnnotationTypeMemberDeclaration")

_AnnotationTypeMemberDeclaration_annotationType = (Core.Name "annotationType")

_AnnotationTypeMemberDeclaration_constant = (Core.Name "constant")

_AnnotationTypeMemberDeclaration_class = (Core.Name "class")

_AnnotationTypeMemberDeclaration_interface = (Core.Name "interface")

_AnnotationTypeMemberDeclaration_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.AnnotationTypeMemberDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotationType"),
      Core.fieldTypeType = _AnnotationTypeElementDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constant"),
      Core.fieldTypeType = _ConstantDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _ClassDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "interface"),
      Core.fieldTypeType = _InterfaceDeclaration_type_}]}))

data AnnotationTypeElementDeclaration = 
  AnnotationTypeElementDeclaration {
    annotationTypeElementDeclarationModifiers :: [AnnotationTypeElementModifier],
    annotationTypeElementDeclarationType :: UnannType,
    annotationTypeElementDeclarationIdentifier :: Identifier,
    annotationTypeElementDeclarationDims :: (Maybe Dims),
    annotationTypeElementDeclarationDefault :: (Maybe DefaultValue)}
  deriving (Eq, Ord, Read, Show)

_AnnotationTypeElementDeclaration = (Core.Name "hydra/langs/java/syntax.AnnotationTypeElementDeclaration")

_AnnotationTypeElementDeclaration_modifiers = (Core.Name "modifiers")

_AnnotationTypeElementDeclaration_type = (Core.Name "type")

_AnnotationTypeElementDeclaration_identifier = (Core.Name "identifier")

_AnnotationTypeElementDeclaration_dims = (Core.Name "dims")

_AnnotationTypeElementDeclaration_default = (Core.Name "default")

_AnnotationTypeElementDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.AnnotationTypeElementDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _AnnotationTypeElementModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _UnannType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dims"),
      Core.fieldTypeType = (Core.TypeOptional _Dims_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "default"),
      Core.fieldTypeType = (Core.TypeOptional _DefaultValue_type_)}]}))

data AnnotationTypeElementModifier = 
  AnnotationTypeElementModifierPublic Annotation |
  AnnotationTypeElementModifierAbstract 
  deriving (Eq, Ord, Read, Show)

_AnnotationTypeElementModifier = (Core.Name "hydra/langs/java/syntax.AnnotationTypeElementModifier")

_AnnotationTypeElementModifier_public = (Core.Name "public")

_AnnotationTypeElementModifier_abstract = (Core.Name "abstract")

_AnnotationTypeElementModifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.AnnotationTypeElementModifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "public"),
      Core.fieldTypeType = _Annotation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "abstract"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype DefaultValue = 
  DefaultValue {
    unDefaultValue :: ElementValue}
  deriving (Eq, Ord, Read, Show)

_DefaultValue = (Core.Name "hydra/langs/java/syntax.DefaultValue")

_DefaultValue_type_ = _ElementValue_type_

data Annotation = 
  AnnotationNormal NormalAnnotation |
  AnnotationMarker MarkerAnnotation |
  AnnotationSingleElement SingleElementAnnotation
  deriving (Eq, Ord, Read, Show)

_Annotation = (Core.Name "hydra/langs/java/syntax.Annotation")

_Annotation_normal = (Core.Name "normal")

_Annotation_marker = (Core.Name "marker")

_Annotation_singleElement = (Core.Name "singleElement")

_Annotation_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.Annotation"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "normal"),
      Core.fieldTypeType = _NormalAnnotation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "marker"),
      Core.fieldTypeType = _MarkerAnnotation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "singleElement"),
      Core.fieldTypeType = _SingleElementAnnotation_type_}]}))

data NormalAnnotation = 
  NormalAnnotation {
    normalAnnotationTypeName :: TypeName,
    normalAnnotationPairs :: [ElementValuePair]}
  deriving (Eq, Ord, Read, Show)

_NormalAnnotation = (Core.Name "hydra/langs/java/syntax.NormalAnnotation")

_NormalAnnotation_typeName = (Core.Name "typeName")

_NormalAnnotation_pairs = (Core.Name "pairs")

_NormalAnnotation_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.NormalAnnotation"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeName"),
      Core.fieldTypeType = _TypeName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pairs"),
      Core.fieldTypeType = (Core.TypeList _ElementValuePair_type_)}]}))

data ElementValuePair = 
  ElementValuePair {
    elementValuePairKey :: Identifier,
    elementValuePairValue :: ElementValue}
  deriving (Eq, Ord, Read, Show)

_ElementValuePair = (Core.Name "hydra/langs/java/syntax.ElementValuePair")

_ElementValuePair_key = (Core.Name "key")

_ElementValuePair_value = (Core.Name "value")

_ElementValuePair_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ElementValuePair"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = _Identifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _ElementValue_type_}]}))

data ElementValue = 
  ElementValueConditionalExpression ConditionalExpression |
  ElementValueElementValueArrayInitializer ElementValueArrayInitializer |
  ElementValueAnnotation Annotation
  deriving (Eq, Ord, Read, Show)

_ElementValue = (Core.Name "hydra/langs/java/syntax.ElementValue")

_ElementValue_conditionalExpression = (Core.Name "conditionalExpression")

_ElementValue_elementValueArrayInitializer = (Core.Name "elementValueArrayInitializer")

_ElementValue_annotation = (Core.Name "annotation")

_ElementValue_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ElementValue"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "conditionalExpression"),
      Core.fieldTypeType = _ConditionalExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "elementValueArrayInitializer"),
      Core.fieldTypeType = _ElementValueArrayInitializer_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotation"),
      Core.fieldTypeType = _Annotation_type_}]}))

newtype ElementValueArrayInitializer = 
  ElementValueArrayInitializer {
    unElementValueArrayInitializer :: [ElementValue]}
  deriving (Eq, Ord, Read, Show)

_ElementValueArrayInitializer = (Core.Name "hydra/langs/java/syntax.ElementValueArrayInitializer")

_ElementValueArrayInitializer_type_ = (Core.TypeList _ElementValue_type_)

newtype MarkerAnnotation = 
  MarkerAnnotation {
    unMarkerAnnotation :: TypeName}
  deriving (Eq, Ord, Read, Show)

_MarkerAnnotation = (Core.Name "hydra/langs/java/syntax.MarkerAnnotation")

_MarkerAnnotation_type_ = _TypeName_type_

data SingleElementAnnotation = 
  SingleElementAnnotation {
    singleElementAnnotationName :: TypeName,
    singleElementAnnotationValue :: (Maybe ElementValue)}
  deriving (Eq, Ord, Read, Show)

_SingleElementAnnotation = (Core.Name "hydra/langs/java/syntax.SingleElementAnnotation")

_SingleElementAnnotation_name = (Core.Name "name")

_SingleElementAnnotation_value = (Core.Name "value")

_SingleElementAnnotation_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.SingleElementAnnotation"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _TypeName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeOptional _ElementValue_type_)}]}))

newtype ArrayInitializer = 
  ArrayInitializer {
    unArrayInitializer :: [[VariableInitializer]]}
  deriving (Eq, Ord, Read, Show)

_ArrayInitializer = (Core.Name "hydra/langs/java/syntax.ArrayInitializer")

_ArrayInitializer_type_ = (Core.TypeList (Core.TypeList _VariableInitializer_type_))

newtype Block = 
  Block {
    unBlock :: [BlockStatement]}
  deriving (Eq, Ord, Read, Show)

_Block = (Core.Name "hydra/langs/java/syntax.Block")

_Block_type_ = (Core.TypeList _BlockStatement_type_)

data BlockStatement = 
  BlockStatementLocalVariableDeclaration LocalVariableDeclarationStatement |
  BlockStatementClass ClassDeclaration |
  BlockStatementStatement Statement
  deriving (Eq, Ord, Read, Show)

_BlockStatement = (Core.Name "hydra/langs/java/syntax.BlockStatement")

_BlockStatement_localVariableDeclaration = (Core.Name "localVariableDeclaration")

_BlockStatement_class = (Core.Name "class")

_BlockStatement_statement = (Core.Name "statement")

_BlockStatement_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.BlockStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "localVariableDeclaration"),
      Core.fieldTypeType = _LocalVariableDeclarationStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _ClassDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "statement"),
      Core.fieldTypeType = _Statement_type_}]}))

newtype LocalVariableDeclarationStatement = 
  LocalVariableDeclarationStatement {
    unLocalVariableDeclarationStatement :: LocalVariableDeclaration}
  deriving (Eq, Ord, Read, Show)

_LocalVariableDeclarationStatement = (Core.Name "hydra/langs/java/syntax.LocalVariableDeclarationStatement")

_LocalVariableDeclarationStatement_type_ = _LocalVariableDeclaration_type_

data LocalVariableDeclaration = 
  LocalVariableDeclaration {
    localVariableDeclarationModifiers :: [VariableModifier],
    localVariableDeclarationType :: LocalVariableType,
    localVariableDeclarationDeclarators :: [VariableDeclarator]}
  deriving (Eq, Ord, Read, Show)

_LocalVariableDeclaration = (Core.Name "hydra/langs/java/syntax.LocalVariableDeclaration")

_LocalVariableDeclaration_modifiers = (Core.Name "modifiers")

_LocalVariableDeclaration_type = (Core.Name "type")

_LocalVariableDeclaration_declarators = (Core.Name "declarators")

_LocalVariableDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.LocalVariableDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _VariableModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _LocalVariableType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "declarators"),
      Core.fieldTypeType = (Core.TypeList _VariableDeclarator_type_)}]}))

data LocalVariableType = 
  LocalVariableTypeType UnannType |
  LocalVariableTypeVar 
  deriving (Eq, Ord, Read, Show)

_LocalVariableType = (Core.Name "hydra/langs/java/syntax.LocalVariableType")

_LocalVariableType_type = (Core.Name "type")

_LocalVariableType_var = (Core.Name "var")

_LocalVariableType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.LocalVariableType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _UnannType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "var"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data Statement = 
  StatementWithoutTrailing StatementWithoutTrailingSubstatement |
  StatementLabeled LabeledStatement |
  StatementIfThen IfThenStatement |
  StatementIfThenElse IfThenElseStatement |
  StatementWhile WhileStatement |
  StatementFor ForStatement
  deriving (Eq, Ord, Read, Show)

_Statement = (Core.Name "hydra/langs/java/syntax.Statement")

_Statement_withoutTrailing = (Core.Name "withoutTrailing")

_Statement_labeled = (Core.Name "labeled")

_Statement_ifThen = (Core.Name "ifThen")

_Statement_ifThenElse = (Core.Name "ifThenElse")

_Statement_while = (Core.Name "while")

_Statement_for = (Core.Name "for")

_Statement_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.Statement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withoutTrailing"),
      Core.fieldTypeType = _StatementWithoutTrailingSubstatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "labeled"),
      Core.fieldTypeType = _LabeledStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ifThen"),
      Core.fieldTypeType = _IfThenStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ifThenElse"),
      Core.fieldTypeType = _IfThenElseStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "while"),
      Core.fieldTypeType = _WhileStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "for"),
      Core.fieldTypeType = _ForStatement_type_}]}))

data StatementNoShortIf = 
  StatementNoShortIfWithoutTrailing StatementWithoutTrailingSubstatement |
  StatementNoShortIfLabeled LabeledStatementNoShortIf |
  StatementNoShortIfIfThenElse IfThenElseStatementNoShortIf |
  StatementNoShortIfWhile WhileStatementNoShortIf |
  StatementNoShortIfFor ForStatementNoShortIf
  deriving (Eq, Ord, Read, Show)

_StatementNoShortIf = (Core.Name "hydra/langs/java/syntax.StatementNoShortIf")

_StatementNoShortIf_withoutTrailing = (Core.Name "withoutTrailing")

_StatementNoShortIf_labeled = (Core.Name "labeled")

_StatementNoShortIf_ifThenElse = (Core.Name "ifThenElse")

_StatementNoShortIf_while = (Core.Name "while")

_StatementNoShortIf_for = (Core.Name "for")

_StatementNoShortIf_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.StatementNoShortIf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withoutTrailing"),
      Core.fieldTypeType = _StatementWithoutTrailingSubstatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "labeled"),
      Core.fieldTypeType = _LabeledStatementNoShortIf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ifThenElse"),
      Core.fieldTypeType = _IfThenElseStatementNoShortIf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "while"),
      Core.fieldTypeType = _WhileStatementNoShortIf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "for"),
      Core.fieldTypeType = _ForStatementNoShortIf_type_}]}))

data StatementWithoutTrailingSubstatement = 
  StatementWithoutTrailingSubstatementBlock Block |
  StatementWithoutTrailingSubstatementEmpty EmptyStatement |
  StatementWithoutTrailingSubstatementExpression ExpressionStatement |
  StatementWithoutTrailingSubstatementAssert AssertStatement |
  StatementWithoutTrailingSubstatementSwitch SwitchStatement |
  StatementWithoutTrailingSubstatementDo DoStatement |
  StatementWithoutTrailingSubstatementBreak BreakStatement |
  StatementWithoutTrailingSubstatementContinue ContinueStatement |
  StatementWithoutTrailingSubstatementReturn ReturnStatement |
  StatementWithoutTrailingSubstatementSynchronized SynchronizedStatement |
  StatementWithoutTrailingSubstatementThrow ThrowStatement |
  StatementWithoutTrailingSubstatementTry TryStatement
  deriving (Eq, Ord, Read, Show)

_StatementWithoutTrailingSubstatement = (Core.Name "hydra/langs/java/syntax.StatementWithoutTrailingSubstatement")

_StatementWithoutTrailingSubstatement_block = (Core.Name "block")

_StatementWithoutTrailingSubstatement_empty = (Core.Name "empty")

_StatementWithoutTrailingSubstatement_expression = (Core.Name "expression")

_StatementWithoutTrailingSubstatement_assert = (Core.Name "assert")

_StatementWithoutTrailingSubstatement_switch = (Core.Name "switch")

_StatementWithoutTrailingSubstatement_do = (Core.Name "do")

_StatementWithoutTrailingSubstatement_break = (Core.Name "break")

_StatementWithoutTrailingSubstatement_continue = (Core.Name "continue")

_StatementWithoutTrailingSubstatement_return = (Core.Name "return")

_StatementWithoutTrailingSubstatement_synchronized = (Core.Name "synchronized")

_StatementWithoutTrailingSubstatement_throw = (Core.Name "throw")

_StatementWithoutTrailingSubstatement_try = (Core.Name "try")

_StatementWithoutTrailingSubstatement_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.StatementWithoutTrailingSubstatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "block"),
      Core.fieldTypeType = _Block_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "empty"),
      Core.fieldTypeType = _EmptyStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _ExpressionStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "assert"),
      Core.fieldTypeType = _AssertStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "switch"),
      Core.fieldTypeType = _SwitchStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "do"),
      Core.fieldTypeType = _DoStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "break"),
      Core.fieldTypeType = _BreakStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "continue"),
      Core.fieldTypeType = _ContinueStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "return"),
      Core.fieldTypeType = _ReturnStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "synchronized"),
      Core.fieldTypeType = _SynchronizedStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "throw"),
      Core.fieldTypeType = _ThrowStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "try"),
      Core.fieldTypeType = _TryStatement_type_}]}))

data EmptyStatement = 
  EmptyStatement {}
  deriving (Eq, Ord, Read, Show)

_EmptyStatement = (Core.Name "hydra/langs/java/syntax.EmptyStatement")

_EmptyStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data LabeledStatement = 
  LabeledStatement {
    labeledStatementIdentifier :: Identifier,
    labeledStatementStatement :: Statement}
  deriving (Eq, Ord, Read, Show)

_LabeledStatement = (Core.Name "hydra/langs/java/syntax.LabeledStatement")

_LabeledStatement_identifier = (Core.Name "identifier")

_LabeledStatement_statement = (Core.Name "statement")

_LabeledStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.LabeledStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "statement"),
      Core.fieldTypeType = _Statement_type_}]}))

data LabeledStatementNoShortIf = 
  LabeledStatementNoShortIf {
    labeledStatementNoShortIfIdentifier :: Identifier,
    labeledStatementNoShortIfStatement :: StatementNoShortIf}
  deriving (Eq, Ord, Read, Show)

_LabeledStatementNoShortIf = (Core.Name "hydra/langs/java/syntax.LabeledStatementNoShortIf")

_LabeledStatementNoShortIf_identifier = (Core.Name "identifier")

_LabeledStatementNoShortIf_statement = (Core.Name "statement")

_LabeledStatementNoShortIf_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.LabeledStatementNoShortIf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "statement"),
      Core.fieldTypeType = _StatementNoShortIf_type_}]}))

newtype ExpressionStatement = 
  ExpressionStatement {
    unExpressionStatement :: StatementExpression}
  deriving (Eq, Ord, Read, Show)

_ExpressionStatement = (Core.Name "hydra/langs/java/syntax.ExpressionStatement")

_ExpressionStatement_type_ = _StatementExpression_type_

data StatementExpression = 
  StatementExpressionAssignment Assignment |
  StatementExpressionPreIncrement PreIncrementExpression |
  StatementExpressionPreDecrement PreDecrementExpression |
  StatementExpressionPostIncrement PostIncrementExpression |
  StatementExpressionPostDecrement PostDecrementExpression |
  StatementExpressionMethodInvocation MethodInvocation |
  StatementExpressionClassInstanceCreation ClassInstanceCreationExpression
  deriving (Eq, Ord, Read, Show)

_StatementExpression = (Core.Name "hydra/langs/java/syntax.StatementExpression")

_StatementExpression_assignment = (Core.Name "assignment")

_StatementExpression_preIncrement = (Core.Name "preIncrement")

_StatementExpression_preDecrement = (Core.Name "preDecrement")

_StatementExpression_postIncrement = (Core.Name "postIncrement")

_StatementExpression_postDecrement = (Core.Name "postDecrement")

_StatementExpression_methodInvocation = (Core.Name "methodInvocation")

_StatementExpression_classInstanceCreation = (Core.Name "classInstanceCreation")

_StatementExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.StatementExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "assignment"),
      Core.fieldTypeType = _Assignment_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "preIncrement"),
      Core.fieldTypeType = _PreIncrementExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "preDecrement"),
      Core.fieldTypeType = _PreDecrementExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "postIncrement"),
      Core.fieldTypeType = _PostIncrementExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "postDecrement"),
      Core.fieldTypeType = _PostDecrementExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "methodInvocation"),
      Core.fieldTypeType = _MethodInvocation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classInstanceCreation"),
      Core.fieldTypeType = _ClassInstanceCreationExpression_type_}]}))

data IfThenStatement = 
  IfThenStatement {
    ifThenStatementExpression :: Expression,
    ifThenStatementStatement :: Statement}
  deriving (Eq, Ord, Read, Show)

_IfThenStatement = (Core.Name "hydra/langs/java/syntax.IfThenStatement")

_IfThenStatement_expression = (Core.Name "expression")

_IfThenStatement_statement = (Core.Name "statement")

_IfThenStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.IfThenStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "statement"),
      Core.fieldTypeType = _Statement_type_}]}))

data IfThenElseStatement = 
  IfThenElseStatement {
    ifThenElseStatementCond :: (Maybe Expression),
    ifThenElseStatementThen :: StatementNoShortIf,
    ifThenElseStatementElse :: Statement}
  deriving (Eq, Ord, Read, Show)

_IfThenElseStatement = (Core.Name "hydra/langs/java/syntax.IfThenElseStatement")

_IfThenElseStatement_cond = (Core.Name "cond")

_IfThenElseStatement_then = (Core.Name "then")

_IfThenElseStatement_else = (Core.Name "else")

_IfThenElseStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.IfThenElseStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "then"),
      Core.fieldTypeType = _StatementNoShortIf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "else"),
      Core.fieldTypeType = _Statement_type_}]}))

data IfThenElseStatementNoShortIf = 
  IfThenElseStatementNoShortIf {
    ifThenElseStatementNoShortIfCond :: (Maybe Expression),
    ifThenElseStatementNoShortIfThen :: StatementNoShortIf,
    ifThenElseStatementNoShortIfElse :: StatementNoShortIf}
  deriving (Eq, Ord, Read, Show)

_IfThenElseStatementNoShortIf = (Core.Name "hydra/langs/java/syntax.IfThenElseStatementNoShortIf")

_IfThenElseStatementNoShortIf_cond = (Core.Name "cond")

_IfThenElseStatementNoShortIf_then = (Core.Name "then")

_IfThenElseStatementNoShortIf_else = (Core.Name "else")

_IfThenElseStatementNoShortIf_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.IfThenElseStatementNoShortIf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "then"),
      Core.fieldTypeType = _StatementNoShortIf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "else"),
      Core.fieldTypeType = _StatementNoShortIf_type_}]}))

data AssertStatement = 
  AssertStatementSingle Expression |
  AssertStatementPair AssertStatement_Pair
  deriving (Eq, Ord, Read, Show)

_AssertStatement = (Core.Name "hydra/langs/java/syntax.AssertStatement")

_AssertStatement_single = (Core.Name "single")

_AssertStatement_pair = (Core.Name "pair")

_AssertStatement_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.AssertStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "single"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pair"),
      Core.fieldTypeType = _AssertStatement_Pair_type_}]}))

data AssertStatement_Pair = 
  AssertStatement_Pair {
    assertStatement_PairFirst :: Expression,
    assertStatement_PairSecond :: Expression}
  deriving (Eq, Ord, Read, Show)

_AssertStatement_Pair = (Core.Name "hydra/langs/java/syntax.AssertStatement.Pair")

_AssertStatement_Pair_first = (Core.Name "first")

_AssertStatement_Pair_second = (Core.Name "second")

_AssertStatement_Pair_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.AssertStatement.Pair"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "first"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "second"),
      Core.fieldTypeType = _Expression_type_}]}))

data SwitchStatement = 
  SwitchStatement {
    switchStatementCond :: Expression,
    switchStatementBlock :: SwitchBlock}
  deriving (Eq, Ord, Read, Show)

_SwitchStatement = (Core.Name "hydra/langs/java/syntax.SwitchStatement")

_SwitchStatement_cond = (Core.Name "cond")

_SwitchStatement_block = (Core.Name "block")

_SwitchStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.SwitchStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "block"),
      Core.fieldTypeType = _SwitchBlock_type_}]}))

newtype SwitchBlock = 
  SwitchBlock {
    unSwitchBlock :: [SwitchBlock_Pair]}
  deriving (Eq, Ord, Read, Show)

_SwitchBlock = (Core.Name "hydra/langs/java/syntax.SwitchBlock")

_SwitchBlock_type_ = (Core.TypeList _SwitchBlock_Pair_type_)

data SwitchBlock_Pair = 
  SwitchBlock_Pair {
    switchBlock_PairStatements :: [SwitchBlockStatementGroup],
    switchBlock_PairLabels :: [SwitchLabel]}
  deriving (Eq, Ord, Read, Show)

_SwitchBlock_Pair = (Core.Name "hydra/langs/java/syntax.SwitchBlock.Pair")

_SwitchBlock_Pair_statements = (Core.Name "statements")

_SwitchBlock_Pair_labels = (Core.Name "labels")

_SwitchBlock_Pair_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.SwitchBlock.Pair"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "statements"),
      Core.fieldTypeType = (Core.TypeList _SwitchBlockStatementGroup_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "labels"),
      Core.fieldTypeType = (Core.TypeList _SwitchLabel_type_)}]}))

data SwitchBlockStatementGroup = 
  SwitchBlockStatementGroup {
    switchBlockStatementGroupLabels :: [SwitchLabel],
    switchBlockStatementGroupStatements :: [BlockStatement]}
  deriving (Eq, Ord, Read, Show)

_SwitchBlockStatementGroup = (Core.Name "hydra/langs/java/syntax.SwitchBlockStatementGroup")

_SwitchBlockStatementGroup_labels = (Core.Name "labels")

_SwitchBlockStatementGroup_statements = (Core.Name "statements")

_SwitchBlockStatementGroup_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.SwitchBlockStatementGroup"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "labels"),
      Core.fieldTypeType = (Core.TypeList _SwitchLabel_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "statements"),
      Core.fieldTypeType = (Core.TypeList _BlockStatement_type_)}]}))

data SwitchLabel = 
  SwitchLabelConstant ConstantExpression |
  SwitchLabelEnumConstant EnumConstantName |
  SwitchLabelDefault 
  deriving (Eq, Ord, Read, Show)

_SwitchLabel = (Core.Name "hydra/langs/java/syntax.SwitchLabel")

_SwitchLabel_constant = (Core.Name "constant")

_SwitchLabel_enumConstant = (Core.Name "enumConstant")

_SwitchLabel_default = (Core.Name "default")

_SwitchLabel_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.SwitchLabel"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constant"),
      Core.fieldTypeType = _ConstantExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enumConstant"),
      Core.fieldTypeType = _EnumConstantName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "default"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype EnumConstantName = 
  EnumConstantName {
    unEnumConstantName :: Identifier}
  deriving (Eq, Ord, Read, Show)

_EnumConstantName = (Core.Name "hydra/langs/java/syntax.EnumConstantName")

_EnumConstantName_type_ = _Identifier_type_

data WhileStatement = 
  WhileStatement {
    whileStatementCond :: (Maybe Expression),
    whileStatementBody :: Statement}
  deriving (Eq, Ord, Read, Show)

_WhileStatement = (Core.Name "hydra/langs/java/syntax.WhileStatement")

_WhileStatement_cond = (Core.Name "cond")

_WhileStatement_body = (Core.Name "body")

_WhileStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.WhileStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _Statement_type_}]}))

data WhileStatementNoShortIf = 
  WhileStatementNoShortIf {
    whileStatementNoShortIfCond :: (Maybe Expression),
    whileStatementNoShortIfBody :: StatementNoShortIf}
  deriving (Eq, Ord, Read, Show)

_WhileStatementNoShortIf = (Core.Name "hydra/langs/java/syntax.WhileStatementNoShortIf")

_WhileStatementNoShortIf_cond = (Core.Name "cond")

_WhileStatementNoShortIf_body = (Core.Name "body")

_WhileStatementNoShortIf_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.WhileStatementNoShortIf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _StatementNoShortIf_type_}]}))

data DoStatement = 
  DoStatement {
    doStatementBody :: Statement,
    doStatementConde :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_DoStatement = (Core.Name "hydra/langs/java/syntax.DoStatement")

_DoStatement_body = (Core.Name "body")

_DoStatement_conde = (Core.Name "conde")

_DoStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.DoStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _Statement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "conde"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)}]}))

data ForStatement = 
  ForStatementBasic BasicForStatement |
  ForStatementEnhanced EnhancedForStatement
  deriving (Eq, Ord, Read, Show)

_ForStatement = (Core.Name "hydra/langs/java/syntax.ForStatement")

_ForStatement_basic = (Core.Name "basic")

_ForStatement_enhanced = (Core.Name "enhanced")

_ForStatement_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ForStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "basic"),
      Core.fieldTypeType = _BasicForStatement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enhanced"),
      Core.fieldTypeType = _EnhancedForStatement_type_}]}))

data ForStatementNoShortIf = 
  ForStatementNoShortIfBasic BasicForStatementNoShortIf |
  ForStatementNoShortIfEnhanced EnhancedForStatementNoShortIf
  deriving (Eq, Ord, Read, Show)

_ForStatementNoShortIf = (Core.Name "hydra/langs/java/syntax.ForStatementNoShortIf")

_ForStatementNoShortIf_basic = (Core.Name "basic")

_ForStatementNoShortIf_enhanced = (Core.Name "enhanced")

_ForStatementNoShortIf_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ForStatementNoShortIf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "basic"),
      Core.fieldTypeType = _BasicForStatementNoShortIf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enhanced"),
      Core.fieldTypeType = _EnhancedForStatementNoShortIf_type_}]}))

data BasicForStatement = 
  BasicForStatement {
    basicForStatementCond :: ForCond,
    basicForStatementBody :: Statement}
  deriving (Eq, Ord, Read, Show)

_BasicForStatement = (Core.Name "hydra/langs/java/syntax.BasicForStatement")

_BasicForStatement_cond = (Core.Name "cond")

_BasicForStatement_body = (Core.Name "body")

_BasicForStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.BasicForStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = _ForCond_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _Statement_type_}]}))

data ForCond = 
  ForCond {
    forCondInit :: (Maybe ForInit),
    forCondCond :: (Maybe Expression),
    forCondUpdate :: (Maybe ForUpdate)}
  deriving (Eq, Ord, Read, Show)

_ForCond = (Core.Name "hydra/langs/java/syntax.ForCond")

_ForCond_init = (Core.Name "init")

_ForCond_cond = (Core.Name "cond")

_ForCond_update = (Core.Name "update")

_ForCond_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ForCond"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "init"),
      Core.fieldTypeType = (Core.TypeOptional _ForInit_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "update"),
      Core.fieldTypeType = (Core.TypeOptional _ForUpdate_type_)}]}))

data BasicForStatementNoShortIf = 
  BasicForStatementNoShortIf {
    basicForStatementNoShortIfCond :: ForCond,
    basicForStatementNoShortIfBody :: StatementNoShortIf}
  deriving (Eq, Ord, Read, Show)

_BasicForStatementNoShortIf = (Core.Name "hydra/langs/java/syntax.BasicForStatementNoShortIf")

_BasicForStatementNoShortIf_cond = (Core.Name "cond")

_BasicForStatementNoShortIf_body = (Core.Name "body")

_BasicForStatementNoShortIf_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.BasicForStatementNoShortIf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = _ForCond_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _StatementNoShortIf_type_}]}))

data ForInit = 
  ForInitStatements [StatementExpression] |
  ForInitLocalVariable LocalVariableDeclaration
  deriving (Eq, Ord, Read, Show)

_ForInit = (Core.Name "hydra/langs/java/syntax.ForInit")

_ForInit_statements = (Core.Name "statements")

_ForInit_localVariable = (Core.Name "localVariable")

_ForInit_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ForInit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "statements"),
      Core.fieldTypeType = (Core.TypeList _StatementExpression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "localVariable"),
      Core.fieldTypeType = _LocalVariableDeclaration_type_}]}))

newtype ForUpdate = 
  ForUpdate {
    unForUpdate :: [StatementExpression]}
  deriving (Eq, Ord, Read, Show)

_ForUpdate = (Core.Name "hydra/langs/java/syntax.ForUpdate")

_ForUpdate_type_ = (Core.TypeList _StatementExpression_type_)

data EnhancedForStatement = 
  EnhancedForStatement {
    enhancedForStatementCond :: EnhancedForCond,
    enhancedForStatementBody :: Statement}
  deriving (Eq, Ord, Read, Show)

_EnhancedForStatement = (Core.Name "hydra/langs/java/syntax.EnhancedForStatement")

_EnhancedForStatement_cond = (Core.Name "cond")

_EnhancedForStatement_body = (Core.Name "body")

_EnhancedForStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.EnhancedForStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = _EnhancedForCond_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _Statement_type_}]}))

data EnhancedForCond = 
  EnhancedForCond {
    enhancedForCondModifiers :: [VariableModifier],
    enhancedForCondType :: LocalVariableType,
    enhancedForCondId :: VariableDeclaratorId,
    enhancedForCondExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_EnhancedForCond = (Core.Name "hydra/langs/java/syntax.EnhancedForCond")

_EnhancedForCond_modifiers = (Core.Name "modifiers")

_EnhancedForCond_type = (Core.Name "type")

_EnhancedForCond_id = (Core.Name "id")

_EnhancedForCond_expression = (Core.Name "expression")

_EnhancedForCond_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.EnhancedForCond"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _VariableModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _LocalVariableType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "id"),
      Core.fieldTypeType = _VariableDeclaratorId_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_}]}))

data EnhancedForStatementNoShortIf = 
  EnhancedForStatementNoShortIf {
    enhancedForStatementNoShortIfCond :: EnhancedForCond,
    enhancedForStatementNoShortIfBody :: StatementNoShortIf}
  deriving (Eq, Ord, Read, Show)

_EnhancedForStatementNoShortIf = (Core.Name "hydra/langs/java/syntax.EnhancedForStatementNoShortIf")

_EnhancedForStatementNoShortIf_cond = (Core.Name "cond")

_EnhancedForStatementNoShortIf_body = (Core.Name "body")

_EnhancedForStatementNoShortIf_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.EnhancedForStatementNoShortIf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = _EnhancedForCond_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _StatementNoShortIf_type_}]}))

newtype BreakStatement = 
  BreakStatement {
    unBreakStatement :: (Maybe Identifier)}
  deriving (Eq, Ord, Read, Show)

_BreakStatement = (Core.Name "hydra/langs/java/syntax.BreakStatement")

_BreakStatement_type_ = (Core.TypeOptional _Identifier_type_)

newtype ContinueStatement = 
  ContinueStatement {
    unContinueStatement :: (Maybe Identifier)}
  deriving (Eq, Ord, Read, Show)

_ContinueStatement = (Core.Name "hydra/langs/java/syntax.ContinueStatement")

_ContinueStatement_type_ = (Core.TypeOptional _Identifier_type_)

newtype ReturnStatement = 
  ReturnStatement {
    unReturnStatement :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_ReturnStatement = (Core.Name "hydra/langs/java/syntax.ReturnStatement")

_ReturnStatement_type_ = (Core.TypeOptional _Expression_type_)

newtype ThrowStatement = 
  ThrowStatement {
    unThrowStatement :: Expression}
  deriving (Eq, Ord, Read, Show)

_ThrowStatement = (Core.Name "hydra/langs/java/syntax.ThrowStatement")

_ThrowStatement_type_ = _Expression_type_

data SynchronizedStatement = 
  SynchronizedStatement {
    synchronizedStatementExpression :: Expression,
    synchronizedStatementBlock :: Block}
  deriving (Eq, Ord, Read, Show)

_SynchronizedStatement = (Core.Name "hydra/langs/java/syntax.SynchronizedStatement")

_SynchronizedStatement_expression = (Core.Name "expression")

_SynchronizedStatement_block = (Core.Name "block")

_SynchronizedStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.SynchronizedStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "block"),
      Core.fieldTypeType = _Block_type_}]}))

data TryStatement = 
  TryStatementSimple TryStatement_Simple |
  TryStatementWithFinally TryStatement_WithFinally |
  TryStatementWithResources TryWithResourcesStatement
  deriving (Eq, Ord, Read, Show)

_TryStatement = (Core.Name "hydra/langs/java/syntax.TryStatement")

_TryStatement_simple = (Core.Name "simple")

_TryStatement_withFinally = (Core.Name "withFinally")

_TryStatement_withResources = (Core.Name "withResources")

_TryStatement_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TryStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simple"),
      Core.fieldTypeType = _TryStatement_Simple_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withFinally"),
      Core.fieldTypeType = _TryStatement_WithFinally_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withResources"),
      Core.fieldTypeType = _TryWithResourcesStatement_type_}]}))

data TryStatement_Simple = 
  TryStatement_Simple {
    tryStatement_SimpleBlock :: Block,
    tryStatement_SimpleCatches :: Catches}
  deriving (Eq, Ord, Read, Show)

_TryStatement_Simple = (Core.Name "hydra/langs/java/syntax.TryStatement.Simple")

_TryStatement_Simple_block = (Core.Name "block")

_TryStatement_Simple_catches = (Core.Name "catches")

_TryStatement_Simple_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TryStatement.Simple"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "block"),
      Core.fieldTypeType = _Block_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "catches"),
      Core.fieldTypeType = _Catches_type_}]}))

data TryStatement_WithFinally = 
  TryStatement_WithFinally {
    tryStatement_WithFinallyBlock :: Block,
    tryStatement_WithFinallyCatches :: (Maybe Catches),
    tryStatement_WithFinallyFinally :: Finally}
  deriving (Eq, Ord, Read, Show)

_TryStatement_WithFinally = (Core.Name "hydra/langs/java/syntax.TryStatement.WithFinally")

_TryStatement_WithFinally_block = (Core.Name "block")

_TryStatement_WithFinally_catches = (Core.Name "catches")

_TryStatement_WithFinally_finally = (Core.Name "finally")

_TryStatement_WithFinally_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TryStatement.WithFinally"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "block"),
      Core.fieldTypeType = _Block_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "catches"),
      Core.fieldTypeType = (Core.TypeOptional _Catches_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "finally"),
      Core.fieldTypeType = _Finally_type_}]}))

newtype Catches = 
  Catches {
    unCatches :: [CatchClause]}
  deriving (Eq, Ord, Read, Show)

_Catches = (Core.Name "hydra/langs/java/syntax.Catches")

_Catches_type_ = (Core.TypeList _CatchClause_type_)

data CatchClause = 
  CatchClause {
    catchClauseParameter :: (Maybe CatchFormalParameter),
    catchClauseBlock :: Block}
  deriving (Eq, Ord, Read, Show)

_CatchClause = (Core.Name "hydra/langs/java/syntax.CatchClause")

_CatchClause_parameter = (Core.Name "parameter")

_CatchClause_block = (Core.Name "block")

_CatchClause_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.CatchClause"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parameter"),
      Core.fieldTypeType = (Core.TypeOptional _CatchFormalParameter_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "block"),
      Core.fieldTypeType = _Block_type_}]}))

data CatchFormalParameter = 
  CatchFormalParameter {
    catchFormalParameterModifiers :: [VariableModifier],
    catchFormalParameterType :: CatchType,
    catchFormalParameterId :: VariableDeclaratorId}
  deriving (Eq, Ord, Read, Show)

_CatchFormalParameter = (Core.Name "hydra/langs/java/syntax.CatchFormalParameter")

_CatchFormalParameter_modifiers = (Core.Name "modifiers")

_CatchFormalParameter_type = (Core.Name "type")

_CatchFormalParameter_id = (Core.Name "id")

_CatchFormalParameter_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.CatchFormalParameter"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _VariableModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _CatchType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "id"),
      Core.fieldTypeType = _VariableDeclaratorId_type_}]}))

data CatchType = 
  CatchType {
    catchTypeType :: UnannClassType,
    catchTypeTypes :: [ClassType]}
  deriving (Eq, Ord, Read, Show)

_CatchType = (Core.Name "hydra/langs/java/syntax.CatchType")

_CatchType_type = (Core.Name "type")

_CatchType_types = (Core.Name "types")

_CatchType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.CatchType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _UnannClassType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "types"),
      Core.fieldTypeType = (Core.TypeList _ClassType_type_)}]}))

newtype Finally = 
  Finally {
    unFinally :: Block}
  deriving (Eq, Ord, Read, Show)

_Finally = (Core.Name "hydra/langs/java/syntax.Finally")

_Finally_type_ = _Block_type_

data TryWithResourcesStatement = 
  TryWithResourcesStatement {
    tryWithResourcesStatementResourceSpecification :: ResourceSpecification,
    tryWithResourcesStatementBlock :: Block,
    tryWithResourcesStatementCatches :: (Maybe Catches),
    tryWithResourcesStatementFinally :: (Maybe Finally)}
  deriving (Eq, Ord, Read, Show)

_TryWithResourcesStatement = (Core.Name "hydra/langs/java/syntax.TryWithResourcesStatement")

_TryWithResourcesStatement_resourceSpecification = (Core.Name "resourceSpecification")

_TryWithResourcesStatement_block = (Core.Name "block")

_TryWithResourcesStatement_catches = (Core.Name "catches")

_TryWithResourcesStatement_finally = (Core.Name "finally")

_TryWithResourcesStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TryWithResourcesStatement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "resourceSpecification"),
      Core.fieldTypeType = _ResourceSpecification_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "block"),
      Core.fieldTypeType = _Block_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "catches"),
      Core.fieldTypeType = (Core.TypeOptional _Catches_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "finally"),
      Core.fieldTypeType = (Core.TypeOptional _Finally_type_)}]}))

newtype ResourceSpecification = 
  ResourceSpecification {
    unResourceSpecification :: [Resource]}
  deriving (Eq, Ord, Read, Show)

_ResourceSpecification = (Core.Name "hydra/langs/java/syntax.ResourceSpecification")

_ResourceSpecification_type_ = (Core.TypeList _Resource_type_)

data Resource = 
  ResourceLocal Resource_Local |
  ResourceVariable VariableAccess
  deriving (Eq, Ord, Read, Show)

_Resource = (Core.Name "hydra/langs/java/syntax.Resource")

_Resource_local = (Core.Name "local")

_Resource_variable = (Core.Name "variable")

_Resource_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.Resource"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "local"),
      Core.fieldTypeType = _Resource_Local_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _VariableAccess_type_}]}))

data Resource_Local = 
  Resource_Local {
    resource_LocalModifiers :: [VariableModifier],
    resource_LocalType :: LocalVariableType,
    resource_LocalIdentifier :: Identifier,
    resource_LocalExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_Resource_Local = (Core.Name "hydra/langs/java/syntax.Resource.Local")

_Resource_Local_modifiers = (Core.Name "modifiers")

_Resource_Local_type = (Core.Name "type")

_Resource_Local_identifier = (Core.Name "identifier")

_Resource_Local_expression = (Core.Name "expression")

_Resource_Local_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.Resource.Local"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _VariableModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _LocalVariableType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_}]}))

data VariableAccess = 
  VariableAccessExpressionName ExpressionName |
  VariableAccessFieldAccess FieldAccess
  deriving (Eq, Ord, Read, Show)

_VariableAccess = (Core.Name "hydra/langs/java/syntax.VariableAccess")

_VariableAccess_expressionName = (Core.Name "expressionName")

_VariableAccess_fieldAccess = (Core.Name "fieldAccess")

_VariableAccess_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.VariableAccess"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expressionName"),
      Core.fieldTypeType = _ExpressionName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fieldAccess"),
      Core.fieldTypeType = _FieldAccess_type_}]}))

data Primary = 
  PrimaryNoNewArray PrimaryNoNewArray |
  PrimaryArrayCreation ArrayCreationExpression
  deriving (Eq, Ord, Read, Show)

_Primary = (Core.Name "hydra/langs/java/syntax.Primary")

_Primary_noNewArray = (Core.Name "noNewArray")

_Primary_arrayCreation = (Core.Name "arrayCreation")

_Primary_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.Primary"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "noNewArray"),
      Core.fieldTypeType = _PrimaryNoNewArray_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arrayCreation"),
      Core.fieldTypeType = _ArrayCreationExpression_type_}]}))

data PrimaryNoNewArray = 
  PrimaryNoNewArrayLiteral Literal |
  PrimaryNoNewArrayClassLiteral ClassLiteral |
  PrimaryNoNewArrayThis  |
  PrimaryNoNewArrayDotThis TypeName |
  PrimaryNoNewArrayParens Expression |
  PrimaryNoNewArrayClassInstance ClassInstanceCreationExpression |
  PrimaryNoNewArrayFieldAccess FieldAccess |
  PrimaryNoNewArrayArrayAccess ArrayAccess |
  PrimaryNoNewArrayMethodInvocation MethodInvocation |
  PrimaryNoNewArrayMethodReference MethodReference
  deriving (Eq, Ord, Read, Show)

_PrimaryNoNewArray = (Core.Name "hydra/langs/java/syntax.PrimaryNoNewArray")

_PrimaryNoNewArray_literal = (Core.Name "literal")

_PrimaryNoNewArray_classLiteral = (Core.Name "classLiteral")

_PrimaryNoNewArray_this = (Core.Name "this")

_PrimaryNoNewArray_dotThis = (Core.Name "dotThis")

_PrimaryNoNewArray_parens = (Core.Name "parens")

_PrimaryNoNewArray_classInstance = (Core.Name "classInstance")

_PrimaryNoNewArray_fieldAccess = (Core.Name "fieldAccess")

_PrimaryNoNewArray_arrayAccess = (Core.Name "arrayAccess")

_PrimaryNoNewArray_methodInvocation = (Core.Name "methodInvocation")

_PrimaryNoNewArray_methodReference = (Core.Name "methodReference")

_PrimaryNoNewArray_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.PrimaryNoNewArray"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _Literal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classLiteral"),
      Core.fieldTypeType = _ClassLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "this"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dotThis"),
      Core.fieldTypeType = _TypeName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parens"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classInstance"),
      Core.fieldTypeType = _ClassInstanceCreationExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fieldAccess"),
      Core.fieldTypeType = _FieldAccess_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arrayAccess"),
      Core.fieldTypeType = _ArrayAccess_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "methodInvocation"),
      Core.fieldTypeType = _MethodInvocation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "methodReference"),
      Core.fieldTypeType = _MethodReference_type_}]}))

data ClassLiteral = 
  ClassLiteralType TypeNameArray |
  ClassLiteralNumericType NumericTypeArray |
  ClassLiteralBoolean BooleanArray |
  ClassLiteralVoid 
  deriving (Eq, Ord, Read, Show)

_ClassLiteral = (Core.Name "hydra/langs/java/syntax.ClassLiteral")

_ClassLiteral_type = (Core.Name "type")

_ClassLiteral_numericType = (Core.Name "numericType")

_ClassLiteral_boolean = (Core.Name "boolean")

_ClassLiteral_void = (Core.Name "void")

_ClassLiteral_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassLiteral"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _TypeNameArray_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "numericType"),
      Core.fieldTypeType = _NumericTypeArray_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = _BooleanArray_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "void"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TypeNameArray = 
  TypeNameArraySimple TypeName |
  TypeNameArrayArray TypeNameArray
  deriving (Eq, Ord, Read, Show)

_TypeNameArray = (Core.Name "hydra/langs/java/syntax.TypeNameArray")

_TypeNameArray_simple = (Core.Name "simple")

_TypeNameArray_array = (Core.Name "array")

_TypeNameArray_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TypeNameArray"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simple"),
      Core.fieldTypeType = _TypeName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _TypeNameArray_type_}]}))

data NumericTypeArray = 
  NumericTypeArraySimple NumericType |
  NumericTypeArrayArray NumericTypeArray
  deriving (Eq, Ord, Read, Show)

_NumericTypeArray = (Core.Name "hydra/langs/java/syntax.NumericTypeArray")

_NumericTypeArray_simple = (Core.Name "simple")

_NumericTypeArray_array = (Core.Name "array")

_NumericTypeArray_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.NumericTypeArray"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simple"),
      Core.fieldTypeType = _NumericType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _NumericTypeArray_type_}]}))

data BooleanArray = 
  BooleanArraySimple  |
  BooleanArrayArray BooleanArray
  deriving (Eq, Ord, Read, Show)

_BooleanArray = (Core.Name "hydra/langs/java/syntax.BooleanArray")

_BooleanArray_simple = (Core.Name "simple")

_BooleanArray_array = (Core.Name "array")

_BooleanArray_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.BooleanArray"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simple"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _BooleanArray_type_}]}))

data ClassInstanceCreationExpression = 
  ClassInstanceCreationExpression {
    classInstanceCreationExpressionQualifier :: (Maybe ClassInstanceCreationExpression_Qualifier),
    classInstanceCreationExpressionExpression :: UnqualifiedClassInstanceCreationExpression}
  deriving (Eq, Ord, Read, Show)

_ClassInstanceCreationExpression = (Core.Name "hydra/langs/java/syntax.ClassInstanceCreationExpression")

_ClassInstanceCreationExpression_qualifier = (Core.Name "qualifier")

_ClassInstanceCreationExpression_expression = (Core.Name "expression")

_ClassInstanceCreationExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassInstanceCreationExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifier"),
      Core.fieldTypeType = (Core.TypeOptional _ClassInstanceCreationExpression_Qualifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _UnqualifiedClassInstanceCreationExpression_type_}]}))

data ClassInstanceCreationExpression_Qualifier = 
  ClassInstanceCreationExpression_QualifierExpression ExpressionName |
  ClassInstanceCreationExpression_QualifierPrimary Primary
  deriving (Eq, Ord, Read, Show)

_ClassInstanceCreationExpression_Qualifier = (Core.Name "hydra/langs/java/syntax.ClassInstanceCreationExpression.Qualifier")

_ClassInstanceCreationExpression_Qualifier_expression = (Core.Name "expression")

_ClassInstanceCreationExpression_Qualifier_primary = (Core.Name "primary")

_ClassInstanceCreationExpression_Qualifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassInstanceCreationExpression.Qualifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _ExpressionName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primary"),
      Core.fieldTypeType = _Primary_type_}]}))

data UnqualifiedClassInstanceCreationExpression = 
  UnqualifiedClassInstanceCreationExpression {
    unqualifiedClassInstanceCreationExpressionTypeArguments :: [TypeArgument],
    unqualifiedClassInstanceCreationExpressionClassOrInterface :: ClassOrInterfaceTypeToInstantiate,
    unqualifiedClassInstanceCreationExpressionArguments :: [Expression],
    unqualifiedClassInstanceCreationExpressionBody :: (Maybe ClassBody)}
  deriving (Eq, Ord, Read, Show)

_UnqualifiedClassInstanceCreationExpression = (Core.Name "hydra/langs/java/syntax.UnqualifiedClassInstanceCreationExpression")

_UnqualifiedClassInstanceCreationExpression_typeArguments = (Core.Name "typeArguments")

_UnqualifiedClassInstanceCreationExpression_classOrInterface = (Core.Name "classOrInterface")

_UnqualifiedClassInstanceCreationExpression_arguments = (Core.Name "arguments")

_UnqualifiedClassInstanceCreationExpression_body = (Core.Name "body")

_UnqualifiedClassInstanceCreationExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.UnqualifiedClassInstanceCreationExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeArguments"),
      Core.fieldTypeType = (Core.TypeList _TypeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classOrInterface"),
      Core.fieldTypeType = _ClassOrInterfaceTypeToInstantiate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = (Core.TypeOptional _ClassBody_type_)}]}))

data ClassOrInterfaceTypeToInstantiate = 
  ClassOrInterfaceTypeToInstantiate {
    classOrInterfaceTypeToInstantiateIdentifiers :: [AnnotatedIdentifier],
    classOrInterfaceTypeToInstantiateTypeArguments :: (Maybe TypeArgumentsOrDiamond)}
  deriving (Eq, Ord, Read, Show)

_ClassOrInterfaceTypeToInstantiate = (Core.Name "hydra/langs/java/syntax.ClassOrInterfaceTypeToInstantiate")

_ClassOrInterfaceTypeToInstantiate_identifiers = (Core.Name "identifiers")

_ClassOrInterfaceTypeToInstantiate_typeArguments = (Core.Name "typeArguments")

_ClassOrInterfaceTypeToInstantiate_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ClassOrInterfaceTypeToInstantiate"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifiers"),
      Core.fieldTypeType = (Core.TypeList _AnnotatedIdentifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeArguments"),
      Core.fieldTypeType = (Core.TypeOptional _TypeArgumentsOrDiamond_type_)}]}))

data AnnotatedIdentifier = 
  AnnotatedIdentifier {
    annotatedIdentifierAnnotations :: [Annotation],
    annotatedIdentifierIdentifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_AnnotatedIdentifier = (Core.Name "hydra/langs/java/syntax.AnnotatedIdentifier")

_AnnotatedIdentifier_annotations = (Core.Name "annotations")

_AnnotatedIdentifier_identifier = (Core.Name "identifier")

_AnnotatedIdentifier_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.AnnotatedIdentifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_}]}))

data TypeArgumentsOrDiamond = 
  TypeArgumentsOrDiamondArguments [TypeArgument] |
  TypeArgumentsOrDiamondDiamond 
  deriving (Eq, Ord, Read, Show)

_TypeArgumentsOrDiamond = (Core.Name "hydra/langs/java/syntax.TypeArgumentsOrDiamond")

_TypeArgumentsOrDiamond_arguments = (Core.Name "arguments")

_TypeArgumentsOrDiamond_diamond = (Core.Name "diamond")

_TypeArgumentsOrDiamond_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.TypeArgumentsOrDiamond"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = (Core.TypeList _TypeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "diamond"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data FieldAccess = 
  FieldAccess {
    fieldAccessQualifier :: FieldAccess_Qualifier,
    fieldAccessIdentifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_FieldAccess = (Core.Name "hydra/langs/java/syntax.FieldAccess")

_FieldAccess_qualifier = (Core.Name "qualifier")

_FieldAccess_identifier = (Core.Name "identifier")

_FieldAccess_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.FieldAccess"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifier"),
      Core.fieldTypeType = _FieldAccess_Qualifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_}]}))

data FieldAccess_Qualifier = 
  FieldAccess_QualifierPrimary Primary |
  FieldAccess_QualifierSuper  |
  FieldAccess_QualifierTyped TypeName
  deriving (Eq, Ord, Read, Show)

_FieldAccess_Qualifier = (Core.Name "hydra/langs/java/syntax.FieldAccess.Qualifier")

_FieldAccess_Qualifier_primary = (Core.Name "primary")

_FieldAccess_Qualifier_super = (Core.Name "super")

_FieldAccess_Qualifier_typed = (Core.Name "typed")

_FieldAccess_Qualifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.FieldAccess.Qualifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primary"),
      Core.fieldTypeType = _Primary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "super"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typed"),
      Core.fieldTypeType = _TypeName_type_}]}))

data ArrayAccess = 
  ArrayAccess {
    arrayAccessExpression :: (Maybe Expression),
    arrayAccessVariant :: ArrayAccess_Variant}
  deriving (Eq, Ord, Read, Show)

_ArrayAccess = (Core.Name "hydra/langs/java/syntax.ArrayAccess")

_ArrayAccess_expression = (Core.Name "expression")

_ArrayAccess_variant = (Core.Name "variant")

_ArrayAccess_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ArrayAccess"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variant"),
      Core.fieldTypeType = _ArrayAccess_Variant_type_}]}))

data ArrayAccess_Variant = 
  ArrayAccess_VariantName ExpressionName |
  ArrayAccess_VariantPrimary PrimaryNoNewArray
  deriving (Eq, Ord, Read, Show)

_ArrayAccess_Variant = (Core.Name "hydra/langs/java/syntax.ArrayAccess.Variant")

_ArrayAccess_Variant_name = (Core.Name "name")

_ArrayAccess_Variant_primary = (Core.Name "primary")

_ArrayAccess_Variant_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ArrayAccess.Variant"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _ExpressionName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primary"),
      Core.fieldTypeType = _PrimaryNoNewArray_type_}]}))

data MethodInvocation = 
  MethodInvocation {
    methodInvocationHeader :: MethodInvocation_Header,
    methodInvocationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_MethodInvocation = (Core.Name "hydra/langs/java/syntax.MethodInvocation")

_MethodInvocation_header = (Core.Name "header")

_MethodInvocation_arguments = (Core.Name "arguments")

_MethodInvocation_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodInvocation"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "header"),
      Core.fieldTypeType = _MethodInvocation_Header_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)}]}))

data MethodInvocation_Header = 
  MethodInvocation_HeaderSimple MethodName |
  MethodInvocation_HeaderComplex MethodInvocation_Complex
  deriving (Eq, Ord, Read, Show)

_MethodInvocation_Header = (Core.Name "hydra/langs/java/syntax.MethodInvocation.Header")

_MethodInvocation_Header_simple = (Core.Name "simple")

_MethodInvocation_Header_complex = (Core.Name "complex")

_MethodInvocation_Header_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodInvocation.Header"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simple"),
      Core.fieldTypeType = _MethodName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "complex"),
      Core.fieldTypeType = _MethodInvocation_Complex_type_}]}))

data MethodInvocation_Complex = 
  MethodInvocation_Complex {
    methodInvocation_ComplexVariant :: MethodInvocation_Variant,
    methodInvocation_ComplexTypeArguments :: [TypeArgument],
    methodInvocation_ComplexIdentifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_MethodInvocation_Complex = (Core.Name "hydra/langs/java/syntax.MethodInvocation.Complex")

_MethodInvocation_Complex_variant = (Core.Name "variant")

_MethodInvocation_Complex_typeArguments = (Core.Name "typeArguments")

_MethodInvocation_Complex_identifier = (Core.Name "identifier")

_MethodInvocation_Complex_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodInvocation.Complex"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variant"),
      Core.fieldTypeType = _MethodInvocation_Variant_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeArguments"),
      Core.fieldTypeType = (Core.TypeList _TypeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_}]}))

data MethodInvocation_Variant = 
  MethodInvocation_VariantType TypeName |
  MethodInvocation_VariantExpression ExpressionName |
  MethodInvocation_VariantPrimary Primary |
  MethodInvocation_VariantSuper  |
  MethodInvocation_VariantTypeSuper TypeName
  deriving (Eq, Ord, Read, Show)

_MethodInvocation_Variant = (Core.Name "hydra/langs/java/syntax.MethodInvocation.Variant")

_MethodInvocation_Variant_type = (Core.Name "type")

_MethodInvocation_Variant_expression = (Core.Name "expression")

_MethodInvocation_Variant_primary = (Core.Name "primary")

_MethodInvocation_Variant_super = (Core.Name "super")

_MethodInvocation_Variant_typeSuper = (Core.Name "typeSuper")

_MethodInvocation_Variant_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodInvocation.Variant"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _TypeName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _ExpressionName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primary"),
      Core.fieldTypeType = _Primary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "super"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeSuper"),
      Core.fieldTypeType = _TypeName_type_}]}))

data MethodReference = 
  MethodReferenceExpression MethodReference_Expression |
  MethodReferencePrimary MethodReference_Primary |
  MethodReferenceReferenceType MethodReference_ReferenceType |
  MethodReferenceSuper MethodReference_Super |
  MethodReferenceNew MethodReference_New |
  MethodReferenceArray MethodReference_Array
  deriving (Eq, Ord, Read, Show)

_MethodReference = (Core.Name "hydra/langs/java/syntax.MethodReference")

_MethodReference_expression = (Core.Name "expression")

_MethodReference_primary = (Core.Name "primary")

_MethodReference_referenceType = (Core.Name "referenceType")

_MethodReference_super = (Core.Name "super")

_MethodReference_new = (Core.Name "new")

_MethodReference_array = (Core.Name "array")

_MethodReference_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodReference"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _MethodReference_Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primary"),
      Core.fieldTypeType = _MethodReference_Primary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "referenceType"),
      Core.fieldTypeType = _MethodReference_ReferenceType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "super"),
      Core.fieldTypeType = _MethodReference_Super_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "new"),
      Core.fieldTypeType = _MethodReference_New_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _MethodReference_Array_type_}]}))

data MethodReference_Expression = 
  MethodReference_Expression {
    methodReference_ExpressionName :: ExpressionName,
    methodReference_ExpressionTypeArguments :: [TypeArgument],
    methodReference_ExpressionIdentifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_MethodReference_Expression = (Core.Name "hydra/langs/java/syntax.MethodReference.Expression")

_MethodReference_Expression_name = (Core.Name "name")

_MethodReference_Expression_typeArguments = (Core.Name "typeArguments")

_MethodReference_Expression_identifier = (Core.Name "identifier")

_MethodReference_Expression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodReference.Expression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _ExpressionName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeArguments"),
      Core.fieldTypeType = (Core.TypeList _TypeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_}]}))

data MethodReference_Primary = 
  MethodReference_Primary {
    methodReference_PrimaryPrimary :: Primary,
    methodReference_PrimaryTypeArguments :: [TypeArgument],
    methodReference_PrimaryIdentifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_MethodReference_Primary = (Core.Name "hydra/langs/java/syntax.MethodReference.Primary")

_MethodReference_Primary_primary = (Core.Name "primary")

_MethodReference_Primary_typeArguments = (Core.Name "typeArguments")

_MethodReference_Primary_identifier = (Core.Name "identifier")

_MethodReference_Primary_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodReference.Primary"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primary"),
      Core.fieldTypeType = _Primary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeArguments"),
      Core.fieldTypeType = (Core.TypeList _TypeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_}]}))

data MethodReference_ReferenceType = 
  MethodReference_ReferenceType {
    methodReference_ReferenceTypeReferenceType :: ReferenceType,
    methodReference_ReferenceTypeTypeArguments :: [TypeArgument],
    methodReference_ReferenceTypeIdentifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_MethodReference_ReferenceType = (Core.Name "hydra/langs/java/syntax.MethodReference.ReferenceType")

_MethodReference_ReferenceType_referenceType = (Core.Name "referenceType")

_MethodReference_ReferenceType_typeArguments = (Core.Name "typeArguments")

_MethodReference_ReferenceType_identifier = (Core.Name "identifier")

_MethodReference_ReferenceType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodReference.ReferenceType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "referenceType"),
      Core.fieldTypeType = _ReferenceType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeArguments"),
      Core.fieldTypeType = (Core.TypeList _TypeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_}]}))

data MethodReference_Super = 
  MethodReference_Super {
    methodReference_SuperTypeArguments :: [TypeArgument],
    methodReference_SuperIdentifier :: Identifier,
    methodReference_SuperSuper :: Bool}
  deriving (Eq, Ord, Read, Show)

_MethodReference_Super = (Core.Name "hydra/langs/java/syntax.MethodReference.Super")

_MethodReference_Super_typeArguments = (Core.Name "typeArguments")

_MethodReference_Super_identifier = (Core.Name "identifier")

_MethodReference_Super_super = (Core.Name "super")

_MethodReference_Super_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodReference.Super"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeArguments"),
      Core.fieldTypeType = (Core.TypeList _TypeArgument_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identifier"),
      Core.fieldTypeType = _Identifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "super"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}]}))

data MethodReference_New = 
  MethodReference_New {
    methodReference_NewClassType :: ClassType,
    methodReference_NewTypeArguments :: [TypeArgument]}
  deriving (Eq, Ord, Read, Show)

_MethodReference_New = (Core.Name "hydra/langs/java/syntax.MethodReference.New")

_MethodReference_New_classType = (Core.Name "classType")

_MethodReference_New_typeArguments = (Core.Name "typeArguments")

_MethodReference_New_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MethodReference.New"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classType"),
      Core.fieldTypeType = _ClassType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeArguments"),
      Core.fieldTypeType = (Core.TypeList _TypeArgument_type_)}]}))

newtype MethodReference_Array = 
  MethodReference_Array {
    unMethodReference_Array :: ArrayType}
  deriving (Eq, Ord, Read, Show)

_MethodReference_Array = (Core.Name "hydra/langs/java/syntax.MethodReference.Array")

_MethodReference_Array_type_ = _ArrayType_type_

data ArrayCreationExpression = 
  ArrayCreationExpressionPrimitive ArrayCreationExpression_Primitive |
  ArrayCreationExpressionClassOrInterface ArrayCreationExpression_ClassOrInterface |
  ArrayCreationExpressionPrimitiveArray ArrayCreationExpression_PrimitiveArray |
  ArrayCreationExpressionClassOrInterfaceArray ArrayCreationExpression_ClassOrInterfaceArray
  deriving (Eq, Ord, Read, Show)

_ArrayCreationExpression = (Core.Name "hydra/langs/java/syntax.ArrayCreationExpression")

_ArrayCreationExpression_primitive = (Core.Name "primitive")

_ArrayCreationExpression_classOrInterface = (Core.Name "classOrInterface")

_ArrayCreationExpression_primitiveArray = (Core.Name "primitiveArray")

_ArrayCreationExpression_classOrInterfaceArray = (Core.Name "classOrInterfaceArray")

_ArrayCreationExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ArrayCreationExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primitive"),
      Core.fieldTypeType = _ArrayCreationExpression_Primitive_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classOrInterface"),
      Core.fieldTypeType = _ArrayCreationExpression_ClassOrInterface_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primitiveArray"),
      Core.fieldTypeType = _ArrayCreationExpression_PrimitiveArray_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classOrInterfaceArray"),
      Core.fieldTypeType = _ArrayCreationExpression_ClassOrInterfaceArray_type_}]}))

data ArrayCreationExpression_Primitive = 
  ArrayCreationExpression_Primitive {
    arrayCreationExpression_PrimitiveType :: PrimitiveTypeWithAnnotations,
    arrayCreationExpression_PrimitiveDimExprs :: [DimExpr],
    arrayCreationExpression_PrimitiveDims :: (Maybe Dims)}
  deriving (Eq, Ord, Read, Show)

_ArrayCreationExpression_Primitive = (Core.Name "hydra/langs/java/syntax.ArrayCreationExpression.Primitive")

_ArrayCreationExpression_Primitive_type = (Core.Name "type")

_ArrayCreationExpression_Primitive_dimExprs = (Core.Name "dimExprs")

_ArrayCreationExpression_Primitive_dims = (Core.Name "dims")

_ArrayCreationExpression_Primitive_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ArrayCreationExpression.Primitive"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _PrimitiveTypeWithAnnotations_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dimExprs"),
      Core.fieldTypeType = (Core.TypeList _DimExpr_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dims"),
      Core.fieldTypeType = (Core.TypeOptional _Dims_type_)}]}))

data ArrayCreationExpression_ClassOrInterface = 
  ArrayCreationExpression_ClassOrInterface {
    arrayCreationExpression_ClassOrInterfaceType :: ClassOrInterfaceType,
    arrayCreationExpression_ClassOrInterfaceDimExprs :: [DimExpr],
    arrayCreationExpression_ClassOrInterfaceDims :: (Maybe Dims)}
  deriving (Eq, Ord, Read, Show)

_ArrayCreationExpression_ClassOrInterface = (Core.Name "hydra/langs/java/syntax.ArrayCreationExpression.ClassOrInterface")

_ArrayCreationExpression_ClassOrInterface_type = (Core.Name "type")

_ArrayCreationExpression_ClassOrInterface_dimExprs = (Core.Name "dimExprs")

_ArrayCreationExpression_ClassOrInterface_dims = (Core.Name "dims")

_ArrayCreationExpression_ClassOrInterface_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ArrayCreationExpression.ClassOrInterface"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _ClassOrInterfaceType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dimExprs"),
      Core.fieldTypeType = (Core.TypeList _DimExpr_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dims"),
      Core.fieldTypeType = (Core.TypeOptional _Dims_type_)}]}))

data ArrayCreationExpression_PrimitiveArray = 
  ArrayCreationExpression_PrimitiveArray {
    arrayCreationExpression_PrimitiveArrayType :: PrimitiveTypeWithAnnotations,
    arrayCreationExpression_PrimitiveArrayDims :: [Dims],
    arrayCreationExpression_PrimitiveArrayArray :: ArrayInitializer}
  deriving (Eq, Ord, Read, Show)

_ArrayCreationExpression_PrimitiveArray = (Core.Name "hydra/langs/java/syntax.ArrayCreationExpression.PrimitiveArray")

_ArrayCreationExpression_PrimitiveArray_type = (Core.Name "type")

_ArrayCreationExpression_PrimitiveArray_dims = (Core.Name "dims")

_ArrayCreationExpression_PrimitiveArray_array = (Core.Name "array")

_ArrayCreationExpression_PrimitiveArray_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ArrayCreationExpression.PrimitiveArray"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _PrimitiveTypeWithAnnotations_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dims"),
      Core.fieldTypeType = (Core.TypeList _Dims_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _ArrayInitializer_type_}]}))

data ArrayCreationExpression_ClassOrInterfaceArray = 
  ArrayCreationExpression_ClassOrInterfaceArray {
    arrayCreationExpression_ClassOrInterfaceArrayType :: ClassOrInterfaceType,
    arrayCreationExpression_ClassOrInterfaceArrayDims :: [Dims],
    arrayCreationExpression_ClassOrInterfaceArrayArray :: ArrayInitializer}
  deriving (Eq, Ord, Read, Show)

_ArrayCreationExpression_ClassOrInterfaceArray = (Core.Name "hydra/langs/java/syntax.ArrayCreationExpression.ClassOrInterfaceArray")

_ArrayCreationExpression_ClassOrInterfaceArray_type = (Core.Name "type")

_ArrayCreationExpression_ClassOrInterfaceArray_dims = (Core.Name "dims")

_ArrayCreationExpression_ClassOrInterfaceArray_array = (Core.Name "array")

_ArrayCreationExpression_ClassOrInterfaceArray_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ArrayCreationExpression.ClassOrInterfaceArray"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _ClassOrInterfaceType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dims"),
      Core.fieldTypeType = (Core.TypeList _Dims_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _ArrayInitializer_type_}]}))

data DimExpr = 
  DimExpr {
    dimExprAnnotations :: [Annotation],
    dimExprExpression :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_DimExpr = (Core.Name "hydra/langs/java/syntax.DimExpr")

_DimExpr_annotations = (Core.Name "annotations")

_DimExpr_expression = (Core.Name "expression")

_DimExpr_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.DimExpr"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)}]}))

data Expression = 
  ExpressionLambda LambdaExpression |
  ExpressionAssignment AssignmentExpression
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra/langs/java/syntax.Expression")

_Expression_lambda = (Core.Name "lambda")

_Expression_assignment = (Core.Name "assignment")

_Expression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.Expression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lambda"),
      Core.fieldTypeType = _LambdaExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "assignment"),
      Core.fieldTypeType = _AssignmentExpression_type_}]}))

data LambdaExpression = 
  LambdaExpression {
    lambdaExpressionParameters :: LambdaParameters,
    lambdaExpressionBody :: LambdaBody}
  deriving (Eq, Ord, Read, Show)

_LambdaExpression = (Core.Name "hydra/langs/java/syntax.LambdaExpression")

_LambdaExpression_parameters = (Core.Name "parameters")

_LambdaExpression_body = (Core.Name "body")

_LambdaExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.LambdaExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parameters"),
      Core.fieldTypeType = _LambdaParameters_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _LambdaBody_type_}]}))

data LambdaParameters = 
  LambdaParametersTuple [LambdaParameters] |
  LambdaParametersSingle Identifier
  deriving (Eq, Ord, Read, Show)

_LambdaParameters = (Core.Name "hydra/langs/java/syntax.LambdaParameters")

_LambdaParameters_tuple = (Core.Name "tuple")

_LambdaParameters_single = (Core.Name "single")

_LambdaParameters_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.LambdaParameters"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tuple"),
      Core.fieldTypeType = (Core.TypeList _LambdaParameters_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "single"),
      Core.fieldTypeType = _Identifier_type_}]}))

data LambdaParameter = 
  LambdaParameterNormal LambdaParameter_Normal |
  LambdaParameterVariableArity VariableArityParameter
  deriving (Eq, Ord, Read, Show)

_LambdaParameter = (Core.Name "hydra/langs/java/syntax.LambdaParameter")

_LambdaParameter_normal = (Core.Name "normal")

_LambdaParameter_variableArity = (Core.Name "variableArity")

_LambdaParameter_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.LambdaParameter"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "normal"),
      Core.fieldTypeType = _LambdaParameter_Normal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variableArity"),
      Core.fieldTypeType = _VariableArityParameter_type_}]}))

data LambdaParameter_Normal = 
  LambdaParameter_Normal {
    lambdaParameter_NormalModifiers :: [VariableModifier],
    lambdaParameter_NormalType :: LambdaParameterType,
    lambdaParameter_NormalId :: VariableDeclaratorId}
  deriving (Eq, Ord, Read, Show)

_LambdaParameter_Normal = (Core.Name "hydra/langs/java/syntax.LambdaParameter.Normal")

_LambdaParameter_Normal_modifiers = (Core.Name "modifiers")

_LambdaParameter_Normal_type = (Core.Name "type")

_LambdaParameter_Normal_id = (Core.Name "id")

_LambdaParameter_Normal_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.LambdaParameter.Normal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifiers"),
      Core.fieldTypeType = (Core.TypeList _VariableModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _LambdaParameterType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "id"),
      Core.fieldTypeType = _VariableDeclaratorId_type_}]}))

data LambdaParameterType = 
  LambdaParameterTypeType UnannType |
  LambdaParameterTypeVar 
  deriving (Eq, Ord, Read, Show)

_LambdaParameterType = (Core.Name "hydra/langs/java/syntax.LambdaParameterType")

_LambdaParameterType_type = (Core.Name "type")

_LambdaParameterType_var = (Core.Name "var")

_LambdaParameterType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.LambdaParameterType"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _UnannType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "var"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data LambdaBody = 
  LambdaBodyExpression Expression |
  LambdaBodyBlock Block
  deriving (Eq, Ord, Read, Show)

_LambdaBody = (Core.Name "hydra/langs/java/syntax.LambdaBody")

_LambdaBody_expression = (Core.Name "expression")

_LambdaBody_block = (Core.Name "block")

_LambdaBody_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.LambdaBody"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "block"),
      Core.fieldTypeType = _Block_type_}]}))

data AssignmentExpression = 
  AssignmentExpressionConditional ConditionalExpression |
  AssignmentExpressionAssignment Assignment
  deriving (Eq, Ord, Read, Show)

_AssignmentExpression = (Core.Name "hydra/langs/java/syntax.AssignmentExpression")

_AssignmentExpression_conditional = (Core.Name "conditional")

_AssignmentExpression_assignment = (Core.Name "assignment")

_AssignmentExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.AssignmentExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "conditional"),
      Core.fieldTypeType = _ConditionalExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "assignment"),
      Core.fieldTypeType = _Assignment_type_}]}))

data Assignment = 
  Assignment {
    assignmentLhs :: LeftHandSide,
    assignmentOp :: AssignmentOperator,
    assignmentExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_Assignment = (Core.Name "hydra/langs/java/syntax.Assignment")

_Assignment_lhs = (Core.Name "lhs")

_Assignment_op = (Core.Name "op")

_Assignment_expression = (Core.Name "expression")

_Assignment_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.Assignment"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _LeftHandSide_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "op"),
      Core.fieldTypeType = _AssignmentOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_}]}))

data LeftHandSide = 
  LeftHandSideExpressionName ExpressionName |
  LeftHandSideFieldAccess FieldAccess |
  LeftHandSideArrayAccess ArrayAccess
  deriving (Eq, Ord, Read, Show)

_LeftHandSide = (Core.Name "hydra/langs/java/syntax.LeftHandSide")

_LeftHandSide_expressionName = (Core.Name "expressionName")

_LeftHandSide_fieldAccess = (Core.Name "fieldAccess")

_LeftHandSide_arrayAccess = (Core.Name "arrayAccess")

_LeftHandSide_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.LeftHandSide"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expressionName"),
      Core.fieldTypeType = _ExpressionName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fieldAccess"),
      Core.fieldTypeType = _FieldAccess_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arrayAccess"),
      Core.fieldTypeType = _ArrayAccess_type_}]}))

data AssignmentOperator = 
  AssignmentOperatorSimple  |
  AssignmentOperatorTimes  |
  AssignmentOperatorDiv  |
  AssignmentOperatorMod  |
  AssignmentOperatorPlus  |
  AssignmentOperatorMinus  |
  AssignmentOperatorShiftLeft  |
  AssignmentOperatorShiftRight  |
  AssignmentOperatorShiftRightZeroFill  |
  AssignmentOperatorAnd  |
  AssignmentOperatorXor  |
  AssignmentOperatorOr 
  deriving (Eq, Ord, Read, Show)

_AssignmentOperator = (Core.Name "hydra/langs/java/syntax.AssignmentOperator")

_AssignmentOperator_simple = (Core.Name "simple")

_AssignmentOperator_times = (Core.Name "times")

_AssignmentOperator_div = (Core.Name "div")

_AssignmentOperator_mod = (Core.Name "mod")

_AssignmentOperator_plus = (Core.Name "plus")

_AssignmentOperator_minus = (Core.Name "minus")

_AssignmentOperator_shiftLeft = (Core.Name "shiftLeft")

_AssignmentOperator_shiftRight = (Core.Name "shiftRight")

_AssignmentOperator_shiftRightZeroFill = (Core.Name "shiftRightZeroFill")

_AssignmentOperator_and = (Core.Name "and")

_AssignmentOperator_xor = (Core.Name "xor")

_AssignmentOperator_or = (Core.Name "or")

_AssignmentOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.AssignmentOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simple"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "times"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "div"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mod"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "plus"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "minus"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "shiftLeft"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "shiftRight"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "shiftRightZeroFill"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "and"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "xor"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "or"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data ConditionalExpression = 
  ConditionalExpressionSimple ConditionalOrExpression |
  ConditionalExpressionTernaryCond ConditionalExpression_TernaryCond |
  ConditionalExpressionTernaryLambda ConditionalExpression_TernaryLambda
  deriving (Eq, Ord, Read, Show)

_ConditionalExpression = (Core.Name "hydra/langs/java/syntax.ConditionalExpression")

_ConditionalExpression_simple = (Core.Name "simple")

_ConditionalExpression_ternaryCond = (Core.Name "ternaryCond")

_ConditionalExpression_ternaryLambda = (Core.Name "ternaryLambda")

_ConditionalExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ConditionalExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simple"),
      Core.fieldTypeType = _ConditionalOrExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ternaryCond"),
      Core.fieldTypeType = _ConditionalExpression_TernaryCond_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ternaryLambda"),
      Core.fieldTypeType = _ConditionalExpression_TernaryLambda_type_}]}))

data ConditionalExpression_TernaryCond = 
  ConditionalExpression_TernaryCond {
    conditionalExpression_TernaryCondCond :: ConditionalOrExpression,
    conditionalExpression_TernaryCondIfTrue :: Expression,
    conditionalExpression_TernaryCondIfFalse :: ConditionalExpression}
  deriving (Eq, Ord, Read, Show)

_ConditionalExpression_TernaryCond = (Core.Name "hydra/langs/java/syntax.ConditionalExpression.TernaryCond")

_ConditionalExpression_TernaryCond_cond = (Core.Name "cond")

_ConditionalExpression_TernaryCond_ifTrue = (Core.Name "ifTrue")

_ConditionalExpression_TernaryCond_ifFalse = (Core.Name "ifFalse")

_ConditionalExpression_TernaryCond_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ConditionalExpression.TernaryCond"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = _ConditionalOrExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ifTrue"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ifFalse"),
      Core.fieldTypeType = _ConditionalExpression_type_}]}))

data ConditionalExpression_TernaryLambda = 
  ConditionalExpression_TernaryLambda {
    conditionalExpression_TernaryLambdaCond :: ConditionalOrExpression,
    conditionalExpression_TernaryLambdaIfTrue :: Expression,
    conditionalExpression_TernaryLambdaIfFalse :: LambdaExpression}
  deriving (Eq, Ord, Read, Show)

_ConditionalExpression_TernaryLambda = (Core.Name "hydra/langs/java/syntax.ConditionalExpression.TernaryLambda")

_ConditionalExpression_TernaryLambda_cond = (Core.Name "cond")

_ConditionalExpression_TernaryLambda_ifTrue = (Core.Name "ifTrue")

_ConditionalExpression_TernaryLambda_ifFalse = (Core.Name "ifFalse")

_ConditionalExpression_TernaryLambda_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ConditionalExpression.TernaryLambda"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cond"),
      Core.fieldTypeType = _ConditionalOrExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ifTrue"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ifFalse"),
      Core.fieldTypeType = _LambdaExpression_type_}]}))

newtype ConditionalOrExpression = 
  ConditionalOrExpression {
    unConditionalOrExpression :: [ConditionalAndExpression]}
  deriving (Eq, Ord, Read, Show)

_ConditionalOrExpression = (Core.Name "hydra/langs/java/syntax.ConditionalOrExpression")

_ConditionalOrExpression_type_ = (Core.TypeList _ConditionalAndExpression_type_)

newtype ConditionalAndExpression = 
  ConditionalAndExpression {
    unConditionalAndExpression :: [InclusiveOrExpression]}
  deriving (Eq, Ord, Read, Show)

_ConditionalAndExpression = (Core.Name "hydra/langs/java/syntax.ConditionalAndExpression")

_ConditionalAndExpression_type_ = (Core.TypeList _InclusiveOrExpression_type_)

newtype InclusiveOrExpression = 
  InclusiveOrExpression {
    unInclusiveOrExpression :: [ExclusiveOrExpression]}
  deriving (Eq, Ord, Read, Show)

_InclusiveOrExpression = (Core.Name "hydra/langs/java/syntax.InclusiveOrExpression")

_InclusiveOrExpression_type_ = (Core.TypeList _ExclusiveOrExpression_type_)

newtype ExclusiveOrExpression = 
  ExclusiveOrExpression {
    unExclusiveOrExpression :: [AndExpression]}
  deriving (Eq, Ord, Read, Show)

_ExclusiveOrExpression = (Core.Name "hydra/langs/java/syntax.ExclusiveOrExpression")

_ExclusiveOrExpression_type_ = (Core.TypeList _AndExpression_type_)

newtype AndExpression = 
  AndExpression {
    unAndExpression :: [EqualityExpression]}
  deriving (Eq, Ord, Read, Show)

_AndExpression = (Core.Name "hydra/langs/java/syntax.AndExpression")

_AndExpression_type_ = (Core.TypeList _EqualityExpression_type_)

data EqualityExpression = 
  EqualityExpressionUnary RelationalExpression |
  EqualityExpressionEqual EqualityExpression_Binary |
  EqualityExpressionNotEqual EqualityExpression_Binary
  deriving (Eq, Ord, Read, Show)

_EqualityExpression = (Core.Name "hydra/langs/java/syntax.EqualityExpression")

_EqualityExpression_unary = (Core.Name "unary")

_EqualityExpression_equal = (Core.Name "equal")

_EqualityExpression_notEqual = (Core.Name "notEqual")

_EqualityExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.EqualityExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unary"),
      Core.fieldTypeType = _RelationalExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "equal"),
      Core.fieldTypeType = _EqualityExpression_Binary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "notEqual"),
      Core.fieldTypeType = _EqualityExpression_Binary_type_}]}))

data EqualityExpression_Binary = 
  EqualityExpression_Binary {
    equalityExpression_BinaryLhs :: EqualityExpression,
    equalityExpression_BinaryRhs :: RelationalExpression}
  deriving (Eq, Ord, Read, Show)

_EqualityExpression_Binary = (Core.Name "hydra/langs/java/syntax.EqualityExpression.Binary")

_EqualityExpression_Binary_lhs = (Core.Name "lhs")

_EqualityExpression_Binary_rhs = (Core.Name "rhs")

_EqualityExpression_Binary_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.EqualityExpression.Binary"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _EqualityExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _RelationalExpression_type_}]}))

data RelationalExpression = 
  RelationalExpressionSimple ShiftExpression |
  RelationalExpressionLessThan RelationalExpression_LessThan |
  RelationalExpressionGreaterThan RelationalExpression_GreaterThan |
  RelationalExpressionLessThanEqual RelationalExpression_LessThanEqual |
  RelationalExpressionGreaterThanEqual RelationalExpression_GreaterThanEqual |
  RelationalExpressionInstanceof RelationalExpression_InstanceOf
  deriving (Eq, Ord, Read, Show)

_RelationalExpression = (Core.Name "hydra/langs/java/syntax.RelationalExpression")

_RelationalExpression_simple = (Core.Name "simple")

_RelationalExpression_lessThan = (Core.Name "lessThan")

_RelationalExpression_greaterThan = (Core.Name "greaterThan")

_RelationalExpression_lessThanEqual = (Core.Name "lessThanEqual")

_RelationalExpression_greaterThanEqual = (Core.Name "greaterThanEqual")

_RelationalExpression_instanceof = (Core.Name "instanceof")

_RelationalExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.RelationalExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simple"),
      Core.fieldTypeType = _ShiftExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lessThan"),
      Core.fieldTypeType = _RelationalExpression_LessThan_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "greaterThan"),
      Core.fieldTypeType = _RelationalExpression_GreaterThan_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lessThanEqual"),
      Core.fieldTypeType = _RelationalExpression_LessThanEqual_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "greaterThanEqual"),
      Core.fieldTypeType = _RelationalExpression_GreaterThanEqual_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "instanceof"),
      Core.fieldTypeType = _RelationalExpression_InstanceOf_type_}]}))

data RelationalExpression_LessThan = 
  RelationalExpression_LessThan {
    relationalExpression_LessThanLhs :: RelationalExpression,
    relationalExpression_LessThanRhs :: ShiftExpression}
  deriving (Eq, Ord, Read, Show)

_RelationalExpression_LessThan = (Core.Name "hydra/langs/java/syntax.RelationalExpression.LessThan")

_RelationalExpression_LessThan_lhs = (Core.Name "lhs")

_RelationalExpression_LessThan_rhs = (Core.Name "rhs")

_RelationalExpression_LessThan_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.RelationalExpression.LessThan"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _RelationalExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _ShiftExpression_type_}]}))

data RelationalExpression_GreaterThan = 
  RelationalExpression_GreaterThan {
    relationalExpression_GreaterThanLhs :: RelationalExpression,
    relationalExpression_GreaterThanRhs :: ShiftExpression}
  deriving (Eq, Ord, Read, Show)

_RelationalExpression_GreaterThan = (Core.Name "hydra/langs/java/syntax.RelationalExpression.GreaterThan")

_RelationalExpression_GreaterThan_lhs = (Core.Name "lhs")

_RelationalExpression_GreaterThan_rhs = (Core.Name "rhs")

_RelationalExpression_GreaterThan_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.RelationalExpression.GreaterThan"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _RelationalExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _ShiftExpression_type_}]}))

data RelationalExpression_LessThanEqual = 
  RelationalExpression_LessThanEqual {
    relationalExpression_LessThanEqualLhs :: RelationalExpression,
    relationalExpression_LessThanEqualRhs :: ShiftExpression}
  deriving (Eq, Ord, Read, Show)

_RelationalExpression_LessThanEqual = (Core.Name "hydra/langs/java/syntax.RelationalExpression.LessThanEqual")

_RelationalExpression_LessThanEqual_lhs = (Core.Name "lhs")

_RelationalExpression_LessThanEqual_rhs = (Core.Name "rhs")

_RelationalExpression_LessThanEqual_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.RelationalExpression.LessThanEqual"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _RelationalExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _ShiftExpression_type_}]}))

data RelationalExpression_GreaterThanEqual = 
  RelationalExpression_GreaterThanEqual {
    relationalExpression_GreaterThanEqualLhs :: RelationalExpression,
    relationalExpression_GreaterThanEqualRhs :: ShiftExpression}
  deriving (Eq, Ord, Read, Show)

_RelationalExpression_GreaterThanEqual = (Core.Name "hydra/langs/java/syntax.RelationalExpression.GreaterThanEqual")

_RelationalExpression_GreaterThanEqual_lhs = (Core.Name "lhs")

_RelationalExpression_GreaterThanEqual_rhs = (Core.Name "rhs")

_RelationalExpression_GreaterThanEqual_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.RelationalExpression.GreaterThanEqual"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _RelationalExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _ShiftExpression_type_}]}))

data RelationalExpression_InstanceOf = 
  RelationalExpression_InstanceOf {
    relationalExpression_InstanceOfLhs :: RelationalExpression,
    relationalExpression_InstanceOfRhs :: ReferenceType}
  deriving (Eq, Ord, Read, Show)

_RelationalExpression_InstanceOf = (Core.Name "hydra/langs/java/syntax.RelationalExpression.InstanceOf")

_RelationalExpression_InstanceOf_lhs = (Core.Name "lhs")

_RelationalExpression_InstanceOf_rhs = (Core.Name "rhs")

_RelationalExpression_InstanceOf_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.RelationalExpression.InstanceOf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _RelationalExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _ReferenceType_type_}]}))

data ShiftExpression = 
  ShiftExpressionUnary AdditiveExpression |
  ShiftExpressionShiftLeft ShiftExpression_Binary |
  ShiftExpressionShiftRight ShiftExpression_Binary |
  ShiftExpressionShiftRightZeroFill ShiftExpression_Binary
  deriving (Eq, Ord, Read, Show)

_ShiftExpression = (Core.Name "hydra/langs/java/syntax.ShiftExpression")

_ShiftExpression_unary = (Core.Name "unary")

_ShiftExpression_shiftLeft = (Core.Name "shiftLeft")

_ShiftExpression_shiftRight = (Core.Name "shiftRight")

_ShiftExpression_shiftRightZeroFill = (Core.Name "shiftRightZeroFill")

_ShiftExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ShiftExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unary"),
      Core.fieldTypeType = _AdditiveExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "shiftLeft"),
      Core.fieldTypeType = _ShiftExpression_Binary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "shiftRight"),
      Core.fieldTypeType = _ShiftExpression_Binary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "shiftRightZeroFill"),
      Core.fieldTypeType = _ShiftExpression_Binary_type_}]}))

data ShiftExpression_Binary = 
  ShiftExpression_Binary {
    shiftExpression_BinaryLhs :: ShiftExpression,
    shiftExpression_BinaryRhs :: AdditiveExpression}
  deriving (Eq, Ord, Read, Show)

_ShiftExpression_Binary = (Core.Name "hydra/langs/java/syntax.ShiftExpression.Binary")

_ShiftExpression_Binary_lhs = (Core.Name "lhs")

_ShiftExpression_Binary_rhs = (Core.Name "rhs")

_ShiftExpression_Binary_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.ShiftExpression.Binary"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _ShiftExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _AdditiveExpression_type_}]}))

data AdditiveExpression = 
  AdditiveExpressionUnary MultiplicativeExpression |
  AdditiveExpressionPlus AdditiveExpression_Binary |
  AdditiveExpressionMinus AdditiveExpression_Binary
  deriving (Eq, Ord, Read, Show)

_AdditiveExpression = (Core.Name "hydra/langs/java/syntax.AdditiveExpression")

_AdditiveExpression_unary = (Core.Name "unary")

_AdditiveExpression_plus = (Core.Name "plus")

_AdditiveExpression_minus = (Core.Name "minus")

_AdditiveExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.AdditiveExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unary"),
      Core.fieldTypeType = _MultiplicativeExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "plus"),
      Core.fieldTypeType = _AdditiveExpression_Binary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "minus"),
      Core.fieldTypeType = _AdditiveExpression_Binary_type_}]}))

data AdditiveExpression_Binary = 
  AdditiveExpression_Binary {
    additiveExpression_BinaryLhs :: AdditiveExpression,
    additiveExpression_BinaryRhs :: MultiplicativeExpression}
  deriving (Eq, Ord, Read, Show)

_AdditiveExpression_Binary = (Core.Name "hydra/langs/java/syntax.AdditiveExpression.Binary")

_AdditiveExpression_Binary_lhs = (Core.Name "lhs")

_AdditiveExpression_Binary_rhs = (Core.Name "rhs")

_AdditiveExpression_Binary_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.AdditiveExpression.Binary"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _AdditiveExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _MultiplicativeExpression_type_}]}))

data MultiplicativeExpression = 
  MultiplicativeExpressionUnary UnaryExpression |
  MultiplicativeExpressionTimes MultiplicativeExpression_Binary |
  MultiplicativeExpressionDivide MultiplicativeExpression_Binary |
  MultiplicativeExpressionMod MultiplicativeExpression_Binary
  deriving (Eq, Ord, Read, Show)

_MultiplicativeExpression = (Core.Name "hydra/langs/java/syntax.MultiplicativeExpression")

_MultiplicativeExpression_unary = (Core.Name "unary")

_MultiplicativeExpression_times = (Core.Name "times")

_MultiplicativeExpression_divide = (Core.Name "divide")

_MultiplicativeExpression_mod = (Core.Name "mod")

_MultiplicativeExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MultiplicativeExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unary"),
      Core.fieldTypeType = _UnaryExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "times"),
      Core.fieldTypeType = _MultiplicativeExpression_Binary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "divide"),
      Core.fieldTypeType = _MultiplicativeExpression_Binary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mod"),
      Core.fieldTypeType = _MultiplicativeExpression_Binary_type_}]}))

data MultiplicativeExpression_Binary = 
  MultiplicativeExpression_Binary {
    multiplicativeExpression_BinaryLhs :: MultiplicativeExpression,
    multiplicativeExpression_BinaryRhs :: UnaryExpression}
  deriving (Eq, Ord, Read, Show)

_MultiplicativeExpression_Binary = (Core.Name "hydra/langs/java/syntax.MultiplicativeExpression.Binary")

_MultiplicativeExpression_Binary_lhs = (Core.Name "lhs")

_MultiplicativeExpression_Binary_rhs = (Core.Name "rhs")

_MultiplicativeExpression_Binary_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.MultiplicativeExpression.Binary"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _MultiplicativeExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _UnaryExpression_type_}]}))

data UnaryExpression = 
  UnaryExpressionPreIncrement PreIncrementExpression |
  UnaryExpressionPreDecrement PreDecrementExpression |
  UnaryExpressionPlus UnaryExpression |
  UnaryExpressionMinus UnaryExpression |
  UnaryExpressionOther UnaryExpressionNotPlusMinus
  deriving (Eq, Ord, Read, Show)

_UnaryExpression = (Core.Name "hydra/langs/java/syntax.UnaryExpression")

_UnaryExpression_preIncrement = (Core.Name "preIncrement")

_UnaryExpression_preDecrement = (Core.Name "preDecrement")

_UnaryExpression_plus = (Core.Name "plus")

_UnaryExpression_minus = (Core.Name "minus")

_UnaryExpression_other = (Core.Name "other")

_UnaryExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.UnaryExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "preIncrement"),
      Core.fieldTypeType = _PreIncrementExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "preDecrement"),
      Core.fieldTypeType = _PreDecrementExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "plus"),
      Core.fieldTypeType = _UnaryExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "minus"),
      Core.fieldTypeType = _UnaryExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "other"),
      Core.fieldTypeType = _UnaryExpressionNotPlusMinus_type_}]}))

newtype PreIncrementExpression = 
  PreIncrementExpression {
    unPreIncrementExpression :: UnaryExpression}
  deriving (Eq, Ord, Read, Show)

_PreIncrementExpression = (Core.Name "hydra/langs/java/syntax.PreIncrementExpression")

_PreIncrementExpression_type_ = _UnaryExpression_type_

newtype PreDecrementExpression = 
  PreDecrementExpression {
    unPreDecrementExpression :: UnaryExpression}
  deriving (Eq, Ord, Read, Show)

_PreDecrementExpression = (Core.Name "hydra/langs/java/syntax.PreDecrementExpression")

_PreDecrementExpression_type_ = _UnaryExpression_type_

data UnaryExpressionNotPlusMinus = 
  UnaryExpressionNotPlusMinusPostfix PostfixExpression |
  UnaryExpressionNotPlusMinusTilde UnaryExpression |
  UnaryExpressionNotPlusMinusNot UnaryExpression |
  UnaryExpressionNotPlusMinusCast CastExpression
  deriving (Eq, Ord, Read, Show)

_UnaryExpressionNotPlusMinus = (Core.Name "hydra/langs/java/syntax.UnaryExpressionNotPlusMinus")

_UnaryExpressionNotPlusMinus_postfix = (Core.Name "postfix")

_UnaryExpressionNotPlusMinus_tilde = (Core.Name "tilde")

_UnaryExpressionNotPlusMinus_not = (Core.Name "not")

_UnaryExpressionNotPlusMinus_cast = (Core.Name "cast")

_UnaryExpressionNotPlusMinus_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.UnaryExpressionNotPlusMinus"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "postfix"),
      Core.fieldTypeType = _PostfixExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tilde"),
      Core.fieldTypeType = _UnaryExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "not"),
      Core.fieldTypeType = _UnaryExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cast"),
      Core.fieldTypeType = _CastExpression_type_}]}))

data PostfixExpression = 
  PostfixExpressionPrimary Primary |
  PostfixExpressionName ExpressionName |
  PostfixExpressionPostIncrement PostIncrementExpression |
  PostfixExpressionPostDecrement PostDecrementExpression
  deriving (Eq, Ord, Read, Show)

_PostfixExpression = (Core.Name "hydra/langs/java/syntax.PostfixExpression")

_PostfixExpression_primary = (Core.Name "primary")

_PostfixExpression_name = (Core.Name "name")

_PostfixExpression_postIncrement = (Core.Name "postIncrement")

_PostfixExpression_postDecrement = (Core.Name "postDecrement")

_PostfixExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.PostfixExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primary"),
      Core.fieldTypeType = _Primary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _ExpressionName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "postIncrement"),
      Core.fieldTypeType = _PostIncrementExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "postDecrement"),
      Core.fieldTypeType = _PostDecrementExpression_type_}]}))

newtype PostIncrementExpression = 
  PostIncrementExpression {
    unPostIncrementExpression :: PostfixExpression}
  deriving (Eq, Ord, Read, Show)

_PostIncrementExpression = (Core.Name "hydra/langs/java/syntax.PostIncrementExpression")

_PostIncrementExpression_type_ = _PostfixExpression_type_

newtype PostDecrementExpression = 
  PostDecrementExpression {
    unPostDecrementExpression :: PostfixExpression}
  deriving (Eq, Ord, Read, Show)

_PostDecrementExpression = (Core.Name "hydra/langs/java/syntax.PostDecrementExpression")

_PostDecrementExpression_type_ = _PostfixExpression_type_

data CastExpression = 
  CastExpressionPrimitive CastExpression_Primitive |
  CastExpressionNotPlusMinus CastExpression_NotPlusMinus |
  CastExpressionLambda CastExpression_Lambda
  deriving (Eq, Ord, Read, Show)

_CastExpression = (Core.Name "hydra/langs/java/syntax.CastExpression")

_CastExpression_primitive = (Core.Name "primitive")

_CastExpression_notPlusMinus = (Core.Name "notPlusMinus")

_CastExpression_lambda = (Core.Name "lambda")

_CastExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.CastExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primitive"),
      Core.fieldTypeType = _CastExpression_Primitive_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "notPlusMinus"),
      Core.fieldTypeType = _CastExpression_NotPlusMinus_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lambda"),
      Core.fieldTypeType = _CastExpression_Lambda_type_}]}))

data CastExpression_Primitive = 
  CastExpression_Primitive {
    castExpression_PrimitiveType :: PrimitiveTypeWithAnnotations,
    castExpression_PrimitiveExpression :: UnaryExpression}
  deriving (Eq, Ord, Read, Show)

_CastExpression_Primitive = (Core.Name "hydra/langs/java/syntax.CastExpression.Primitive")

_CastExpression_Primitive_type = (Core.Name "type")

_CastExpression_Primitive_expression = (Core.Name "expression")

_CastExpression_Primitive_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.CastExpression.Primitive"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _PrimitiveTypeWithAnnotations_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _UnaryExpression_type_}]}))

data CastExpression_NotPlusMinus = 
  CastExpression_NotPlusMinus {
    castExpression_NotPlusMinusRefAndBounds :: CastExpression_RefAndBounds,
    castExpression_NotPlusMinusExpression :: UnaryExpression}
  deriving (Eq, Ord, Read, Show)

_CastExpression_NotPlusMinus = (Core.Name "hydra/langs/java/syntax.CastExpression.NotPlusMinus")

_CastExpression_NotPlusMinus_refAndBounds = (Core.Name "refAndBounds")

_CastExpression_NotPlusMinus_expression = (Core.Name "expression")

_CastExpression_NotPlusMinus_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.CastExpression.NotPlusMinus"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "refAndBounds"),
      Core.fieldTypeType = _CastExpression_RefAndBounds_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _UnaryExpression_type_}]}))

data CastExpression_Lambda = 
  CastExpression_Lambda {
    castExpression_LambdaRefAndBounds :: CastExpression_RefAndBounds,
    castExpression_LambdaExpression :: LambdaExpression}
  deriving (Eq, Ord, Read, Show)

_CastExpression_Lambda = (Core.Name "hydra/langs/java/syntax.CastExpression.Lambda")

_CastExpression_Lambda_refAndBounds = (Core.Name "refAndBounds")

_CastExpression_Lambda_expression = (Core.Name "expression")

_CastExpression_Lambda_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.CastExpression.Lambda"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "refAndBounds"),
      Core.fieldTypeType = _CastExpression_RefAndBounds_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _LambdaExpression_type_}]}))

data CastExpression_RefAndBounds = 
  CastExpression_RefAndBounds {
    castExpression_RefAndBoundsType :: ReferenceType,
    castExpression_RefAndBoundsBounds :: [AdditionalBound]}
  deriving (Eq, Ord, Read, Show)

_CastExpression_RefAndBounds = (Core.Name "hydra/langs/java/syntax.CastExpression.RefAndBounds")

_CastExpression_RefAndBounds_type = (Core.Name "type")

_CastExpression_RefAndBounds_bounds = (Core.Name "bounds")

_CastExpression_RefAndBounds_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/java/syntax.CastExpression.RefAndBounds"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _ReferenceType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bounds"),
      Core.fieldTypeType = (Core.TypeList _AdditionalBound_type_)}]}))

newtype ConstantExpression = 
  ConstantExpression {
    unConstantExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_ConstantExpression = (Core.Name "hydra/langs/java/syntax.ConstantExpression")

_ConstantExpression_type_ = _Expression_type_