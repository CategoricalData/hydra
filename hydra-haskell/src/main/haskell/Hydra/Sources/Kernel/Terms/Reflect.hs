
module Hydra.Sources.Kernel.Terms.Reflect where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  eliminationVariant, eliminationVariants, floatTypePrecision, floatTypes, floatValueType,
  functionVariant, functionVariants, integerTypeIsSigned, integerTypePrecision, integerTypes,
  integerValueType, literalType, literalTypeVariant, literalTypes, literalVariant, literalVariants,
  termVariant, termVariants, typeVariant, typeVariants)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


ns :: Namespace
ns = Namespace "hydra.reflect"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces $
    Just ("Reflection functions for working with term, type, and literal type variants, as well as numeric precision.")
  where
    elements = [
      toBinding eliminationVariant,
      toBinding eliminationVariants,
      toBinding floatTypePrecision,
      toBinding floatTypes,
      toBinding floatValueType,
      toBinding functionVariant,
      toBinding functionVariants,
      toBinding integerTypeIsSigned,
      toBinding integerTypePrecision,
      toBinding integerTypes,
      toBinding integerValueType,
      toBinding literalType,
      toBinding literalTypeVariant,
      toBinding literalTypes,
      toBinding literalVariant,
      toBinding literalVariants,
      toBinding termVariant,
      toBinding termVariants,
      toBinding typeVariant,
      toBinding typeVariants]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

eliminationVariant :: TBinding (Elimination -> EliminationVariant)
eliminationVariant = define "eliminationVariant" $
  doc "Find the elimination inject (constructor) for a given elimination term" $
  match _Elimination Nothing [
    _Elimination_record>>: constant Variants.eliminationVariantRecord,
    _Elimination_union>>: constant Variants.eliminationVariantUnion,
    _Elimination_wrap>>: constant Variants.eliminationVariantWrap]

eliminationVariants :: TBinding [EliminationVariant]
eliminationVariants = define "eliminationVariants" $
  doc "All elimination variants (constructors), in a canonical order" $
  list $ injectUnit _EliminationVariant <$> [
    _EliminationVariant_record,
    _EliminationVariant_union,
    _EliminationVariant_wrap]

floatTypePrecision :: TBinding (FloatType -> Precision)
floatTypePrecision = define "floatTypePrecision" $
  doc "Find the precision of a given floating-point type" $
  match _FloatType Nothing [
    _FloatType_bigfloat>>: constant Util.precisionArbitrary,
    _FloatType_float32>>: constant $ Util.precisionBits $ int32 32,
    _FloatType_float64>>: constant $ Util.precisionBits $ int32 64]

floatTypes :: TBinding [FloatType]
floatTypes = define "floatTypes" $
  doc "All floating-point types in a canonical order" $
  list $ injectUnit _FloatType <$> [
    _FloatType_bigfloat,
    _FloatType_float32,
    _FloatType_float64]

floatValueType :: TBinding (FloatValue -> FloatType)
floatValueType = define "floatValueType" $
  doc "Find the float type for a given floating-point value" $
  match _FloatValue Nothing [
    _FloatValue_bigfloat>>: constant Core.floatTypeBigfloat,
    _FloatValue_float32>>: constant Core.floatTypeFloat32,
    _FloatValue_float64>>: constant Core.floatTypeFloat64]

functionVariant :: TBinding (Function -> FunctionVariant)
functionVariant = define "functionVariant" $
  doc "Find the function inject (constructor) for a given function" $
  match _Function Nothing [
    _Function_elimination>>: constant Variants.functionVariantElimination,
    _Function_lambda>>: constant Variants.functionVariantLambda,
    _Function_primitive>>: constant Variants.functionVariantPrimitive]

functionVariants :: TBinding [FunctionVariant]
functionVariants = define "functionVariants" $
  doc "All function variants (constructors), in a canonical order" $
  list $ injectUnit _FunctionVariant <$> [
    _FunctionVariant_elimination,
    _FunctionVariant_lambda,
    _FunctionVariant_primitive]

integerTypeIsSigned :: TBinding (IntegerType -> Bool)
integerTypeIsSigned = define "integerTypeIsSigned" $
  doc "Find whether a given integer type is signed (true) or unsigned (false)" $
  match _IntegerType Nothing [
    _IntegerType_bigint>>: constant true,
    _IntegerType_int8>>:   constant true,
    _IntegerType_int16>>:  constant true,
    _IntegerType_int32>>:  constant true,
    _IntegerType_int64>>:  constant true,
    _IntegerType_uint8>>:  constant false,
    _IntegerType_uint16>>: constant false,
    _IntegerType_uint32>>: constant false,
    _IntegerType_uint64>>: constant false]

integerTypePrecision :: TBinding (IntegerType -> Precision)
integerTypePrecision = define "integerTypePrecision" $
  doc "Find the precision of a given integer type" $
  match _IntegerType Nothing [
    _IntegerType_bigint>>: constant Util.precisionArbitrary,
    _IntegerType_int8>>: constant $ Util.precisionBits $ int32 8,
    _IntegerType_int16>>: constant $ Util.precisionBits $ int32 16,
    _IntegerType_int32>>: constant $ Util.precisionBits $ int32 32,
    _IntegerType_int64>>: constant $ Util.precisionBits $ int32 64,
    _IntegerType_uint8>>: constant $ Util.precisionBits $ int32 8,
    _IntegerType_uint16>>: constant $ Util.precisionBits $ int32 16,
    _IntegerType_uint32>>: constant $ Util.precisionBits $ int32 32,
    _IntegerType_uint64>>: constant $ Util.precisionBits $ int32 64]

integerTypes :: TBinding [IntegerType]
integerTypes = define "integerTypes" $
  doc "All integer types, in a canonical order" $
  list $ injectUnit _IntegerType <$> [
    _IntegerType_bigint,
    _IntegerType_int8,
    _IntegerType_int16,
    _IntegerType_int32,
    _IntegerType_int64,
    _IntegerType_uint8,
    _IntegerType_uint16,
    _IntegerType_uint32,
    _IntegerType_uint64]

integerValueType :: TBinding (IntegerValue -> IntegerType)
integerValueType = define "integerValueType" $
  doc "Find the integer type for a given integer value" $
  match _IntegerValue Nothing [
    _IntegerValue_bigint>>: constant Core.integerTypeBigint,
    _IntegerValue_int8>>: constant Core.integerTypeInt8,
    _IntegerValue_int16>>: constant Core.integerTypeInt16,
    _IntegerValue_int32>>: constant Core.integerTypeInt32,
    _IntegerValue_int64>>: constant Core.integerTypeInt64,
    _IntegerValue_uint8>>: constant Core.integerTypeUint8,
    _IntegerValue_uint16>>: constant Core.integerTypeUint16,
    _IntegerValue_uint32>>: constant Core.integerTypeUint32,
    _IntegerValue_uint64>>: constant Core.integerTypeUint64]

literalType :: TBinding (Literal -> LiteralType)
literalType = define "literalType" $
  doc "Find the literal type for a given literal value" $
  match _Literal Nothing [
    _Literal_binary>>: constant $ inject _LiteralType _LiteralType_binary unit,
    _Literal_boolean>>: constant $ inject _LiteralType _LiteralType_boolean unit,
    _Literal_float>>: injectLambda _LiteralType _LiteralType_float <.> floatValueType,
    _Literal_integer>>: injectLambda _LiteralType _LiteralType_integer <.> integerValueType,
    _Literal_string>>: constant $ inject _LiteralType _LiteralType_string unit]

literalTypeVariant :: TBinding (LiteralType -> LiteralVariant)
literalTypeVariant = define "literalTypeVariant" $
  doc "Find the literal type inject (constructor) for a given literal value" $
  match _LiteralType Nothing [
    _LiteralType_binary>>:  constant $ Variants.literalVariantBinary,
    _LiteralType_boolean>>: constant $ Variants.literalVariantBoolean,
    _LiteralType_float>>:   constant $ Variants.literalVariantFloat,
    _LiteralType_integer>>: constant $ Variants.literalVariantInteger,
    _LiteralType_string>>:  constant $ Variants.literalVariantString]

literalTypes :: TBinding [LiteralType]
literalTypes = define "literalTypes" $
  doc "All literal types, in a canonical order" $
  Lists.concat $ list [
    list [
      Core.literalTypeBinary,
      Core.literalTypeBoolean],
    Lists.map (unaryFunction Core.literalTypeFloat) (floatTypes),
    Lists.map (unaryFunction Core.literalTypeInteger) (integerTypes),
    list [
      Core.literalTypeString]]

literalVariant :: TBinding (Literal -> LiteralVariant)
literalVariant = define "literalVariant" $
  doc "Find the literal inject (constructor) for a given literal value" $
  literalTypeVariant <.> literalType

literalVariants :: TBinding [LiteralVariant]
literalVariants = define "literalVariants" $
  doc "All literal variants, in a canonical order" $
  list $ injectUnit _LiteralVariant <$> [
    _LiteralVariant_binary,
    _LiteralVariant_boolean,
    _LiteralVariant_float,
    _LiteralVariant_integer,
    _LiteralVariant_string]

termVariant :: TBinding (Term -> TermVariant)
termVariant = define "termVariant" $
  doc "Find the term inject (constructor) for a given term" $
  match _Term Nothing [
    _Term_annotated>>: constant Variants.termVariantAnnotated,
    _Term_application>>: constant Variants.termVariantApplication,
    _Term_either>>: constant Variants.termVariantEither,
    _Term_function>>: constant Variants.termVariantFunction,
    _Term_let>>: constant Variants.termVariantLet,
    _Term_list>>: constant Variants.termVariantList,
    _Term_literal>>: constant Variants.termVariantLiteral,
    _Term_map>>: constant Variants.termVariantMap,
    _Term_maybe>>: constant Variants.termVariantMaybe,
    _Term_pair>>: constant Variants.termVariantPair,
    _Term_record>>: constant Variants.termVariantRecord,
    _Term_set>>: constant Variants.termVariantSet,
    _Term_typeApplication>>: constant Variants.termVariantTypeApplication,
    _Term_typeLambda>>: constant Variants.termVariantTypeLambda,
    _Term_union>>: constant Variants.termVariantUnion,
    _Term_unit>>: constant Variants.termVariantUnit,
    _Term_variable>>: constant Variants.termVariantVariable,
    _Term_wrap>>: constant Variants.termVariantWrap]

termVariants :: TBinding [TermVariant]
termVariants = define "termVariants" $
  doc "All term (expression) variants, in a canonical order" $
  list $ injectUnit _TermVariant <$> [
    _TermVariant_annotated,
    _TermVariant_application,
    _TermVariant_either,
    _TermVariant_function,
    _TermVariant_list,
    _TermVariant_literal,
    _TermVariant_map,
    _TermVariant_maybe,
    _TermVariant_pair,
    _TermVariant_record,
    _TermVariant_set,
    _TermVariant_typeLambda,
    _TermVariant_typeApplication,
    _TermVariant_union,
    _TermVariant_unit,
    _TermVariant_variable,
    _TermVariant_wrap]

typeVariant :: TBinding (Type -> TypeVariant)
typeVariant = define "typeVariant" $
  doc "Find the type inject (constructor) for a given type" $
  match _Type Nothing [
    _Type_annotated>>: constant Variants.typeVariantAnnotated,
    _Type_application>>: constant Variants.typeVariantApplication,
    _Type_either>>: constant Variants.typeVariantEither,
    _Type_function>>: constant Variants.typeVariantFunction,
    _Type_forall>>: constant Variants.typeVariantForall,
    _Type_list>>: constant Variants.typeVariantList,
    _Type_literal>>: constant Variants.typeVariantLiteral,
    _Type_map>>: constant Variants.typeVariantMap,
    _Type_maybe>>: constant Variants.typeVariantMaybe,
    _Type_pair>>: constant Variants.typeVariantPair,
    _Type_record>>: constant Variants.typeVariantRecord,
    _Type_set>>: constant Variants.typeVariantSet,
    _Type_union>>: constant Variants.typeVariantUnion,
    _Type_unit>>: constant Variants.typeVariantUnit,
    _Type_variable>>: constant Variants.typeVariantVariable,
    _Type_wrap>>: constant Variants.typeVariantWrap]

typeVariants :: TBinding [TypeVariant]
typeVariants = define "typeVariants" $
  doc "All type variants, in a canonical order" $
  list $ injectUnit _TypeVariant <$> [
    _TypeVariant_annotated,
    _TypeVariant_application,
    _TypeVariant_either,
    _TypeVariant_function,
    _TypeVariant_forall,
    _TypeVariant_list,
    _TypeVariant_literal,
    _TypeVariant_map,
    _TypeVariant_wrap,
    _TypeVariant_maybe,
    _TypeVariant_pair,
    _TypeVariant_record,
    _TypeVariant_set,
    _TypeVariant_union,
    _TypeVariant_unit,
    _TypeVariant_variable]
