{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Variants where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors              as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Coders                 as Coders
import qualified Hydra.Dsl.Compute                as Compute
import qualified Hydra.Dsl.Core                   as Core
import qualified Hydra.Dsl.Graph                  as Graph
import qualified Hydra.Dsl.Lib.Chars              as Chars
import qualified Hydra.Dsl.Lib.Equality           as Equality
import qualified Hydra.Dsl.Lib.Flows              as Flows
import qualified Hydra.Dsl.Lib.Lists              as Lists
import qualified Hydra.Dsl.Lib.Literals           as Literals
import qualified Hydra.Dsl.Lib.Logic              as Logic
import qualified Hydra.Dsl.Lib.Maps               as Maps
import qualified Hydra.Dsl.Lib.Math               as Math
import qualified Hydra.Dsl.Lib.Optionals          as Optionals
import           Hydra.Dsl.Phantoms               as Phantoms
import qualified Hydra.Dsl.Lib.Sets               as Sets
import           Hydra.Dsl.Lib.Strings            as Strings
import qualified Hydra.Dsl.Mantle                 as Mantle
import qualified Hydra.Dsl.Module                 as Module
import qualified Hydra.Dsl.TTerms                 as TTerms
import qualified Hydra.Dsl.TTypes                 as TTypes
import qualified Hydra.Dsl.Terms                  as Terms
import qualified Hydra.Dsl.Topology               as Topology
import qualified Hydra.Dsl.Types                  as Types
import qualified Hydra.Dsl.Typing                 as Typing
import qualified Hydra.Sources.Tier1.All          as Tier1
import qualified Hydra.Sources.Tier1.Constants    as Constants
import qualified Hydra.Sources.Tier1.Encode.Core as EncodeCore
import qualified Hydra.Sources.Tier1.Decode       as Decode
import qualified Hydra.Sources.Tier1.Formatting   as Formatting
import qualified Hydra.Sources.Tier1.Functions    as Functions
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Messages     as Messages
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.CoreLanguage as CoreLanguage
--import qualified Hydra.Sources.Tier2.Errors as Errors
--import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
--import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
--import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Show.Accessors as ShowAccessors
--import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.TermAdapters as TermAdapters
--import qualified Hydra.Sources.Tier2.TermEncoding as TermEncoding
--import qualified Hydra.Sources.Tier2.Unification as Unification
--import qualified Hydra.Sources.Tier2.Variants as Variants


variantsDefinition :: String -> TTerm a -> TElement a
variantsDefinition = definitionInModule hydraVariantsModule

hydraVariantsModule :: Module
hydraVariantsModule = Module (Namespace "hydra.variants") elements
    []
    [Tier1.hydraMantleModule] $
    Just ("Functions for working with term, type, and literal type variants, as well as numeric precision.")
  where
    elements = [
      el eliminationVariantDef,
      el eliminationVariantsDef,
      el floatTypePrecisionDef,
      el floatTypesDef,
      el floatValueTypeDef,
      el functionVariantDef,
      el functionVariantsDef,
      el integerTypeIsSignedDef,
      el integerTypePrecisionDef,
      el integerTypesDef,
      el integerValueTypeDef,
      el literalTypeDef,
      el literalTypeVariantDef,
      el literalVariantDef,
      el literalVariantsDef,
      el termVariantDef,
      el termVariantsDef,
      el typeVariantDef,
      el typeVariantsDef,
   
      -- Additional definitions; consider moving out of hydra.variants  
      el fieldMapDef,
      el fieldTypeMapDef]

eliminationVariantDef :: TElement (Elimination -> EliminationVariant)
eliminationVariantDef = variantsDefinition "eliminationVariant" $
  doc "Find the elimination variant (constructor) for a given elimination term" $
  match _Elimination Nothing [
    _Elimination_product>>: constant Mantle.eliminationVariantProduct,
    _Elimination_record>>: constant Mantle.eliminationVariantRecord,
    _Elimination_union>>: constant Mantle.eliminationVariantUnion,
    _Elimination_wrap>>: constant Mantle.eliminationVariantWrap]

eliminationVariantsDef :: TElement [EliminationVariant]
eliminationVariantsDef = variantsDefinition "eliminationVariants" $
  doc "All elimination variants (constructors), in a canonical order" $
  list $ unitVariant _EliminationVariant <$> [
    _EliminationVariant_product,
    _EliminationVariant_record,
    _EliminationVariant_union,
    _EliminationVariant_wrap]

floatTypePrecisionDef :: TElement (FloatType -> Precision)
floatTypePrecisionDef = variantsDefinition "floatTypePrecision" $
  doc "Find the precision of a given floating-point type" $
  match _FloatType Nothing [
    _FloatType_bigfloat>>: constant Mantle.precisionArbitrary,
    _FloatType_float32>>: constant $ Mantle.precisionBits $ int32 32,
    _FloatType_float64>>: constant $ Mantle.precisionBits $ int32 64]

floatTypesDef :: TElement [FloatType]
floatTypesDef = variantsDefinition "floatTypes" $
  doc "All floating-point types in a canonical order" $
  list $ unitVariant _FloatType <$> [
    _FloatType_bigfloat,
    _FloatType_float32,
    _FloatType_float64]

floatValueTypeDef :: TElement (FloatValue -> FloatType)
floatValueTypeDef = variantsDefinition "floatValueType" $
  doc "Find the float type for a given floating-point value" $
  match _FloatValue Nothing [
    _FloatValue_bigfloat>>: constant Core.floatTypeBigfloat,
    _FloatValue_float32>>: constant Core.floatTypeFloat32,
    _FloatValue_float64>>: constant Core.floatTypeFloat64]

functionVariantDef :: TElement (Function -> FunctionVariant)
functionVariantDef = variantsDefinition "functionVariant" $
  doc "Find the function variant (constructor) for a given function" $
  match _Function Nothing [
    _Function_elimination>>: constant Mantle.functionVariantElimination,
    _Function_lambda>>: constant Mantle.functionVariantLambda,
    _Function_primitive>>: constant Mantle.functionVariantPrimitive]

functionVariantsDef :: TElement [FunctionVariant]
functionVariantsDef = variantsDefinition "functionVariants" $
  doc "All function variants (constructors), in a canonical order" $
  list $ unitVariant _FunctionVariant <$> [
    _FunctionVariant_elimination,
    _FunctionVariant_lambda,
    _FunctionVariant_primitive]

integerTypeIsSignedDef :: TElement (IntegerType -> Bool)
integerTypeIsSignedDef = variantsDefinition "integerTypeIsSigned" $
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

integerTypePrecisionDef :: TElement (IntegerType -> Precision)
integerTypePrecisionDef = variantsDefinition "integerTypePrecision" $
  doc "Find the precision of a given integer type" $
  match _IntegerType Nothing [
    _IntegerType_bigint>>: constant Mantle.precisionArbitrary,
    _IntegerType_int8>>: constant $ Mantle.precisionBits $ int32 8,
    _IntegerType_int16>>: constant $ Mantle.precisionBits $ int32 16,
    _IntegerType_int32>>: constant $ Mantle.precisionBits $ int32 32,
    _IntegerType_int64>>: constant $ Mantle.precisionBits $ int32 64,
    _IntegerType_uint8>>: constant $ Mantle.precisionBits $ int32 8,
    _IntegerType_uint16>>: constant $ Mantle.precisionBits $ int32 16,
    _IntegerType_uint32>>: constant $ Mantle.precisionBits $ int32 32,
    _IntegerType_uint64>>: constant $ Mantle.precisionBits $ int32 64]

integerTypesDef :: TElement [IntegerType]
integerTypesDef = variantsDefinition "integerTypes" $
  doc "All integer types, in a canonical order" $
  list $ unitVariant _IntegerType <$> [
    _IntegerType_bigint,
    _IntegerType_int8,
    _IntegerType_int16,
    _IntegerType_int32,
    _IntegerType_int64,
    _IntegerType_uint8,
    _IntegerType_uint16,
    _IntegerType_uint32,
    _IntegerType_uint64]

integerValueTypeDef :: TElement (IntegerValue -> IntegerType)
integerValueTypeDef = variantsDefinition "integerValueType" $
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

literalTypeDef :: TElement (Literal -> LiteralType)
literalTypeDef = variantsDefinition "literalType" $
  doc "Find the literal type for a given literal value" $
  match _Literal Nothing [
    _Literal_binary>>: constant $ variant _LiteralType _LiteralType_binary unit,
    _Literal_boolean>>: constant $ variant _LiteralType _LiteralType_boolean unit,
    _Literal_float>>: injectLambda _LiteralType _LiteralType_float <.> ref floatValueTypeDef,
    _Literal_integer>>: injectLambda _LiteralType _LiteralType_integer <.> ref integerValueTypeDef,
    _Literal_string>>: constant $ variant _LiteralType _LiteralType_string unit]

literalTypeVariantDef :: TElement (LiteralType -> LiteralVariant)
literalTypeVariantDef = variantsDefinition "literalTypeVariant" $
  doc "Find the literal type variant (constructor) for a given literal value" $
  match _LiteralType Nothing [
    _LiteralType_binary>>:  constant $ Mantle.literalVariantBinary,
    _LiteralType_boolean>>: constant $ Mantle.literalVariantBoolean,
    _LiteralType_float>>:   constant $ Mantle.literalVariantFloat,
    _LiteralType_integer>>: constant $ Mantle.literalVariantInteger,
    _LiteralType_string>>:  constant $ Mantle.literalVariantString]

literalVariantDef :: TElement (Literal -> LiteralVariant)
literalVariantDef = variantsDefinition "literalVariant" $
  doc "Find the literal variant (constructor) for a given literal value" $
  ref literalTypeVariantDef <.> ref literalTypeDef

literalVariantsDef :: TElement [LiteralVariant]
literalVariantsDef = variantsDefinition "literalVariants" $
  doc "All literal variants, in a canonical order" $
  list $ unitVariant _LiteralVariant <$> [
    _LiteralVariant_binary,
    _LiteralVariant_boolean,
    _LiteralVariant_float,
    _LiteralVariant_integer,
    _LiteralVariant_string]

termVariantDef :: TElement (Term -> TermVariant)
termVariantDef = variantsDefinition "termVariant" $
  doc "Find the term variant (constructor) for a given term" $
  match _Term Nothing [
    _Term_annotated>>: constant Mantle.termVariantAnnotated,
    _Term_application>>: constant Mantle.termVariantApplication,
    _Term_function>>: constant Mantle.termVariantFunction,
    _Term_let>>: constant Mantle.termVariantLet,
    _Term_list>>: constant Mantle.termVariantList,
    _Term_literal>>: constant Mantle.termVariantLiteral,
    _Term_map>>: constant Mantle.termVariantMap,
    _Term_optional>>: constant Mantle.termVariantOptional,
    _Term_product>>: constant Mantle.termVariantProduct,
    _Term_record>>: constant Mantle.termVariantRecord,
    _Term_set>>: constant Mantle.termVariantSet,
    _Term_sum>>: constant Mantle.termVariantSum,
    _Term_typeAbstraction>>: constant Mantle.termVariantTypeAbstraction,
    _Term_typeApplication>>: constant Mantle.termVariantTypeApplication,
    _Term_union>>: constant Mantle.termVariantUnion,
    _Term_variable>>: constant Mantle.termVariantVariable,
    _Term_wrap>>: constant Mantle.termVariantWrap]

termVariantsDef :: TElement [TermVariant]
termVariantsDef = variantsDefinition "termVariants" $
  doc "All term (expression) variants, in a canonical order" $
  list $ unitVariant _TermVariant <$> [
    _TermVariant_annotated,
    _TermVariant_application,
    _TermVariant_literal,
    _TermVariant_function,
    _TermVariant_list,
    _TermVariant_map,
    _TermVariant_optional,
    _TermVariant_product,
    _TermVariant_record,
    _TermVariant_set,
    _TermVariant_sum,
    _TermVariant_typeAbstraction,
    _TermVariant_typeApplication,
    _TermVariant_union,
    _TermVariant_variable,
    _TermVariant_wrap]

typeVariantDef :: TElement (Type -> TypeVariant)
typeVariantDef = variantsDefinition "typeVariant" $
  doc "Find the type variant (constructor) for a given type" $
  match _Type Nothing [
    _Type_annotated>>: constant Mantle.typeVariantAnnotated,
    _Type_application>>: constant Mantle.typeVariantApplication,
    _Type_function>>: constant Mantle.typeVariantFunction,
    _Type_forall>>: constant Mantle.typeVariantForall,
    _Type_list>>: constant Mantle.typeVariantList,
    _Type_literal>>: constant Mantle.typeVariantLiteral,
    _Type_map>>: constant Mantle.typeVariantMap,
    _Type_optional>>: constant Mantle.typeVariantOptional,
    _Type_product>>: constant Mantle.typeVariantProduct,
    _Type_record>>: constant Mantle.typeVariantRecord,
    _Type_set>>: constant Mantle.typeVariantSet,
    _Type_sum>>: constant Mantle.typeVariantSum,
    _Type_union>>: constant Mantle.typeVariantUnion,
    _Type_variable>>: constant Mantle.typeVariantVariable,
    _Type_wrap>>: constant Mantle.typeVariantWrap]

typeVariantsDef :: TElement [TypeVariant]
typeVariantsDef = variantsDefinition "typeVariants" $
  doc "All type variants, in a canonical order" $
  list $ unitVariant _TypeVariant <$> [
    _TypeVariant_annotated,
    _TypeVariant_application,
    _TypeVariant_function,
    _TypeVariant_forall,
    _TypeVariant_list,
    _TypeVariant_literal,
    _TypeVariant_map,
    _TypeVariant_wrap,
    _TypeVariant_optional,
    _TypeVariant_product,
    _TypeVariant_record,
    _TypeVariant_set,
    _TypeVariant_sum,
    _TypeVariant_union,
    _TypeVariant_variable]

-- Additional definitions; consider moving out of hydra.variants  

fieldMapDef :: TElement ([Field] -> M.Map Name Term)
fieldMapDef = variantsDefinition "fieldMap" $
  lets [
    "toPair">: lambda "f" $ pair (Core.fieldName $ var "f") (Core.fieldTerm $ var "f")]
    $ lambda "fields" $ Maps.fromList $ Lists.map (var "toPair") (var "fields")

fieldTypeMapDef :: TElement ([FieldType] -> M.Map Name Type)
fieldTypeMapDef = variantsDefinition "fieldTypeMap" $
  lets [
    "toPair">: lambda "f" $ pair (Core.fieldTypeName $ var "f") (Core.fieldTypeType $ var "f")]
    $ lambda "fields" $ Maps.fromList $ Lists.map (var "toPair") (var "fields")
