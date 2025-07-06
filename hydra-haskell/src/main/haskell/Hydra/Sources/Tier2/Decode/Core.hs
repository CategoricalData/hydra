{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Decode.Core where

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
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.Adapt.Literals as AdaptLiterals
--import qualified Hydra.Sources.Tier2.Adapt.Modules as AdaptModules
--import qualified Hydra.Sources.Tier2.Adapt.Terms as AdaptTerms
--import qualified Hydra.Sources.Tier2.Adapt.Utils as AdaptUtils
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Describe.Mantle as DescribeMantle
--import qualified Hydra.Sources.Tier2.Errors as Errors
import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
--import qualified Hydra.Sources.Tier2.Extract.Mantle as ExtractMantle
--import qualified Hydra.Sources.Tier2.Grammars as Grammars
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Languages as Languages
import qualified Hydra.Sources.Tier2.Lexical as Lexical
import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.Names as Names
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templates as Templates
--import qualified Hydra.Sources.Tier2.Unification as Unification
--import qualified Hydra.Sources.Tier2.Variants as Variants


coreDecodingDefinition :: String -> TTerm a -> TElement a
coreDecodingDefinition = definitionInModule decodeCoreModule

decodeCoreModule :: Module
decodeCoreModule = Module (Namespace "hydra.decode.core") elements
    [ExtractCore.extractCoreModule, Monads.hydraMonadsModule, Lexical.hydraLexicalModule,
      Rewriting.hydraRewritingModule, ShowCore.showCoreModule]
    [Tier1.hydraCodersModule, Tier1.hydraMantleModule] $
    Just ("Decode hydra.core types from the hydra.core.Term type")
  where
   elements = [
     el applicationTypeDef,
     el fieldTypeDef,
     el fieldTypesDef,
     el floatTypeDef,
     el forallTypeDef,
     el functionTypeDef,
     el integerTypeDef,
     el literalTypeDef,
     el mapTypeDef,
     el nameDef,
     el rowTypeDef,
     el stringDef,
     el typeDef,
     el typeSchemeDef,
     el wrappedTypeDef]

applicationTypeDef :: TElement (Term -> Flow Graph ApplicationType)
applicationTypeDef = coreDecodingDefinition "applicationType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $
    ref Monads.map2Def
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _ApplicationType_function @@ ref typeDef)
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _ApplicationType_argument @@ ref typeDef)
      @@ (lambdas ["function", "argument"] $ Core.applicationType (var "function") (var "argument")))

fieldTypeDef :: TElement (Term -> Flow Graph FieldType)
fieldTypeDef = coreDecodingDefinition "fieldType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $
    ref Monads.map2Def
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _FieldType_name @@ ref nameDef)
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _FieldType_type @@ ref typeDef)
      @@ (lambdas ["name", "typ"] $ Core.fieldType (var "name") (var "typ")))

fieldTypesDef :: TElement (Term -> Flow Graph [FieldType])
fieldTypesDef = coreDecodingDefinition "fieldTypes" $
  lambda "term" $ lets [
    "stripped">: ref Strip.fullyStripTermDef @@ var "term"]
    $ cases _Term (var "stripped")
        (Just $ ref Monads.unexpectedDef @@ string "list" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_list>>: lambda "els" $ Flows.mapList (ref fieldTypeDef) (var "els")]

floatTypeDef :: TElement (Term -> Flow Graph FloatType)
floatTypeDef = coreDecodingDefinition "floatType" $
  ref Lexical.matchEnumDef @@ Core.nameLift _FloatType @@ list [
    pair (Core.nameLift _FloatType_bigfloat) Core.floatTypeBigfloat,
    pair (Core.nameLift _FloatType_float32) Core.floatTypeFloat32,
    pair (Core.nameLift _FloatType_float64) Core.floatTypeFloat64]

forallTypeDef :: TElement (Term -> Flow Graph ForallType)
forallTypeDef = coreDecodingDefinition "forallType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $
    ref Monads.map2Def
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _ForallType_parameter @@ ref nameDef)
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _ForallType_body @@ ref typeDef)
      @@ (lambdas ["parameter", "body"] $ Core.forallType (var "parameter") (var "body")))

functionTypeDef :: TElement (Term -> Flow Graph FunctionType)
functionTypeDef = coreDecodingDefinition "functionType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $
    ref Monads.map2Def
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _FunctionType_domain @@ ref typeDef)
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _FunctionType_codomain @@ ref typeDef)
      @@ (lambdas ["domain", "codomain"] $ Core.functionType (var "domain") (var "codomain")))

integerTypeDef :: TElement (Term -> Flow Graph IntegerType)
integerTypeDef = coreDecodingDefinition "integerType" $
  ref Lexical.matchEnumDef @@ Core.nameLift _IntegerType @@ list [
    pair (Core.nameLift _IntegerType_bigint) Core.integerTypeBigint,
    pair (Core.nameLift _IntegerType_int8) Core.integerTypeInt8,
    pair (Core.nameLift _IntegerType_int16) Core.integerTypeInt16,
    pair (Core.nameLift _IntegerType_int32) Core.integerTypeInt32,
    pair (Core.nameLift _IntegerType_int64) Core.integerTypeInt64,
    pair (Core.nameLift _IntegerType_uint8) Core.integerTypeUint8,
    pair (Core.nameLift _IntegerType_uint16) Core.integerTypeUint16,
    pair (Core.nameLift _IntegerType_uint32) Core.integerTypeUint32,
    pair (Core.nameLift _IntegerType_uint64) Core.integerTypeUint64]

literalTypeDef :: TElement (Term -> Flow Graph LiteralType)
literalTypeDef = coreDecodingDefinition "literalType" $
  ref Lexical.matchUnionDef @@ Core.nameLift _LiteralType @@ list [
    ref Lexical.matchUnitFieldDef @@ Core.nameLift _LiteralType_binary @@ Core.literalTypeBinary,
    ref Lexical.matchUnitFieldDef @@ Core.nameLift _LiteralType_boolean @@ Core.literalTypeBoolean,
    pair
     (Core.nameLift _LiteralType_float)
     (lambda "ft" $ Flows.map (unaryFunction Core.literalTypeFloat) (ref floatTypeDef @@ var "ft")),
    pair
      (Core.nameLift _LiteralType_integer)
      (lambda "it" $ Flows.map (unaryFunction Core.literalTypeInteger) (ref integerTypeDef @@ var "it")),
    ref Lexical.matchUnitFieldDef @@ Core.nameLift _LiteralType_string @@ Core.literalTypeString]

mapTypeDef :: TElement (Term -> Flow Graph MapType)
mapTypeDef = coreDecodingDefinition "mapType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $
    ref Monads.map2Def
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _MapType_keys @@ ref typeDef)
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _MapType_values @@ ref typeDef)
      @@ (lambdas ["keys", "values"] $ Core.mapType (var "keys") (var "values")))

nameDef :: TElement (Term -> Flow Graph Name)
nameDef = coreDecodingDefinition "name" $
  lambda "term" $ Flows.map (unaryFunction Core.name) $
    Flows.bind (ref ExtractCore.wrapDef @@ Core.nameLift _Name @@ var "term") $
    ref ExtractCore.stringDef

rowTypeDef :: TElement (Term -> Flow Graph RowType)
rowTypeDef = coreDecodingDefinition "rowType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $
    ref Monads.map2Def
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _RowType_typeName @@ ref nameDef)
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _RowType_fields @@ ref fieldTypesDef)
      @@ (lambdas ["typeName", "fields"] $ Core.rowType (var "typeName") (var "fields")))

stringDef :: TElement (Term -> Flow Graph String)
stringDef = coreDecodingDefinition "string" $
  lambda "term" $ ref ExtractCore.stringDef @@ (ref Strip.fullyStripTermDef @@ var "term")

typeDef :: TElement (Term -> Flow Graph Type)
typeDef = coreDecodingDefinition "type" $
  lambda "dat" $ cases _Term (var "dat")
    (Just $ ref Lexical.matchUnionDef @@ Core.nameLift _Type @@ list [
      pair
        (Core.nameLift _Type_application)
        (lambda "at" $ Flows.map (unaryFunction Core.typeApplication) $ ref applicationTypeDef @@ var "at"),
      pair
        (Core.nameLift _Type_forall)
        (lambda "ft" $ Flows.map (unaryFunction Core.typeForall) $ ref forallTypeDef @@ var "ft"),
      pair
        (Core.nameLift _Type_function)
        (lambda "ft" $ Flows.map (unaryFunction Core.typeFunction) $ ref functionTypeDef @@ var "ft"),
      pair
        (Core.nameLift _Type_list)
        (lambda "et" $ Flows.map (unaryFunction Core.typeList) $ ref typeDef @@ var "et"),
      pair
        (Core.nameLift _Type_literal)
        (lambda "lt" $ Flows.map (unaryFunction Core.typeLiteral) $ ref literalTypeDef @@ var "lt"),
      pair
        (Core.nameLift _Type_map)
        (lambda "mt" $ Flows.map (unaryFunction Core.typeMap) $ ref mapTypeDef @@ var "mt"),
      pair
        (Core.nameLift _Type_optional)
        (lambda "et" $ Flows.map (unaryFunction Core.typeOptional) $ ref typeDef @@ var "et"),
      pair
        (Core.nameLift _Type_product)
        (lambda "types" $ Flows.map (unaryFunction Core.typeProduct) $ ref ExtractCore.listDef @@ ref typeDef @@ var "types"),
      pair
        (Core.nameLift _Type_record)
        (lambda "rt" $ Flows.map (unaryFunction Core.typeRecord) $ ref rowTypeDef @@ var "rt"),
      pair
        (Core.nameLift _Type_set)
        (lambda "et" $ Flows.map (unaryFunction Core.typeSet) $ ref typeDef @@ var "et"),
      pair
        (Core.nameLift _Type_sum)
        (lambda "types" $ Flows.map (unaryFunction Core.typeSum) $ ref ExtractCore.listDef @@ ref typeDef @@ var "types"),
      pair
        (Core.nameLift _Type_union)
        (lambda "rt" $ Flows.map (unaryFunction Core.typeUnion) $ ref rowTypeDef @@ var "rt"),
      pair
        (Core.nameLift _Type_unit)
        (constant $ Flows.pure Core.typeUnit),
      pair
        (Core.nameLift _Type_variable)
        (lambda "n" $ Flows.map (unaryFunction Core.typeVariable) $ ref nameDef @@ var "n"),
      pair
        (Core.nameLift _Type_wrap)
        (lambda "wt" $ Flows.map (unaryFunction Core.typeWrap) $ ref wrappedTypeDef @@ var "wt")] @@ var "dat") [
    _Term_annotated>>: lambda "annotatedTerm" $
      Flows.map
        (lambda "t" $ Core.typeAnnotated $ Core.annotatedType (var "t") (Core.annotatedTermAnnotation $ var "annotatedTerm"))
        (ref typeDef @@ (Core.annotatedTermSubject $ var "annotatedTerm"))]

typeSchemeDef :: TElement (Term -> Flow Graph TypeScheme)
typeSchemeDef = coreDecodingDefinition "typeScheme" $
  ref Lexical.matchRecordDef @@ (lambda "m" $
    ref Monads.map2Def
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _TypeScheme_variables @@ (ref ExtractCore.listDef @@ ref nameDef))
      @@ (ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _TypeScheme_type @@ ref typeDef)
      @@ (lambdas ["vars", "body"] $ Core.typeScheme (var "vars") (var "body")))

wrappedTypeDef :: TElement (Term -> Flow Graph WrappedType)
wrappedTypeDef = coreDecodingDefinition "wrappedType" $
  lambda "term" $
    Flows.bind (ref ExtractCore.recordDef @@ Core.nameLift _WrappedType @@ var "term") $
      lambda "fields" $ ref Monads.map2Def
        @@ (ref ExtractCore.fieldDef @@ Core.nameLift _WrappedType_typeName @@ ref nameDef @@ var "fields")
        @@ (ref ExtractCore.fieldDef @@ Core.nameLift _WrappedType_object @@ ref typeDef @@ var "fields")
        @@ (lambdas ["name", "obj"] $ Core.wrappedType (var "name") (var "obj"))
