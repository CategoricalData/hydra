{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.CoreDecoding where

-- Standard Tier-2 imports
import qualified Hydra.Dsl.Compute         as Compute
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Chars       as Chars
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import           Hydra.Dsl.Phantoms        as Phantoms
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Mantle          as Mantle
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.TTerms          as TTerms
import qualified Hydra.Dsl.TTypes          as TTypes
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Topology        as Topology
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

import Hydra.Sources.Libraries
import qualified Hydra.Sources.Tier2.Errors as Errors
import qualified Hydra.Sources.Tier2.Expect as Expect
import Hydra.Sources.Tier2.Flows
import Hydra.Sources.Tier2.Lexical
import Hydra.Sources.Tier2.Rewriting


coreDecodingDefinition :: String -> TTerm a -> TElement a
coreDecodingDefinition = definitionInModule hydraCoreDecodingModule

hydraCoreDecodingModule :: Module
hydraCoreDecodingModule = Module (Namespace "hydra.coreDecoding") elements
    [Errors.hydraErrorsModule, Expect.hydraExpectModule, hydraFlowsModule, hydraLexicalModule, hydraRewritingModule]
    [hydraCodersModule, hydraMantleModule] $
    Just ("Decoding of encoded types (as terms) back to types according to LambdaGraph's epsilon encoding.")
  where
   elements = [
     el coreDecodeApplicationTypeDef,
     el coreDecodeFieldTypeDef,
     el coreDecodeFieldTypesDef,
     el coreDecodeFloatTypeDef,
     el coreDecodeForallTypeDef,
     el coreDecodeFunctionTypeDef,
     el coreDecodeIntegerTypeDef,
     el coreDecodeLiteralTypeDef,
     el coreDecodeMapTypeDef,
     el coreDecodeNameDef,
     el coreDecodeRowTypeDef,
     el coreDecodeStringDef,
     el coreDecodeTypeDef,
     el coreDecodeTypeSchemeDef,
     el coreDecodeWrappedTypeDef,
     el getFieldDef,
     el matchEnumDef,
     el matchRecordDef,
     el matchUnionDef,
     el matchUnitFieldDef]

-- Helper functions

getFieldDef :: TElement (M.Map Name Term -> Name -> (Term -> Flow Graph b) -> Flow Graph b)
getFieldDef = coreDecodingDefinition "getField" $
  lambdas ["m", "fname", "decode"] $
    Optionals.maybe
      (Flows.fail $ "expected field " ++ (Core.unName $ var "fname") ++ " not found")
      (var "decode")
      (Maps.lookup (var "fname") (var "m"))

matchUnitFieldDef :: TElement (Name -> y -> (Name, x -> Flow Graph y))
matchUnitFieldDef = coreDecodingDefinition "matchUnitField" $
  lambdas ["fname", "x"] $ pair (var "fname") (lambda "ignored" $ Flows.pure $ var "x")

matchEnumDef :: TElement (Name -> [(Name, b)] -> Term -> Flow Graph b)
matchEnumDef = coreDecodingDefinition "matchEnum" $
  lambdas ["tname", "pairs"] $
    ref matchUnionDef @@ var "tname" @@ (Lists.map (lambda "pair" $
      ref matchUnitFieldDef @@ (first $ var "pair") @@ (second $ var "pair")) $ var "pairs")

matchRecordDef :: TElement ((M.Map Name Term -> Flow Graph b) -> Term -> Flow Graph b)
matchRecordDef = coreDecodingDefinition "matchRecord" $
  lambdas ["decode", "term"] $ lets [
    "stripped">: ref fullyStripTermDef @@ var "term"]
    $ cases _Term (var "stripped")
        (Just $ ref Errors.unexpectedDef @@ string "record" @@ (Io.showTerm $ var "term")) [
      _Term_record>>: lambda "record" $ var "decode" @@
        (Maps.fromList $ Lists.map
          (lambda "field" $ pair (Core.fieldName $ var "field") (Core.fieldTerm $ var "field"))
          (Core.recordFields $ var "record"))]

matchUnionDef :: TElement (Name -> [(Name, Term -> Flow Graph b)] -> Term -> Flow Graph b)
matchUnionDef = coreDecodingDefinition "matchUnion" $
  lambdas ["tname", "pairs", "term"] $ lets [
    "stripped">: ref fullyStripTermDef @@ var "term",
    "mapping">: Maps.fromList $ var "pairs"]
    $ cases _Term (var "stripped")
        (Just $ ref Errors.unexpectedDef @@
          ("union with one of {" ++ (Strings.intercalate ", " $ Lists.map (lambda "pair" $ Core.unName $ first $ var "pair") $ var "pairs") ++ "}") @@
          (Io.showTerm $ var "stripped")) [
      _Term_variable>>: lambda "name" $
        Flows.bind (ref requireElementDef @@ var "name") $
        lambda "el" $ ref matchUnionDef @@ var "tname" @@ var "pairs" @@ (Graph.elementTerm $ var "el"),
      _Term_union>>: lambda "injection" $
        Logic.ifElse (Core.equalName_ (Core.injectionTypeName $ var "injection") (var "tname"))
          (lets [
            "fname">: Core.fieldName $ Core.injectionField $ var "injection",
            "val">: Core.fieldTerm $ Core.injectionField $ var "injection"]
            $ Optionals.maybe
              (Flows.fail $ "no matching case for field " ++ (Core.unName $ var "fname"))
              (lambda "f" $ var "f" @@ var "val")
              (Maps.lookup (var "fname") (var "mapping")))
          (ref Errors.unexpectedDef @@ ("injection for type " ++ (Core.unName $ var "tname")) @@ (Io.showTerm $ var "term"))]

-- Simple decoding functions

coreDecodeNameDef :: TElement (Term -> Flow Graph Name)
coreDecodeNameDef = coreDecodingDefinition "coreDecodeName" $
  lambda "term" $ Flows.map (unaryFunction Core.name') $
    Flows.bind (ref Expect.wrapDef @@ Core.name _Name @@ var "term") $
    ref Expect.stringDef

coreDecodeStringDef :: TElement (Term -> Flow Graph String)
coreDecodeStringDef = coreDecodingDefinition "coreDecodeString" $
  lambda "term" $ ref Expect.stringDef @@ (ref fullyStripTermDef @@ var "term")

-- Enum decoding functions

coreDecodeFloatTypeDef :: TElement (Term -> Flow Graph FloatType)
coreDecodeFloatTypeDef = coreDecodingDefinition "coreDecodeFloatType" $
  ref matchEnumDef @@ Core.name _FloatType @@ list [
    pair (Core.name _FloatType_bigfloat) Core.floatTypeBigfloat,
    pair (Core.name _FloatType_float32) Core.floatTypeFloat32,
    pair (Core.name _FloatType_float64) Core.floatTypeFloat64]

coreDecodeIntegerTypeDef :: TElement (Term -> Flow Graph IntegerType)
coreDecodeIntegerTypeDef = coreDecodingDefinition "coreDecodeIntegerType" $
  ref matchEnumDef @@ Core.name _IntegerType @@ list [
    pair (Core.name _IntegerType_bigint) Core.integerTypeBigint,
    pair (Core.name _IntegerType_int8) Core.integerTypeInt8,
    pair (Core.name _IntegerType_int16) Core.integerTypeInt16,
    pair (Core.name _IntegerType_int32) Core.integerTypeInt32,
    pair (Core.name _IntegerType_int64) Core.integerTypeInt64,
    pair (Core.name _IntegerType_uint8) Core.integerTypeUint8,
    pair (Core.name _IntegerType_uint16) Core.integerTypeUint16,
    pair (Core.name _IntegerType_uint32) Core.integerTypeUint32,
    pair (Core.name _IntegerType_uint64) Core.integerTypeUint64]

-- Record decoding functions using applicative style

coreDecodeApplicationTypeDef :: TElement (Term -> Flow Graph ApplicationType)
coreDecodeApplicationTypeDef = coreDecodingDefinition "coreDecodeApplicationType" $
  ref matchRecordDef @@ (lambda "m" $
    ref map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.name _ApplicationType_function @@ ref coreDecodeTypeDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.name _ApplicationType_argument @@ ref coreDecodeTypeDef)
      @@ (lambdas ["function", "argument"] $ Core.applicationType (var "function") (var "argument")))

coreDecodeFieldTypeDef :: TElement (Term -> Flow Graph FieldType)
coreDecodeFieldTypeDef = coreDecodingDefinition "coreDecodeFieldType" $
  ref matchRecordDef @@ (lambda "m" $
    ref map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.name _FieldType_name @@ ref coreDecodeNameDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.name _FieldType_type @@ ref coreDecodeTypeDef)
      @@ (lambdas ["name", "typ"] $ Core.fieldType (var "name") (var "typ")))

coreDecodeForallTypeDef :: TElement (Term -> Flow Graph ForallType)
coreDecodeForallTypeDef = coreDecodingDefinition "coreDecodeForallType" $
  ref matchRecordDef @@ (lambda "m" $
    ref map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.name _ForallType_parameter @@ ref coreDecodeNameDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.name _ForallType_body @@ ref coreDecodeTypeDef)
      @@ (lambdas ["parameter", "body"] $ Core.forallType (var "parameter") (var "body")))

coreDecodeFunctionTypeDef :: TElement (Term -> Flow Graph FunctionType)
coreDecodeFunctionTypeDef = coreDecodingDefinition "coreDecodeFunctionType" $
  ref matchRecordDef @@ (lambda "m" $
    ref map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.name _FunctionType_domain @@ ref coreDecodeTypeDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.name _FunctionType_codomain @@ ref coreDecodeTypeDef)
      @@ (lambdas ["domain", "codomain"] $ Core.functionType (var "domain") (var "codomain")))

coreDecodeMapTypeDef :: TElement (Term -> Flow Graph MapType)
coreDecodeMapTypeDef = coreDecodingDefinition "coreDecodeMapType" $
  ref matchRecordDef @@ (lambda "m" $
    ref map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.name _MapType_keys @@ ref coreDecodeTypeDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.name _MapType_values @@ ref coreDecodeTypeDef)
      @@ (lambdas ["keys", "values"] $ Core.mapType (var "keys") (var "values")))

coreDecodeRowTypeDef :: TElement (Term -> Flow Graph RowType)
coreDecodeRowTypeDef = coreDecodingDefinition "coreDecodeRowType" $
  ref matchRecordDef @@ (lambda "m" $
    ref map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.name _RowType_typeName @@ ref coreDecodeNameDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.name _RowType_fields @@ ref coreDecodeFieldTypesDef)
      @@ (lambdas ["typeName", "fields"] $ Core.rowType (var "typeName") (var "fields")))

coreDecodeWrappedTypeDef :: TElement (Term -> Flow Graph WrappedType)
coreDecodeWrappedTypeDef = coreDecodingDefinition "coreDecodeWrappedType" $
  lambda "term" $
    Flows.bind (ref Expect.recordDef @@ Core.name _WrappedType @@ var "term") $
      lambda "fields" $ ref map2Def
        @@ (ref Expect.fieldDef @@ Core.name _WrappedType_typeName @@ ref coreDecodeNameDef @@ var "fields")
        @@ (ref Expect.fieldDef @@ Core.name _WrappedType_object @@ ref coreDecodeTypeDef @@ var "fields")
        @@ (lambdas ["name", "obj"] $ Core.wrappedType (var "name") (var "obj"))

-- List and complex union decoding

coreDecodeFieldTypesDef :: TElement (Term -> Flow Graph [FieldType])
coreDecodeFieldTypesDef = coreDecodingDefinition "coreDecodeFieldTypes" $
  lambda "term" $ lets [
    "stripped">: ref fullyStripTermDef @@ var "term"]
    $ cases _Term (var "stripped")
        (Just $ ref Errors.unexpectedDef @@ string "list" @@ (Io.showTerm $ var "term")) [
      _Term_list>>: lambda "els" $ Flows.mapList (ref coreDecodeFieldTypeDef) (var "els")]

coreDecodeLiteralTypeDef :: TElement (Term -> Flow Graph LiteralType)
coreDecodeLiteralTypeDef = coreDecodingDefinition "coreDecodeLiteralType" $
  ref matchUnionDef @@ Core.name _LiteralType @@ list [
    ref matchUnitFieldDef @@ Core.name _LiteralType_binary @@ Core.literalTypeBinary,
    ref matchUnitFieldDef @@ Core.name _LiteralType_boolean @@ Core.literalTypeBoolean,
    pair
     (Core.name _LiteralType_float)
     (lambda "ft" $ Flows.map (unaryFunction Core.literalTypeFloat) (ref coreDecodeFloatTypeDef @@ var "ft")),
    pair
      (Core.name _LiteralType_integer)
      (lambda "it" $ Flows.map (unaryFunction Core.literalTypeInteger) (ref coreDecodeIntegerTypeDef @@ var "it")),
    ref matchUnitFieldDef @@ Core.name _LiteralType_string @@ Core.literalTypeString]

coreDecodeTypeDef :: TElement (Term -> Flow Graph Type)
coreDecodeTypeDef = coreDecodingDefinition "coreDecodeType" $
  lambda "dat" $ cases _Term (var "dat")
    (Just $ ref matchUnionDef @@ Core.name _Type @@ list [
      pair
        (Core.name _Type_application)
        (lambda "at" $ Flows.map (unaryFunction Core.typeApplication) $ ref coreDecodeApplicationTypeDef @@ var "at"),
      pair
        (Core.name _Type_forall)
        (lambda "ft" $ Flows.map (unaryFunction Core.typeForall) $ ref coreDecodeForallTypeDef @@ var "ft"),
      pair
        (Core.name _Type_function)
        (lambda "ft" $ Flows.map (unaryFunction Core.typeFunction) $ ref coreDecodeFunctionTypeDef @@ var "ft"),
      pair
        (Core.name _Type_list)
        (lambda "et" $ Flows.map (unaryFunction Core.typeList) $ ref coreDecodeTypeDef @@ var "et"),
      pair
        (Core.name _Type_literal)
        (lambda "lt" $ Flows.map (unaryFunction Core.typeLiteral) $ ref coreDecodeLiteralTypeDef @@ var "lt"),
      pair
        (Core.name _Type_map)
        (lambda "mt" $ Flows.map (unaryFunction Core.typeMap) $ ref coreDecodeMapTypeDef @@ var "mt"),
      pair
        (Core.name _Type_optional)
        (lambda "et" $ Flows.map (unaryFunction Core.typeOptional) $ ref coreDecodeTypeDef @@ var "et"),
      pair
        (Core.name _Type_product)
        (lambda "types" $ Flows.map (unaryFunction Core.typeProduct) $ ref Expect.listDef @@ ref coreDecodeTypeDef @@ var "types"),
      pair
        (Core.name _Type_record)
        (lambda "rt" $ Flows.map (unaryFunction Core.typeRecord) $ ref coreDecodeRowTypeDef @@ var "rt"),
      pair
        (Core.name _Type_set)
        (lambda "et" $ Flows.map (unaryFunction Core.typeSet) $ ref coreDecodeTypeDef @@ var "et"),
      pair
        (Core.name _Type_sum)
        (lambda "types" $ Flows.map (unaryFunction Core.typeSum) $ ref Expect.listDef @@ ref coreDecodeTypeDef @@ var "types"),
      pair
        (Core.name _Type_union)
        (lambda "rt" $ Flows.map (unaryFunction Core.typeUnion) $ ref coreDecodeRowTypeDef @@ var "rt"),
      pair
        (Core.name _Type_variable)
        (lambda "n" $ Flows.map (unaryFunction Core.typeVariable) $ ref coreDecodeNameDef @@ var "n"),
      pair
        (Core.name _Type_wrap)
        (lambda "wt" $ Flows.map (unaryFunction Core.typeWrap) $ ref coreDecodeWrappedTypeDef @@ var "wt")] @@ var "dat") [
    _Term_annotated>>: lambda "annotatedTerm" $
      Flows.map
        (lambda "t" $ Core.typeAnnotated $ Core.annotatedType (var "t") (Core.annotatedTermAnnotation $ var "annotatedTerm"))
        (ref coreDecodeTypeDef @@ (Core.annotatedTermSubject $ var "annotatedTerm")),
    _Term_typed>>: lambda "typedTerm" $
      ref coreDecodeTypeDef @@ (Core.typedTermTerm $ var "typedTerm")]

coreDecodeTypeSchemeDef :: TElement (Term -> Flow Graph TypeScheme)
coreDecodeTypeSchemeDef = coreDecodingDefinition "coreDecodeTypeScheme" $
  ref matchRecordDef @@ (lambda "m" $
    ref map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.name _TypeScheme_variables @@ (ref Expect.listDef @@ ref coreDecodeNameDef))
      @@ (ref getFieldDef @@ var "m" @@ Core.name _TypeScheme_type @@ ref coreDecodeTypeDef)
      @@ (lambdas ["vars", "body"] $ Core.typeScheme (var "vars") (var "body")))
