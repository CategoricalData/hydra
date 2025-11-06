{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Decode.Core where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Maybes    as Maybes
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


module_ :: Module
module_ = Module (Namespace "hydra.decode.core") elements
    [ExtractCore.module_, Monads.module_, Lexical.module_,
      Rewriting.module_, ShowCore.module_]
    kernelTypesModules $
    Just ("Decode hydra.core types from the hydra.core.Term type")
  where
   elements = [
     el applicationTypeDef,
     el eitherTypeDef,
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

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

applicationTypeDef :: TBinding (Term -> Flow Graph ApplicationType)
applicationTypeDef = define "applicationType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $ binds [
    "function">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _ApplicationType_function @@ ref typeDef,
    "argument">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _ApplicationType_argument @@ ref typeDef] $
    produce $ Core.applicationType (var "function") (var "argument"))

eitherTypeDef :: TBinding (Term -> Flow Graph EitherType)
eitherTypeDef = define "eitherType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $ binds [
    "left">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _EitherType_left @@ ref typeDef,
    "right">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _EitherType_right @@ ref typeDef] $
    produce $ Core.eitherType (var "left") (var "right"))

fieldTypeDef :: TBinding (Term -> Flow Graph FieldType)
fieldTypeDef = define "fieldType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $ binds [
    "name">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _FieldType_name @@ ref nameDef,
    "typ">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _FieldType_type @@ ref typeDef] $
    produce $ Core.fieldType (var "name") (var "typ"))

fieldTypesDef :: TBinding (Term -> Flow Graph [FieldType])
fieldTypesDef = define "fieldTypes" $
  lambda "term" $ lets [
    "stripped">: ref Rewriting.deannotateAndDetypeTermDef @@ var "term"]
    $ cases _Term (var "stripped")
        (Just $ ref Monads.unexpectedDef @@ string "list" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_list>>: lambda "els" $ Flows.mapList (ref fieldTypeDef) (var "els")]

floatTypeDef :: TBinding (Term -> Flow Graph FloatType)
floatTypeDef = define "floatType" $
  ref Lexical.matchEnumDef @@ Core.nameLift _FloatType @@ list [
    tuple2 (Core.nameLift _FloatType_bigfloat) Core.floatTypeBigfloat,
    tuple2 (Core.nameLift _FloatType_float32) Core.floatTypeFloat32,
    tuple2 (Core.nameLift _FloatType_float64) Core.floatTypeFloat64]

forallTypeDef :: TBinding (Term -> Flow Graph ForallType)
forallTypeDef = define "forallType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $ binds [
    "parameter">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _ForallType_parameter @@ ref nameDef,
    "body">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _ForallType_body @@ ref typeDef] $
    produce $ Core.forallType (var "parameter") (var "body"))

functionTypeDef :: TBinding (Term -> Flow Graph FunctionType)
functionTypeDef = define "functionType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $ binds [
    "domain">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _FunctionType_domain @@ ref typeDef,
    "codomain">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _FunctionType_codomain @@ ref typeDef] $
    produce $ Core.functionType (var "domain") (var "codomain"))

integerTypeDef :: TBinding (Term -> Flow Graph IntegerType)
integerTypeDef = define "integerType" $
  ref Lexical.matchEnumDef @@ Core.nameLift _IntegerType @@ list [
    tuple2 (Core.nameLift _IntegerType_bigint) Core.integerTypeBigint,
    tuple2 (Core.nameLift _IntegerType_int8) Core.integerTypeInt8,
    tuple2 (Core.nameLift _IntegerType_int16) Core.integerTypeInt16,
    tuple2 (Core.nameLift _IntegerType_int32) Core.integerTypeInt32,
    tuple2 (Core.nameLift _IntegerType_int64) Core.integerTypeInt64,
    tuple2 (Core.nameLift _IntegerType_uint8) Core.integerTypeUint8,
    tuple2 (Core.nameLift _IntegerType_uint16) Core.integerTypeUint16,
    tuple2 (Core.nameLift _IntegerType_uint32) Core.integerTypeUint32,
    tuple2 (Core.nameLift _IntegerType_uint64) Core.integerTypeUint64]

literalTypeDef :: TBinding (Term -> Flow Graph LiteralType)
literalTypeDef = define "literalType" $
  ref Lexical.matchUnionDef @@ Core.nameLift _LiteralType @@ list [
    ref Lexical.matchUnitFieldDef @@ Core.nameLift _LiteralType_binary @@ Core.literalTypeBinary,
    ref Lexical.matchUnitFieldDef @@ Core.nameLift _LiteralType_boolean @@ Core.literalTypeBoolean,
    tuple2
     (Core.nameLift _LiteralType_float)
     (lambda "ft" $ Flows.map (unaryFunction Core.literalTypeFloat) (ref floatTypeDef @@ var "ft")),
    tuple2
      (Core.nameLift _LiteralType_integer)
      (lambda "it" $ Flows.map (unaryFunction Core.literalTypeInteger) (ref integerTypeDef @@ var "it")),
    ref Lexical.matchUnitFieldDef @@ Core.nameLift _LiteralType_string @@ Core.literalTypeString]

mapTypeDef :: TBinding (Term -> Flow Graph MapType)
mapTypeDef = define "mapType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $ binds [
   "keys">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _MapType_keys @@ ref typeDef,
   "values">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _MapType_values @@ ref typeDef] $
    produce $ Core.mapType (var "keys") (var "values"))

nameDef :: TBinding (Term -> Flow Graph Name)
nameDef = define "name" $
  lambda "term" $ Flows.map (unaryFunction Core.name) $
    Flows.bind (ref ExtractCore.wrapDef @@ Core.nameLift _Name @@ var "term") $
    ref ExtractCore.stringDef

rowTypeDef :: TBinding (Term -> Flow Graph RowType)
rowTypeDef = define "rowType" $
  ref Lexical.matchRecordDef @@ (lambda "m" $ binds [
   "typeName">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _RowType_typeName @@ ref nameDef,
   "fields">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _RowType_fields @@ ref fieldTypesDef] $
   produce $ Core.rowType (var "typeName") (var "fields"))

stringDef :: TBinding (Term -> Flow Graph String)
stringDef = define "string" $
  lambda "term" $ ref ExtractCore.stringDef @@ (ref Rewriting.deannotateAndDetypeTermDef @@ var "term")

typeDef :: TBinding (Term -> Flow Graph Type)
typeDef = define "type" $
  lambda "dat" $ cases _Term (var "dat")
    (Just $ ref Lexical.matchUnionDef @@ Core.nameLift _Type @@ list [
      tuple2
        (Core.nameLift _Type_application)
        (lambda "at" $ Flows.map (unaryFunction Core.typeApplication) $ ref applicationTypeDef @@ var "at"),
      tuple2
        (Core.nameLift _Type_either)
        (lambda "et" $ Flows.map (unaryFunction Core.typeEither) $ ref eitherTypeDef @@ var "et"),
      tuple2
        (Core.nameLift _Type_forall)
        (lambda "ft" $ Flows.map (unaryFunction Core.typeForall) $ ref forallTypeDef @@ var "ft"),
      tuple2
        (Core.nameLift _Type_function)
        (lambda "ft" $ Flows.map (unaryFunction Core.typeFunction) $ ref functionTypeDef @@ var "ft"),
      tuple2
        (Core.nameLift _Type_list)
        (lambda "et" $ Flows.map (unaryFunction Core.typeList) $ ref typeDef @@ var "et"),
      tuple2
        (Core.nameLift _Type_literal)
        (lambda "lt" $ Flows.map (unaryFunction Core.typeLiteral) $ ref literalTypeDef @@ var "lt"),
      tuple2
        (Core.nameLift _Type_map)
        (lambda "mt" $ Flows.map (unaryFunction Core.typeMap) $ ref mapTypeDef @@ var "mt"),
      tuple2
        (Core.nameLift _Type_maybe)
        (lambda "et" $ Flows.map (unaryFunction Core.typeMaybe) $ ref typeDef @@ var "et"),
      tuple2
        (Core.nameLift _Type_product)
        (lambda "types" $ Flows.map (unaryFunction Core.typeProduct) $ ref ExtractCore.listOfDef @@ ref typeDef @@ var "types"),
      tuple2
        (Core.nameLift _Type_record)
        (lambda "rt" $ Flows.map (unaryFunction Core.typeRecord) $ ref rowTypeDef @@ var "rt"),
      tuple2
        (Core.nameLift _Type_set)
        (lambda "et" $ Flows.map (unaryFunction Core.typeSet) $ ref typeDef @@ var "et"),
      tuple2
        (Core.nameLift _Type_sum)
        (lambda "types" $ Flows.map (unaryFunction Core.typeSum) $ ref ExtractCore.listOfDef @@ ref typeDef @@ var "types"),
      tuple2
        (Core.nameLift _Type_union)
        (lambda "rt" $ Flows.map (unaryFunction Core.typeUnion) $ ref rowTypeDef @@ var "rt"),
      tuple2
        (Core.nameLift _Type_unit)
        (constant $ Flows.pure Core.typeUnit),
      tuple2
        (Core.nameLift _Type_variable)
        (lambda "n" $ Flows.map (unaryFunction Core.typeVariable) $ ref nameDef @@ var "n"),
      tuple2
        (Core.nameLift _Type_wrap)
        (lambda "wt" $ Flows.map (unaryFunction Core.typeWrap) $ ref wrappedTypeDef @@ var "wt")] @@ var "dat") [
    _Term_annotated>>: lambda "annotatedTerm" $
      Flows.map
        (lambda "t" $ Core.typeAnnotated $ Core.annotatedType (var "t") (Core.annotatedTermAnnotation $ var "annotatedTerm"))
        (ref typeDef @@ (Core.annotatedTermBody $ var "annotatedTerm"))]

typeSchemeDef :: TBinding (Term -> Flow Graph TypeScheme)
typeSchemeDef = define "typeScheme" $
  ref Lexical.matchRecordDef @@ (lambda "m" $ binds [
    "vars">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _TypeScheme_variables @@ (ref ExtractCore.listOfDef @@ ref nameDef),
    "body">: ref Lexical.getFieldDef @@ var "m" @@ Core.nameLift _TypeScheme_type @@ ref typeDef] $
    produce $ Core.typeScheme (var "vars") (var "body"))

wrappedTypeDef :: TBinding (Term -> Flow Graph WrappedType)
wrappedTypeDef = define "wrappedType" $
  lambda "term" $ binds [
    "fields">: ref ExtractCore.recordDef @@ Core.nameLift _WrappedType @@ var "term",
    "name">: ref ExtractCore.fieldDef @@ Core.nameLift _WrappedType_typeName @@ ref nameDef @@ var "fields",
    "obj">: ref ExtractCore.fieldDef @@ Core.nameLift _WrappedType_body @@ ref typeDef @@ var "fields"] $
    produce $ Core.wrappedType (var "name") (var "obj")
