
module Hydra.Sources.Kernel.Terms.Decode.Core where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (fieldTypes, literalType)
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

import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.decode.core"

module_ :: Module
module_ = Module ns elements
    [ExtractCore.ns, Lexical.ns, Monads.ns, Rewriting.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Decode hydra.core types from the hydra.core.Term type")
  where
   elements = [
     toBinding annotatedType,
     toBinding applicationType,
     toBinding eitherType,
     toBinding fieldType,
     toBinding fieldTypes,
     toBinding floatType,
     toBinding forallType,
     toBinding functionType,
     toBinding integerType,
     toBinding literalType,
     toBinding mapType,
     toBinding name,
     toBinding pairType,
     toBinding rowType,
     toBinding string_,
     toBinding type_,
     toBinding typeScheme,
     toBinding wrappedType]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

annotatedType :: TBinding (Term -> Flow Graph AnnotatedType)
annotatedType = define "annotatedType" $
    doc "Decode an annotated type from a term" $
    Lexical.matchRecord @@ (lambda "m" $ binds [
      "body">: Lexical.getField @@ var "m" @@ Core.nameLift _AnnotatedType_body @@ type_,
      "annotation">: Lexical.getField @@ var "m" @@ Core.nameLift _AnnotatedType_annotation @@ decodeAnnotationMap] $
      produce $ Core.annotatedType (var "body") (var "annotation"))
  where
    decodeAnnotationMap = lambda "mapTerm" $
      cases _Term (var "mapTerm")
        (Just $ Monads.unexpected @@ string "map" @@ (ShowCore.term @@ var "mapTerm")) [
          _Term_map>>: lambda "m" $
            Flows.map (unaryFunction Maps.fromList) $
              Flows.mapList
                (lambda "entry" $
                  Flows.bind (name @@ (Pairs.first $ var "entry")) $
                    lambda "k" $
                      Flows.map
                        (lambda "v" $ pair (var "k") (var "v"))
                        (decodeTerm @@ (Pairs.second $ var "entry")))
                (Maps.toList $ var "m")]

    -- Minimal term decoder - handles literal and variable terms (sufficient for annotation maps)
    decodeTerm = lambda "t" $
      Lexical.matchUnion @@ Core.nameLift _Term @@ list [
        pair (Core.nameLift _Term_literal)
          (lambda "lit" $ Flows.map (unaryFunction Core.termLiteral) $ decodeLiteral @@ var "lit"),
        pair (Core.nameLift _Term_variable)
          (lambda "n" $ Flows.map (unaryFunction Core.termVariable) $ name @@ var "n")]
        @@ var "t"

    -- Complete literal decoder (ignoring binary)
    decodeLiteral = lambda "lit" $
      Lexical.matchUnion @@ Core.nameLift _Literal @@ list [
        pair (Core.nameLift _Literal_boolean)
          (lambda "b" $ Flows.map (unaryFunction Core.literalBoolean) $ ExtractCore.boolean @@ var "b"),
        pair (Core.nameLift _Literal_float)
          (lambda "fv" $ Flows.map (unaryFunction Core.literalFloat) $ decodeFloatValue @@ var "fv"),
        pair (Core.nameLift _Literal_integer)
          (lambda "iv" $ Flows.map (unaryFunction Core.literalInteger) $ decodeIntegerValue @@ var "iv"),
        pair (Core.nameLift _Literal_string)
          (lambda "s" $ Flows.map (unaryFunction Core.literalString) $ ExtractCore.string @@ var "s")]
        @@ var "lit"

    decodeFloatValue = lambda "fv" $
      Lexical.matchUnion @@ Core.nameLift _FloatValue @@ list [
        pair (Core.nameLift _FloatValue_bigfloat)
          (lambda "v" $ Flows.map (unaryFunction Core.floatValueBigfloat) $ ExtractCore.bigfloat @@ var "v"),
        pair (Core.nameLift _FloatValue_float32)
          (lambda "v" $ Flows.map (unaryFunction Core.floatValueFloat32) $ ExtractCore.float32 @@ var "v"),
        pair (Core.nameLift _FloatValue_float64)
          (lambda "v" $ Flows.map (unaryFunction Core.floatValueFloat64) $ ExtractCore.float64 @@ var "v")]
        @@ var "fv"

    decodeIntegerValue = lambda "iv" $
      Lexical.matchUnion @@ Core.nameLift _IntegerValue @@ list [
        pair (Core.nameLift _IntegerValue_bigint)
          (lambda "v" $ Flows.map (unaryFunction Core.integerValueBigint) $ ExtractCore.bigint @@ var "v"),
        pair (Core.nameLift _IntegerValue_int8)
          (lambda "v" $ Flows.map (unaryFunction Core.integerValueInt8) $ ExtractCore.int8 @@ var "v"),
        pair (Core.nameLift _IntegerValue_int16)
          (lambda "v" $ Flows.map (unaryFunction Core.integerValueInt16) $ ExtractCore.int16 @@ var "v"),
        pair (Core.nameLift _IntegerValue_int32)
          (lambda "v" $ Flows.map (unaryFunction Core.integerValueInt32) $ ExtractCore.int32 @@ var "v"),
        pair (Core.nameLift _IntegerValue_int64)
          (lambda "v" $ Flows.map (unaryFunction Core.integerValueInt64) $ ExtractCore.int64 @@ var "v"),
        pair (Core.nameLift _IntegerValue_uint8)
          (lambda "v" $ Flows.map (unaryFunction Core.integerValueUint8) $ ExtractCore.uint8 @@ var "v"),
        pair (Core.nameLift _IntegerValue_uint16)
          (lambda "v" $ Flows.map (unaryFunction Core.integerValueUint16) $ ExtractCore.uint16 @@ var "v"),
        pair (Core.nameLift _IntegerValue_uint32)
          (lambda "v" $ Flows.map (unaryFunction Core.integerValueUint32) $ ExtractCore.uint32 @@ var "v"),
        pair (Core.nameLift _IntegerValue_uint64)
          (lambda "v" $ Flows.map (unaryFunction Core.integerValueUint64) $ ExtractCore.uint64 @@ var "v")]
        @@ var "iv"

applicationType :: TBinding (Term -> Flow Graph ApplicationType)
applicationType = define "applicationType" $
  doc "Decode an application type from a term" $
  Lexical.matchRecord @@ (lambda "m" $ binds [
    "function">: Lexical.getField @@ var "m" @@ Core.nameLift _ApplicationType_function @@ type_,
    "argument">: Lexical.getField @@ var "m" @@ Core.nameLift _ApplicationType_argument @@ type_] $
    produce $ Core.applicationType (var "function") (var "argument"))

eitherType :: TBinding (Term -> Flow Graph EitherType)
eitherType = define "eitherType" $
  doc "Decode an either type from a term" $
  Lexical.matchRecord @@ (lambda "m" $ binds [
    "left">: Lexical.getField @@ var "m" @@ Core.nameLift _EitherType_left @@ type_,
    "right">: Lexical.getField @@ var "m" @@ Core.nameLift _EitherType_right @@ type_] $
    produce $ Core.eitherType (var "left") (var "right"))

fieldType :: TBinding (Term -> Flow Graph FieldType)
fieldType = define "fieldType" $
  doc "Decode a field type from a term" $
  Lexical.matchRecord @@ (lambda "m" $ binds [
    "name">: Lexical.getField @@ var "m" @@ Core.nameLift _FieldType_name @@ name,
    "typ">: Lexical.getField @@ var "m" @@ Core.nameLift _FieldType_type @@ type_] $
    produce $ Core.fieldType (var "name") (var "typ"))

fieldTypes :: TBinding (Term -> Flow Graph [FieldType])
fieldTypes = define "fieldTypes" $
  doc "Decode a list of field types from a term" $
  lambda "term" $ lets [
    "stripped">: Rewriting.deannotateAndDetypeTerm @@ var "term"]
    $ cases _Term (var "stripped")
        (Just $ Monads.unexpected @@ string "list" @@ (ShowCore.term @@ var "term")) [
      _Term_list>>: lambda "els" $ Flows.mapList (fieldType) (var "els")]

floatType :: TBinding (Term -> Flow Graph FloatType)
floatType = define "floatType" $
  doc "Decode a floating-point type from a term" $
  "term0" ~>
  trace (string "dbg 1") $
  Lexical.matchEnum @@ Core.nameLift _FloatType @@ list [
    pair (Core.nameLift _FloatType_bigfloat) Core.floatTypeBigfloat,
    pair (Core.nameLift _FloatType_float32) Core.floatTypeFloat32,
    pair (Core.nameLift _FloatType_float64) Core.floatTypeFloat64] @@ var "term0"

forallType :: TBinding (Term -> Flow Graph ForallType)
forallType = define "forallType" $
  doc "Decode a forall type from a term" $
  Lexical.matchRecord @@ (lambda "m" $ binds [
    "parameter">: Lexical.getField @@ var "m" @@ Core.nameLift _ForallType_parameter @@ name,
    "body">: Lexical.getField @@ var "m" @@ Core.nameLift _ForallType_body @@ type_] $
    produce $ Core.forallType (var "parameter") (var "body"))

functionType :: TBinding (Term -> Flow Graph FunctionType)
functionType = define "functionType" $
  doc "Decode a function type from a term" $
  Lexical.matchRecord @@ (lambda "m" $ binds [
    "domain">: Lexical.getField @@ var "m" @@ Core.nameLift _FunctionType_domain @@ type_,
    "codomain">: Lexical.getField @@ var "m" @@ Core.nameLift _FunctionType_codomain @@ type_] $
    produce $ Core.functionType (var "domain") (var "codomain"))

integerType :: TBinding (Term -> Flow Graph IntegerType)
integerType = define "integerType" $
  doc "Decode an integer type from a term" $
  "term0" ~>
  trace (string "dbg 1") $
  Lexical.matchEnum @@ Core.nameLift _IntegerType @@ list [
    pair (Core.nameLift _IntegerType_bigint) Core.integerTypeBigint,
    pair (Core.nameLift _IntegerType_int8) Core.integerTypeInt8,
    pair (Core.nameLift _IntegerType_int16) Core.integerTypeInt16,
    pair (Core.nameLift _IntegerType_int32) Core.integerTypeInt32,
    pair (Core.nameLift _IntegerType_int64) Core.integerTypeInt64,
    pair (Core.nameLift _IntegerType_uint8) Core.integerTypeUint8,
    pair (Core.nameLift _IntegerType_uint16) Core.integerTypeUint16,
    pair (Core.nameLift _IntegerType_uint32) Core.integerTypeUint32,
    pair (Core.nameLift _IntegerType_uint64) Core.integerTypeUint64] @@ var "term0"

literalType :: TBinding (Term -> Flow Graph LiteralType)
literalType = define "literalType" $
  doc "Decode a literal type from a term" $
  "term0" ~>
  trace (string "dbg 3") $
  Lexical.matchUnion @@ Core.nameLift _LiteralType @@ list [
    Lexical.matchUnitField @@ Core.nameLift _LiteralType_binary @@ Core.literalTypeBinary,
    Lexical.matchUnitField @@ Core.nameLift _LiteralType_boolean @@ Core.literalTypeBoolean,
    pair
     (Core.nameLift _LiteralType_float)
     (lambda "ft" $ Flows.map (unaryFunction Core.literalTypeFloat) (floatType @@ var "ft")),
    pair
      (Core.nameLift _LiteralType_integer)
      (lambda "it" $ Flows.map (unaryFunction Core.literalTypeInteger) (integerType @@ var "it")),
    Lexical.matchUnitField @@ Core.nameLift _LiteralType_string @@ Core.literalTypeString] @@ var "term0"

mapType :: TBinding (Term -> Flow Graph MapType)
mapType = define "mapType" $
  doc "Decode a map type from a term" $
  Lexical.matchRecord @@ (lambda "m" $ binds [
   "keys">: Lexical.getField @@ var "m" @@ Core.nameLift _MapType_keys @@ type_,
   "values">: Lexical.getField @@ var "m" @@ Core.nameLift _MapType_values @@ type_] $
    produce $ Core.mapType (var "keys") (var "values"))

name :: TBinding (Term -> Flow Graph Name)
name = define "name" $
  doc "Decode a name from a term" $
  lambda "term" $ Flows.map (unaryFunction Core.name) $
    Flows.bind (ExtractCore.wrap @@ Core.nameLift _Name @@ var "term") $
    ExtractCore.string

pairType :: TBinding (Term -> Flow Graph PairType)
pairType = define "pairType" $
  doc "Decode a pair type from a term" $
  Lexical.matchRecord @@ (lambda "m" $ binds [
   "first">: Lexical.getField @@ var "m" @@ Core.nameLift _PairType_first @@ type_,
   "second">: Lexical.getField @@ var "m" @@ Core.nameLift _PairType_second @@ type_] $
   produce $ Core.pairType (var "first") (var "second"))

rowType :: TBinding (Term -> Flow Graph RowType)
rowType = define "rowType" $
  doc "Decode a row type from a term" $
  Lexical.matchRecord @@ (lambda "m" $ binds [
   "typeName">: Lexical.getField @@ var "m" @@ Core.nameLift _RowType_typeName @@ name,
   "fields">: Lexical.getField @@ var "m" @@ Core.nameLift _RowType_fields @@ fieldTypes] $
   produce $ Core.rowType (var "typeName") (var "fields"))

string_ :: TBinding (Term -> Flow Graph String)
string_ = define "string" $
  doc "Decode a string from a term" $
  lambda "term" $ ExtractCore.string @@ (Rewriting.deannotateAndDetypeTerm @@ var "term")

type_ :: TBinding (Term -> Flow Graph Type)
type_ = define "type" $
  doc "Decode a type from a term" $
  lambda "dat" $
  cases _Term (var "dat")
    (Just $ trace (string "dbg 4") $ Lexical.matchUnion @@ Core.nameLift _Type @@ list [
      pair
        (Core.nameLift _Type_annotated)
        (lambda "at" $ Flows.map (unaryFunction Core.typeAnnotated) $ annotatedType @@ var "at"),
      pair
        (Core.nameLift _Type_application)
        (lambda "at" $ Flows.map (unaryFunction Core.typeApplication) $ applicationType @@ var "at"),
      pair
        (Core.nameLift _Type_either)
        (lambda "et" $ Flows.map (unaryFunction Core.typeEither) $ eitherType @@ var "et"),
      pair
        (Core.nameLift _Type_forall)
        (lambda "ft" $ Flows.map (unaryFunction Core.typeForall) $ forallType @@ var "ft"),
      pair
        (Core.nameLift _Type_function)
        (lambda "ft" $ Flows.map (unaryFunction Core.typeFunction) $ functionType @@ var "ft"),
      pair
        (Core.nameLift _Type_list)
        (lambda "et" $ Flows.map (unaryFunction Core.typeList) $ type_ @@ var "et"),
      pair
        (Core.nameLift _Type_literal)
        (lambda "lt" $ Flows.map (unaryFunction Core.typeLiteral) $ literalType @@ var "lt"),
      pair
        (Core.nameLift _Type_map)
        (lambda "mt" $ Flows.map (unaryFunction Core.typeMap) $ mapType @@ var "mt"),
      pair
        (Core.nameLift _Type_maybe)
        (lambda "et" $ Flows.map (unaryFunction Core.typeMaybe) $ type_ @@ var "et"),
      pair
        (Core.nameLift _Type_pair)
        (lambda "pt" $ Flows.map (unaryFunction Core.typePair) $ pairType @@ var "pt"),
      pair
        (Core.nameLift _Type_record)
        (lambda "rt" $ Flows.map (unaryFunction Core.typeRecord) $ rowType @@ var "rt"),
      pair
        (Core.nameLift _Type_set)
        (lambda "et" $ Flows.map (unaryFunction Core.typeSet) $ type_ @@ var "et"),
      pair
        (Core.nameLift _Type_union)
        (lambda "rt" $ Flows.map (unaryFunction Core.typeUnion) $ rowType @@ var "rt"),
      pair
        (Core.nameLift _Type_unit)
        (constant $ Flows.pure Core.typeUnit),
      pair
        (Core.nameLift _Type_variable)
        (lambda "n" $ Flows.map (unaryFunction Core.typeVariable) $ name @@ var "n"),
      pair
        (Core.nameLift _Type_wrap)
        (lambda "wt" $ Flows.map (unaryFunction Core.typeWrap) $ wrappedType @@ var "wt")] @@ var "dat") [
    _Term_annotated>>: lambda "annotatedTerm" $
      Flows.map
        (lambda "t" $ Core.typeAnnotated $ Core.annotatedType (var "t") (Core.annotatedTermAnnotation $ var "annotatedTerm"))
        (type_ @@ (Core.annotatedTermBody $ var "annotatedTerm"))]

typeVariableMetadata :: TBinding (Term -> Flow Graph TypeVariableMetadata)
typeVariableMetadata = define "typeVariableMetadata" $
  doc "Decode type variable metadata from a term" $
  Lexical.matchRecord @@ (lambda "m" $ binds [
    "classes">: Lexical.getField @@ var "m" @@ Core.nameLift _TypeVariableMetadata_classes @@ (ExtractCore.setOf @@ name)] $
    produce $ Core.typeVariableMetadata (var "classes"))

typeScheme :: TBinding (Term -> Flow Graph TypeScheme)
typeScheme = define "typeScheme" $
  doc "Decode a type scheme from a term" $
  Lexical.matchRecord @@ (lambda "m" $ binds [
    "vars">: Lexical.getField @@ var "m" @@ Core.nameLift _TypeScheme_variables @@ (ExtractCore.listOf @@ name),
    "body">: Lexical.getField @@ var "m" @@ Core.nameLift _TypeScheme_type @@ type_] $
    -- Note: constraints field is optional; for now we default to Nothing during decoding
    produce $ Core.typeScheme (var "vars") (var "body") Phantoms.nothing)

wrappedType :: TBinding (Term -> Flow Graph WrappedType)
wrappedType = define "wrappedType" $
  doc "Decode a wrapped type from a term" $
  lambda "term" $ binds [
    "fields">: ExtractCore.record @@ Core.nameLift _WrappedType @@ var "term",
    "name">: ExtractCore.field @@ Core.nameLift _WrappedType_typeName @@ name @@ var "fields",
    "obj">: ExtractCore.field @@ Core.nameLift _WrappedType_body @@ type_ @@ var "fields"] $
    produce $ Core.wrappedType (var "name") (var "obj")
