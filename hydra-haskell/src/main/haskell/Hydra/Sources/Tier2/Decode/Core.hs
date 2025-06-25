{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Decode.Core where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
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
--import qualified Hydra.Sources.Tier2.Accessors as Accessors
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.CoreLanguage as CoreLanguage
import qualified Hydra.Sources.Tier2.Errors as Errors
import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
import qualified Hydra.Sources.Tier2.Flows as Flows_
--import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
--import qualified Hydra.Sources.Tier2.Inference as Inference
import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.TermAdapters as TermAdapters
--import qualified Hydra.Sources.Tier2.TermEncoding as TermEncoding
--import qualified Hydra.Sources.Tier2.Unification as Unification
--import qualified Hydra.Sources.Tier2.Variants as Variants


coreDecodingDefinition :: String -> TTerm a -> TElement a
coreDecodingDefinition = definitionInModule decodeCoreModule

decodeCoreModule :: Module
decodeCoreModule = Module (Namespace "hydra.decode.core") elements
    [Errors.hydraErrorsModule, ExtractCore.extractCoreModule, Flows_.hydraFlowsModule, Lexical.hydraLexicalModule,
      Rewriting.hydraRewritingModule, ShowCore.showCoreModule]
    [Tier1.hydraCodersModule, Tier1.hydraMantleModule] $
    Just ("Decoding of encoded types (as terms) back to types according to LambdaGraph's epsilon encoding.")
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
     el wrappedTypeDef,
     -- TODO: move these
     el getFieldDef,
     el matchEnumDef,
     el matchRecordDef,
     el matchUnionDef,
     el matchUnitFieldDef]

applicationTypeDef :: TElement (Term -> Flow Graph ApplicationType)
applicationTypeDef = coreDecodingDefinition "applicationType" $
  ref matchRecordDef @@ (lambda "m" $
    ref Flows_.map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _ApplicationType_function @@ ref typeDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _ApplicationType_argument @@ ref typeDef)
      @@ (lambdas ["function", "argument"] $ Core.applicationType (var "function") (var "argument")))

fieldTypeDef :: TElement (Term -> Flow Graph FieldType)
fieldTypeDef = coreDecodingDefinition "fieldType" $
  ref matchRecordDef @@ (lambda "m" $
    ref Flows_.map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _FieldType_name @@ ref nameDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _FieldType_type @@ ref typeDef)
      @@ (lambdas ["name", "typ"] $ Core.fieldType (var "name") (var "typ")))

fieldTypesDef :: TElement (Term -> Flow Graph [FieldType])
fieldTypesDef = coreDecodingDefinition "fieldTypes" $
  lambda "term" $ lets [
    "stripped">: ref Strip.fullyStripTermDef @@ var "term"]
    $ cases _Term (var "stripped")
        (Just $ ref Errors.unexpectedDef @@ string "list" @@ (ref ShowCore.showTermDef @@ var "term")) [
      _Term_list>>: lambda "els" $ Flows.mapList (ref fieldTypeDef) (var "els")]

floatTypeDef :: TElement (Term -> Flow Graph FloatType)
floatTypeDef = coreDecodingDefinition "floatType" $
  ref matchEnumDef @@ Core.nameLift _FloatType @@ list [
    pair (Core.nameLift _FloatType_bigfloat) Core.floatTypeBigfloat,
    pair (Core.nameLift _FloatType_float32) Core.floatTypeFloat32,
    pair (Core.nameLift _FloatType_float64) Core.floatTypeFloat64]

forallTypeDef :: TElement (Term -> Flow Graph ForallType)
forallTypeDef = coreDecodingDefinition "forallType" $
  ref matchRecordDef @@ (lambda "m" $
    ref Flows_.map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _ForallType_parameter @@ ref nameDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _ForallType_body @@ ref typeDef)
      @@ (lambdas ["parameter", "body"] $ Core.forallType (var "parameter") (var "body")))

functionTypeDef :: TElement (Term -> Flow Graph FunctionType)
functionTypeDef = coreDecodingDefinition "functionType" $
  ref matchRecordDef @@ (lambda "m" $
    ref Flows_.map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _FunctionType_domain @@ ref typeDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _FunctionType_codomain @@ ref typeDef)
      @@ (lambdas ["domain", "codomain"] $ Core.functionType (var "domain") (var "codomain")))

integerTypeDef :: TElement (Term -> Flow Graph IntegerType)
integerTypeDef = coreDecodingDefinition "integerType" $
  ref matchEnumDef @@ Core.nameLift _IntegerType @@ list [
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
  ref matchUnionDef @@ Core.nameLift _LiteralType @@ list [
    ref matchUnitFieldDef @@ Core.nameLift _LiteralType_binary @@ Core.literalTypeBinary,
    ref matchUnitFieldDef @@ Core.nameLift _LiteralType_boolean @@ Core.literalTypeBoolean,
    pair
     (Core.nameLift _LiteralType_float)
     (lambda "ft" $ Flows.map (unaryFunction Core.literalTypeFloat) (ref floatTypeDef @@ var "ft")),
    pair
      (Core.nameLift _LiteralType_integer)
      (lambda "it" $ Flows.map (unaryFunction Core.literalTypeInteger) (ref integerTypeDef @@ var "it")),
    ref matchUnitFieldDef @@ Core.nameLift _LiteralType_string @@ Core.literalTypeString]

mapTypeDef :: TElement (Term -> Flow Graph MapType)
mapTypeDef = coreDecodingDefinition "mapType" $
  ref matchRecordDef @@ (lambda "m" $
    ref Flows_.map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _MapType_keys @@ ref typeDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _MapType_values @@ ref typeDef)
      @@ (lambdas ["keys", "values"] $ Core.mapType (var "keys") (var "values")))

nameDef :: TElement (Term -> Flow Graph Name)
nameDef = coreDecodingDefinition "name" $
  lambda "term" $ Flows.map (unaryFunction Core.name) $
    Flows.bind (ref ExtractCore.wrapDef @@ Core.nameLift _Name @@ var "term") $
    ref ExtractCore.stringDef

rowTypeDef :: TElement (Term -> Flow Graph RowType)
rowTypeDef = coreDecodingDefinition "rowType" $
  ref matchRecordDef @@ (lambda "m" $
    ref Flows_.map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _RowType_typeName @@ ref nameDef)
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _RowType_fields @@ ref fieldTypesDef)
      @@ (lambdas ["typeName", "fields"] $ Core.rowType (var "typeName") (var "fields")))

stringDef :: TElement (Term -> Flow Graph String)
stringDef = coreDecodingDefinition "string" $
  lambda "term" $ ref ExtractCore.stringDef @@ (ref Strip.fullyStripTermDef @@ var "term")

typeDef :: TElement (Term -> Flow Graph Type)
typeDef = coreDecodingDefinition "type" $
  lambda "dat" $ cases _Term (var "dat")
    (Just $ ref matchUnionDef @@ Core.nameLift _Type @@ list [
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
        (Core.nameLift _Type_variable)
        (lambda "n" $ Flows.map (unaryFunction Core.typeVariable) $ ref nameDef @@ var "n"),
      pair
        (Core.nameLift _Type_wrap)
        (lambda "wt" $ Flows.map (unaryFunction Core.typeWrap) $ ref wrappedTypeDef @@ var "wt")] @@ var "dat") [
    _Term_annotated>>: lambda "annotatedTerm" $
      Flows.map
        (lambda "t" $ Core.typeAnnotated $ Core.annotatedType (var "t") (Core.annotatedTermAnnotation $ var "annotatedTerm"))
        (ref typeDef @@ (Core.annotatedTermSubject $ var "annotatedTerm")),
    _Term_typed>>: lambda "typedTerm" $
      ref typeDef @@ (Core.typedTermTerm $ var "typedTerm")]

typeSchemeDef :: TElement (Term -> Flow Graph TypeScheme)
typeSchemeDef = coreDecodingDefinition "typeScheme" $
  ref matchRecordDef @@ (lambda "m" $
    ref Flows_.map2Def
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _TypeScheme_variables @@ (ref ExtractCore.listDef @@ ref nameDef))
      @@ (ref getFieldDef @@ var "m" @@ Core.nameLift _TypeScheme_type @@ ref typeDef)
      @@ (lambdas ["vars", "body"] $ Core.typeScheme (var "vars") (var "body")))

wrappedTypeDef :: TElement (Term -> Flow Graph WrappedType)
wrappedTypeDef = coreDecodingDefinition "wrappedType" $
  lambda "term" $
    Flows.bind (ref ExtractCore.recordDef @@ Core.nameLift _WrappedType @@ var "term") $
      lambda "fields" $ ref Flows_.map2Def
        @@ (ref ExtractCore.fieldDef @@ Core.nameLift _WrappedType_typeName @@ ref nameDef @@ var "fields")
        @@ (ref ExtractCore.fieldDef @@ Core.nameLift _WrappedType_object @@ ref typeDef @@ var "fields")
        @@ (lambdas ["name", "obj"] $ Core.wrappedType (var "name") (var "obj"))

-- TODO: move these

getFieldDef :: TElement (M.Map Name Term -> Name -> (Term -> Flow Graph b) -> Flow Graph b)
getFieldDef = coreDecodingDefinition "getField" $
  lambdas ["m", "fname", "decode"] $
    Optionals.maybe
      (Flows.fail $ "expected field " ++ (Core.unName $ var "fname") ++ " not found")
      (var "decode")
      (Maps.lookup (var "fname") (var "m"))

matchEnumDef :: TElement (Name -> [(Name, b)] -> Term -> Flow Graph b)
matchEnumDef = coreDecodingDefinition "matchEnum" $
  lambdas ["tname", "pairs"] $
    ref matchUnionDef @@ var "tname" @@ (Lists.map (lambda "pair" $
      ref matchUnitFieldDef @@ (first $ var "pair") @@ (second $ var "pair")) $ var "pairs")

matchRecordDef :: TElement ((M.Map Name Term -> Flow Graph b) -> Term -> Flow Graph b)
matchRecordDef = coreDecodingDefinition "matchRecord" $
  lambdas ["decode", "term"] $ lets [
    "stripped">: ref Strip.fullyStripTermDef @@ var "term"]
    $ cases _Term (var "stripped")
        (Just $ ref Errors.unexpectedDef @@ string "record" @@ (ref ShowCore.showTermDef @@ var "term")) [
      _Term_record>>: lambda "record" $ var "decode" @@
        (Maps.fromList $ Lists.map
          (lambda "field" $ pair (Core.fieldName $ var "field") (Core.fieldTerm $ var "field"))
          (Core.recordFields $ var "record"))]

matchUnionDef :: TElement (Name -> [(Name, Term -> Flow Graph b)] -> Term -> Flow Graph b)
matchUnionDef = coreDecodingDefinition "matchUnion" $
  lambdas ["tname", "pairs", "term"] $ lets [
    "stripped">: ref Strip.fullyStripTermDef @@ var "term",
    "mapping">: Maps.fromList $ var "pairs"]
    $ cases _Term (var "stripped")
        (Just $ ref Errors.unexpectedDef @@
          ("union with one of {" ++ (Strings.intercalate ", " $ Lists.map (lambda "pair" $ Core.unName $ first $ var "pair") $ var "pairs") ++ "}") @@
          (ref ShowCore.showTermDef @@ var "stripped")) [
      _Term_variable>>: lambda "name" $
        Flows.bind (ref Lexical.requireElementDef @@ var "name") $
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
          (ref Errors.unexpectedDef @@ ("injection for type " ++ (Core.unName $ var "tname")) @@ (ref ShowCore.showTermDef @@ var "term"))]

matchUnitFieldDef :: TElement (Name -> y -> (Name, x -> Flow Graph y))
matchUnitFieldDef = coreDecodingDefinition "matchUnitField" $
  lambdas ["fname", "x"] $ pair (var "fname") (lambda "ignored" $ Flows.pure $ var "x")
