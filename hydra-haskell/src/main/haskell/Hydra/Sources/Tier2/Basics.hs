{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Basics where

-- Standard Tier-2 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All


basicsDefinition :: String -> TTerm a -> TElement a
basicsDefinition = definitionInModule hydraBasicsModule

hydraBasicsModule :: Module
hydraBasicsModule = Module (Namespace "hydra/basics") elements
    [hydraTier1Module]
    tier0Modules $
    Just "A tier-2 module of basic functions for working with types and terms."
  where
   elements = [
     el eliminationVariantDef,
     el eliminationVariantsDef,
     el floatTypePrecisionDef,
     el floatTypesDef,
     el floatValueTypeDef,
     el functionVariantDef,
     el functionVariantsDef,
     el idDef,
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
     -- Formatting.hs
     el capitalizeDef,
     el decapitalizeDef,
     el mapFirstLetterDef,
     -- Common.hs
     el fieldMapDef,
     el fieldTypeMapDef,
     el isEncodedTypeDef,
     el isTypeDef,
     el isUnitTermDef,
     el isUnitTypeDef,
     el elementsToGraphDef,
     el localNameOfEagerDef,
     el localNameOfLazyDef,
     el namespaceOfEagerDef,
     el namespaceOfLazyDef,
     el namespaceToFilePathDef,
     el qualifyNameEagerDef,
     el qualifyNameLazyDef
     ]

eliminationVariantDef :: TElement (Elimination -> EliminationVariant)
eliminationVariantDef = basicsDefinition "eliminationVariant" $
  doc "Find the elimination variant (constructor) for a given elimination term" $
  function eliminationT eliminationVariantT $
  matchToEnum _Elimination _EliminationVariant Nothing [
    _Elimination_list     @-> _EliminationVariant_list,
    _Elimination_optional @-> _EliminationVariant_optional,
    _Elimination_product  @-> _EliminationVariant_product,
    _Elimination_record   @-> _EliminationVariant_record,
    _Elimination_union    @-> _EliminationVariant_union,
    _Elimination_wrap     @-> _EliminationVariant_wrap]

eliminationVariantsDef :: TElement [EliminationVariant]
eliminationVariantsDef = basicsDefinition "eliminationVariants" $
  doc "All elimination variants (constructors), in a canonical order" $
  typed (listT eliminationVariantT) $
  list $ unitVariant _EliminationVariant <$> [
    _EliminationVariant_list,
    _EliminationVariant_wrap,
    _EliminationVariant_optional,
    _EliminationVariant_product,
    _EliminationVariant_record,
    _EliminationVariant_union]

floatTypePrecisionDef :: TElement (FloatType -> Precision)
floatTypePrecisionDef = basicsDefinition "floatTypePrecision" $
  doc "Find the precision of a given floating-point type" $
  function floatTypeT precisionT $
  matchToUnion _FloatType _Precision Nothing [
    _FloatType_bigfloat @-> field _Precision_arbitrary unit,
    _FloatType_float32  @-> field _Precision_bits $ int 32,
    _FloatType_float64  @-> field _Precision_bits $ int 64]

floatTypesDef :: TElement [FloatType]
floatTypesDef = basicsDefinition "floatTypes" $
  doc "All floating-point types in a canonical order" $
  typed (listT floatTypeT) $
  list $ unitVariant _FloatType <$> [
    _FloatType_bigfloat,
    _FloatType_float32,
    _FloatType_float64]

floatValueTypeDef :: TElement (FloatValue -> FloatType)
floatValueTypeDef = basicsDefinition "floatValueType" $
  doc "Find the float type for a given floating-point value" $
  function floatValueT floatTypeT $
  matchToEnum _FloatValue _FloatType Nothing [
    _FloatValue_bigfloat @-> _FloatType_bigfloat,
    _FloatValue_float32  @-> _FloatType_float32,
    _FloatValue_float64  @-> _FloatType_float64]

functionVariantDef :: TElement (Function -> FunctionVariant)
functionVariantDef = basicsDefinition "functionVariant" $
  doc "Find the function variant (constructor) for a given function" $
  function functionT functionVariantT $
  matchToEnum _Function _FunctionVariant Nothing [
    _Function_elimination @-> _FunctionVariant_elimination,
    _Function_lambda      @-> _FunctionVariant_lambda,
    _Function_primitive   @-> _FunctionVariant_primitive]

functionVariantsDef :: TElement [FunctionVariant]
functionVariantsDef = basicsDefinition "functionVariants" $
  doc "All function variants (constructors), in a canonical order" $
  typed (listT functionVariantT) $
  list $ unitVariant _FunctionVariant <$> [
    _FunctionVariant_elimination,
    _FunctionVariant_lambda,
    _FunctionVariant_primitive]

idDef :: TElement (a -> a)
idDef = basicsDefinition "id" $
  doc "The identity function" $
  function aT aT $
  lambda "x" $ var "x"

integerTypeIsSignedDef :: TElement (IntegerType -> Bool)
integerTypeIsSignedDef = basicsDefinition "integerTypeIsSigned" $
  doc "Find whether a given integer type is signed (true) or unsigned (false)" $
  function integerTypeT booleanT $
  matchData _IntegerType Nothing [
    _IntegerType_bigint @-> constant true,
    _IntegerType_int8   @-> constant true,
    _IntegerType_int16  @-> constant true,
    _IntegerType_int32  @-> constant true,
    _IntegerType_int64  @-> constant true,
    _IntegerType_uint8  @-> constant false,
    _IntegerType_uint16 @-> constant false,
    _IntegerType_uint32 @-> constant false,
    _IntegerType_uint64 @-> constant false]

integerTypePrecisionDef :: TElement (IntegerType -> Precision)
integerTypePrecisionDef = basicsDefinition "integerTypePrecision" $
  doc "Find the precision of a given integer type" $
  function integerTypeT precisionT $
  matchToUnion _IntegerType _Precision Nothing [
    _IntegerType_bigint @-> field _Precision_arbitrary unit,
    _IntegerType_int8   @-> field _Precision_bits $ int 8,
    _IntegerType_int16  @-> field _Precision_bits $ int 16,
    _IntegerType_int32  @-> field _Precision_bits $ int 32,
    _IntegerType_int64  @-> field _Precision_bits $ int 64,
    _IntegerType_uint8  @-> field _Precision_bits $ int 8,
    _IntegerType_uint16 @-> field _Precision_bits $ int 16,
    _IntegerType_uint32 @-> field _Precision_bits $ int 32,
    _IntegerType_uint64 @-> field _Precision_bits $ int 64]

integerTypesDef :: TElement [IntegerType]
integerTypesDef = basicsDefinition "integerTypes" $
  doc "All integer types, in a canonical order" $
  typed (listT integerTypeT) $
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
integerValueTypeDef = basicsDefinition "integerValueType" $
  doc "Find the integer type for a given integer value" $
  function integerValueT integerTypeT $
  matchToEnum _IntegerValue _IntegerType Nothing [
    _IntegerValue_bigint @-> _IntegerType_bigint,
    _IntegerValue_int8   @-> _IntegerType_int8,
    _IntegerValue_int16  @-> _IntegerType_int16,
    _IntegerValue_int32  @-> _IntegerType_int32,
    _IntegerValue_int64  @-> _IntegerType_int64,
    _IntegerValue_uint8  @-> _IntegerType_uint8,
    _IntegerValue_uint16 @-> _IntegerType_uint16,
    _IntegerValue_uint32 @-> _IntegerType_uint32,
    _IntegerValue_uint64 @-> _IntegerType_uint64]

literalTypeDef :: TElement (Literal -> LiteralType)
literalTypeDef = basicsDefinition "literalType" $
  doc "Find the literal type for a given literal value" $
  function literalT literalTypeT $
  match _Literal Nothing [
    TCase _Literal_binary  --> constant $ variant _LiteralType _LiteralType_binary unit,
    TCase _Literal_boolean --> constant $ variant _LiteralType _LiteralType_boolean unit,
    TCase _Literal_float   --> inject2 _LiteralType _LiteralType_float <.> ref floatValueTypeDef,
    TCase _Literal_integer --> inject2 _LiteralType _LiteralType_integer <.> ref integerValueTypeDef,
    TCase _Literal_string  --> constant $ variant _LiteralType _LiteralType_string unit]

literalTypeVariantDef :: TElement (LiteralType -> LiteralVariant)
literalTypeVariantDef = basicsDefinition "literalTypeVariant" $
  doc "Find the literal type variant (constructor) for a given literal value" $
  function literalTypeT literalVariantT $
  matchToEnum _LiteralType _LiteralVariant Nothing [
    _LiteralType_binary  @-> _LiteralVariant_binary,
    _LiteralType_boolean @-> _LiteralVariant_boolean,
    _LiteralType_float   @-> _LiteralVariant_float,
    _LiteralType_integer @-> _LiteralVariant_integer,
    _LiteralType_string  @-> _LiteralVariant_string]

literalVariantDef :: TElement (Literal -> LiteralVariant)
literalVariantDef = basicsDefinition "literalVariant" $
  doc "Find the literal variant (constructor) for a given literal value" $
  function literalT literalVariantT $
  ref literalTypeVariantDef <.> ref literalTypeDef

literalVariantsDef :: TElement [LiteralVariant]
literalVariantsDef = basicsDefinition "literalVariants" $
  doc "All literal variants, in a canonical order" $
  typed (listT literalVariantT) $
  list $ unitVariant _LiteralVariant <$> [
    _LiteralVariant_binary,
    _LiteralVariant_boolean,
    _LiteralVariant_float,
    _LiteralVariant_integer,
    _LiteralVariant_string]

termVariantDef :: TElement (Term -> TermVariant)
termVariantDef = basicsDefinition "termVariant" $
  doc "Find the term variant (constructor) for a given term" $
  function termT termVariantT $
  matchToEnum _Term _TermVariant Nothing [
    _Term_annotated   @-> _TermVariant_annotated,
    _Term_application @-> _TermVariant_application,
    _Term_function    @-> _TermVariant_function,
    _Term_let         @-> _TermVariant_let,
    _Term_list        @-> _TermVariant_list,
    _Term_literal     @-> _TermVariant_literal,
    _Term_map         @-> _TermVariant_map,
    _Term_optional    @-> _TermVariant_optional,
    _Term_product     @-> _TermVariant_product,
    _Term_record      @-> _TermVariant_record,
    _Term_set         @-> _TermVariant_set,
    _Term_sum         @-> _TermVariant_sum,
    _Term_typed       @-> _TermVariant_typed,
    _Term_union       @-> _TermVariant_union,
    _Term_variable    @-> _TermVariant_variable,
    _Term_wrap        @-> _TermVariant_wrap]

termVariantsDef :: TElement [TermVariant]
termVariantsDef = basicsDefinition "termVariants" $
  doc "All term (expression) variants, in a canonical order" $
  typed (listT termVariantT) $
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
    _TermVariant_typed,
    _TermVariant_union,
    _TermVariant_variable,
    _TermVariant_wrap]

typeVariantDef :: TElement (Type -> TypeVariant)
typeVariantDef = basicsDefinition "typeVariant" $
  doc "Find the type variant (constructor) for a given type" $
  function typeT typeVariantT $
  matchToEnum _Type _TypeVariant Nothing [
    _Type_annotated   @-> _TypeVariant_annotated,
    _Type_application @-> _TypeVariant_application,
    _Type_function    @-> _TypeVariant_function,
    _Type_lambda      @-> _TypeVariant_lambda,
    _Type_list        @-> _TypeVariant_list,
    _Type_literal     @-> _TypeVariant_literal,
    _Type_map         @-> _TypeVariant_map,
    _Type_optional    @-> _TypeVariant_optional,
    _Type_product     @-> _TypeVariant_product,
    _Type_record      @-> _TypeVariant_record,
    _Type_set         @-> _TypeVariant_set,
    _Type_sum         @-> _TypeVariant_sum,
    _Type_union       @-> _TypeVariant_union,
    _Type_variable    @-> _TypeVariant_variable,
    _Type_wrap        @-> _TypeVariant_wrap]

typeVariantsDef :: TElement [TypeVariant]
typeVariantsDef = basicsDefinition "typeVariants" $
  doc "All type variants, in a canonical order" $
  typed (listT typeVariantT) $
  list $ unitVariant _TypeVariant <$> [
    _TypeVariant_annotated,
    _TypeVariant_application,
    _TypeVariant_function,
    _TypeVariant_lambda,
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

-- Formatting.hs

capitalizeDef :: TElement (String -> String)
capitalizeDef = basicsDefinition "capitalize" $
  doc "Capitalize the first letter of a string" $
  function stringT stringT $
  ref mapFirstLetterDef @@ Strings.toUpper

decapitalizeDef :: TElement (String -> String)
decapitalizeDef = basicsDefinition "decapitalize" $
  doc "Decapitalize the first letter of a string" $
  function stringT stringT $
  ref mapFirstLetterDef @@ Strings.toLower

-- TODO: simplify this helper
mapFirstLetterDef :: TElement ((String -> String) -> String -> String)
mapFirstLetterDef = basicsDefinition "mapFirstLetter" $
  doc "A helper which maps the first letter of a string to another string" $
  function (funT stringT stringT) (funT stringT stringT) $
  lambda "mapping" $ lambda "s" ((Logic.ifElse
       @@ var "s"
       @@ (Strings.cat2 @@ var "firstLetter" @@ (Strings.fromList @@ (Lists.tail @@ var "list")))
       @@ (Strings.isEmpty @@ var "s"))
    `with` [
      "firstLetter">: var "mapping" @@ (Strings.fromList @@ (Lists.pure @@ (Lists.head @@ var "list"))),
      "list">: typed (listT int32T) $ Strings.toList @@ var "s"])

-- Common.hs

fieldMapDef :: TElement ([Field] -> M.Map Name Term)
fieldMapDef = basicsDefinition "fieldMap" $
  function (TypeList fieldT) (mapT fieldNameT termT) $
  (lambda "fields" $ Maps.fromList @@ (Lists.map @@ var "toPair" @@ var "fields"))
    `with` [
      "toPair">: lambda "f" $ pair (project _Field _Field_name @@ var "f") (project _Field _Field_term @@ var "f")]

fieldTypeMapDef :: TElement ([FieldType] -> M.Map Name Type)
fieldTypeMapDef = basicsDefinition "fieldTypeMap" $
  function (TypeList fieldTypeT) (mapT fieldNameT typeT) $
    (lambda "fields" $ Maps.fromList @@ (Lists.map @@ var "toPair" @@ var "fields"))
  `with` [
    "toPair">: lambda "f" $ pair (project _FieldType _FieldType_name @@ var "f") (project _FieldType _FieldType_type @@ var "f")]

isEncodedTypeDef :: TElement (Term -> Bool)
isEncodedTypeDef = basicsDefinition "isEncodedType" $
  function termT booleanT $
  lambda "t" $ (match _Term (Just false) [
      TCase _Term_application --> lambda "a" $
        ref isEncodedTypeDef @@ (project _Application _Application_function @@ var "a"),
      TCase _Term_union       --> lambda "i" $
        Equality.equalString @@ (string $ unName _Type) @@ (unwrap _Name @@ (project _Injection _Injection_typeName @@ var "i"))
    ]) @@ (ref stripTermDef @@ var "t")

isTypeDef :: TElement (Type -> Bool)
isTypeDef = basicsDefinition "isType" $
  function typeT booleanT $
  lambda "t" $ (match _Type (Just false) [
      TCase _Type_application --> lambda "a" $
        ref isTypeDef @@ (project _ApplicationType _ApplicationType_function @@ var "a"),
      TCase _Type_lambda --> lambda "l" $
        ref isTypeDef @@ (project _LambdaType _LambdaType_body @@ var "l"),
      TCase _Type_union --> lambda "rt" $
        Equality.equalString @@ (string $ unName _Type) @@ (unwrap _Name @@ (project _RowType _RowType_typeName @@ var "rt"))
--      TCase _Type_variable --> constant true
    ]) @@ (ref stripTypeDef @@ var "t")

isUnitTermDef :: TElement (Term -> Bool)
isUnitTermDef = basicsDefinition "isUnitTerm" $
  function termT booleanT $
  lambda "t" $ Equality.equalTerm @@ (ref fullyStripTermDef @@ var "t") @@ TTerm (coreEncodeTerm Terms.unit)

isUnitTypeDef :: TElement (Term -> Bool)
isUnitTypeDef = basicsDefinition "isUnitType" $
  function typeT booleanT $
  lambda "t" $ Equality.equalType @@ (ref stripTypeDef @@ var "t") @@ TTerm (coreEncodeType unitT)

elementsToGraphDef :: TElement (Graph -> Maybe Graph -> [Element] -> Graph)
elementsToGraphDef = basicsDefinition "elementsToGraph" $
  function graphT (funT (optionalT graphT) (funT (TypeList elementT) graphT)) $
  lambda "parent" $ lambda "schema" $ lambda "elements" $
    Graph.graph
      (Maps.fromList @@ (Lists.map @@ var "toPair" @@ var "elements"))
      (Graph.graphEnvironment @@ var "parent")
      (Graph.graphTypes @@ var "parent")
      (Graph.graphBody @@ var "parent")
      (Graph.graphPrimitives @@ var "parent")
      (var "schema")
  `with` [
    "toPair" >: lambda "el" $ pair (project _Element _Element_name @@ var "el") (var "el")]

localNameOfEagerDef :: TElement (Name -> String)
localNameOfEagerDef = basicsDefinition "localNameOfEager" $
  function nameT stringT $
  Module.qualifiedNameLocal <.> ref qualifyNameEagerDef

localNameOfLazyDef :: TElement (Name -> String)
localNameOfLazyDef = basicsDefinition "localNameOfLazy" $
  function nameT stringT $
  Module.qualifiedNameLocal <.> ref qualifyNameLazyDef

namespaceOfEagerDef :: TElement (Name -> Maybe Namespace)
namespaceOfEagerDef = basicsDefinition "namespaceOfEager" $
  function nameT (optionalT namespaceT) $
  Module.qualifiedNameNamespace <.> ref qualifyNameEagerDef

namespaceOfLazyDef :: TElement (Name -> Maybe Namespace)
namespaceOfLazyDef = basicsDefinition "namespaceOfLazy" $
  function nameT (optionalT namespaceT) $
  Module.qualifiedNameNamespace <.> ref qualifyNameLazyDef

namespaceToFilePathDef :: TElement (Bool -> FileExtension -> Namespace -> String)
namespaceToFilePathDef = basicsDefinition "namespaceToFilePath" $
  function booleanT (funT fileExtensionT (funT namespaceT stringT)) $
  lambda "caps" $ lambda "ext" $ lambda "ns" $
    (((Strings.intercalate @@ "/" @@ var "parts") ++ "." ++ (unwrap _FileExtension @@ var "ext"))
    `with` [
      "parts">: Lists.map @@ (Logic.ifElse @@ ref capitalizeDef @@ ref idDef @@ var "caps") @@ (Strings.splitOn @@ "/" @@ (unwrap _Namespace @@ var "ns"))])

qualifyNameEagerDef :: TElement (Name -> QualifiedName)
qualifyNameEagerDef = basicsDefinition "qualifyNameEager" $
  function nameT qualifiedNameT $
  lambda "name" $ ((Logic.ifElse
      @@ Module.qualifiedName nothing (unwrap _Name @@ var "name")
      @@ Module.qualifiedName
        (just $ wrap _Namespace (Lists.head @@ var "parts"))
        (Strings.intercalate @@ "." @@ (Lists.tail @@ var "parts"))
      @@ (Equality.equalInt32 @@ int32 1 @@ (Lists.length @@ var "parts")))
    `with` [
      "parts">: Strings.splitOn @@ "." @@ (unwrap _Name @@ var "name")])

qualifyNameLazyDef :: TElement (Name -> QualifiedName)
qualifyNameLazyDef = basicsDefinition "qualifyNameLazy" $
  function nameT qualifiedNameT $
  lambda "name" $ (Logic.ifElse
      @@ Module.qualifiedName nothing (unwrap _Name @@ var "name")
      @@ Module.qualifiedName
        (just $ wrap _Namespace (Strings.intercalate @@ "." @@ (Lists.reverse @@ (Lists.tail @@ var "parts"))))
        (Lists.head @@ var "parts")
      @@ (Equality.equalInt32 @@ int32 1 @@ (Lists.length @@ var "parts")))
    `with` [
      "parts">: Lists.reverse @@ (Strings.splitOn @@ "." @@ (unwrap _Name @@ var "name"))]
