module Hydra.Sources.Basics where

import Hydra.Kernel
import Hydra.Sources.Compute
import Hydra.Sources.Graph
import Hydra.Sources.Mantle
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Lib.Equality as Equality
import Hydra.Dsl.Lib.Maps as Maps
import Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import Prelude hiding ((++))
import qualified Data.Map as M
import qualified Data.Set as S


basicsDefinition :: String -> Datum a -> Definition a
basicsDefinition = definitionInModule hydraBasicsModule

hydraBasicsModule :: Module Kv
hydraBasicsModule = Module (Namespace "hydra/basics") elements [hydraGraphModule, hydraMantleModule, hydraComputeModule] $
    Just ("Basic functions for working with types and terms. "
      <> "These functions are not allowed to include references to primitive functions, as the definitions of some "
      <> "primitive functions in turn depend on them.")
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
     el termMetaDef,
     el termVariantDef,
     el termVariantsDef,
     el typeVariantDef,
     el typeVariantsDef,
     -- Common.hs
     el fieldMapDef,
     el fieldTypeMapDef,
     el ignoredVariableDef,
     el isEncodedTypeDef,
     el isTypeDef,
     el isUnitTermDef,
     el isUnitTypeDef,
     el placeholderNameDef,
     el skipAnnotationsDef,
     el stripTermDef,
     el stripTypeDef,
     el unqualifyNameDef
     ]

eqA = (M.fromList [(Name "a", S.fromList [TypeClassEquality])])
fieldA = Types.apply (TypeVariable _Field) (Types.var "a") :: Type a
fieldTypeA = Types.apply (TypeVariable _FieldType) (Types.var "a") :: Type a
termA = Types.apply (TypeVariable _Term) (Types.var "a") :: Type a
typeA = Types.apply (TypeVariable _Type) (Types.var "a") :: Type a

eliminationVariantDef :: Definition (Elimination a -> EliminationVariant)
eliminationVariantDef = basicsDefinition "eliminationVariant" $
  doc "Find the elimination variant (constructor) for a given elimination term" $
  typed (Types.function (Types.apply (TypeVariable _Elimination) (Types.var "a")) (TypeVariable _EliminationVariant)) $
  matchToEnum _Elimination _EliminationVariant Nothing [
    _Elimination_list     @-> _EliminationVariant_list,
    _Elimination_optional @-> _EliminationVariant_optional,
    _Elimination_record   @-> _EliminationVariant_record,
    _Elimination_union    @-> _EliminationVariant_union,
    _Elimination_wrap     @-> _EliminationVariant_wrap]

eliminationVariantsDef :: Definition [EliminationVariant]
eliminationVariantsDef = basicsDefinition "eliminationVariants" $
  doc "All elimination variants (constructors), in a canonical order" $
  list $ unitVariant _EliminationVariant <$> [
    _EliminationVariant_list,
    _EliminationVariant_wrap,
    _EliminationVariant_optional,
    _EliminationVariant_record,
    _EliminationVariant_union]

floatTypePrecisionDef :: Definition (FloatType -> Precision)
floatTypePrecisionDef = basicsDefinition "floatTypePrecision" $
  doc "Find the precision of a given floating-point type" $
  matchToUnion _FloatType _Precision Nothing [
    _FloatType_bigfloat @-> field _Precision_arbitrary unit,
    _FloatType_float32  @-> field _Precision_bits $ int 32,
    _FloatType_float64  @-> field _Precision_bits $ int 64]

floatTypesDef :: Definition [FloatType]
floatTypesDef = basicsDefinition "floatTypes" $
  doc "All floating-point types in a canonical order" $
  list $ unitVariant _FloatType <$> [
    _FloatType_bigfloat,
    _FloatType_float32,
    _FloatType_float64]

floatValueTypeDef :: Definition (FloatValue -> FloatType)
floatValueTypeDef = basicsDefinition "floatValueType" $
  doc "Find the float type for a given floating-point value" $
  matchToEnum _FloatValue _FloatType Nothing [
    _FloatValue_bigfloat @-> _FloatType_bigfloat,
    _FloatValue_float32  @-> _FloatType_float32,
    _FloatValue_float64  @-> _FloatType_float64]

functionVariantDef :: Definition (Function a -> FunctionVariant)
functionVariantDef = basicsDefinition "functionVariant" $
  doc "Find the function variant (constructor) for a given function" $
  typed (Types.function (Types.apply (TypeVariable _Function) (Types.var "a")) (TypeVariable _FunctionVariant)) $
  matchToEnum _Function _FunctionVariant Nothing [
    _Function_elimination @-> _FunctionVariant_elimination,
    _Function_lambda      @-> _FunctionVariant_lambda,
    _Function_primitive   @-> _FunctionVariant_primitive]

functionVariantsDef :: Definition [FunctionVariant]
functionVariantsDef = basicsDefinition "functionVariants" $
  doc "All function variants (constructors), in a canonical order" $
  list $ unitVariant _FunctionVariant <$> [
    _FunctionVariant_elimination,
    _FunctionVariant_lambda,
    _FunctionVariant_primitive]

integerTypeIsSignedDef :: Definition (IntegerType -> Bool)
integerTypeIsSignedDef = basicsDefinition "integerTypeIsSigned" $
  doc "Find whether a given integer type is signed (true) or unsigned (false)" $
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

integerTypePrecisionDef :: Definition (IntegerType -> Precision)
integerTypePrecisionDef = basicsDefinition "integerTypePrecision" $
  doc "Find the precision of a given integer type" $
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

integerTypesDef :: Definition [IntegerType]
integerTypesDef = basicsDefinition "integerTypes" $
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

integerValueTypeDef :: Definition (IntegerValue -> IntegerType)
integerValueTypeDef = basicsDefinition "integerValueType" $
  doc "Find the integer type for a given integer value" $
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

literalTypeDef :: Definition (Literal -> LiteralType)
literalTypeDef = basicsDefinition "literalType" $
  doc "Find the literal type for a given literal value" $
  match _Literal Nothing [
    Case _Literal_binary  --> constant $ variant _LiteralType _LiteralType_binary unit,
    Case _Literal_boolean --> constant $ variant _LiteralType _LiteralType_boolean unit,
    Case _Literal_float   --> inject2 _LiteralType _LiteralType_float <.> ref floatValueTypeDef,
    Case _Literal_integer --> inject2 _LiteralType _LiteralType_integer <.> ref integerValueTypeDef,
    Case _Literal_string  --> constant $ variant _LiteralType _LiteralType_string unit]

literalTypeVariantDef :: Definition (LiteralType -> LiteralVariant)
literalTypeVariantDef = basicsDefinition "literalTypeVariant" $
  doc "Find the literal type variant (constructor) for a given literal value" $
  matchToEnum _LiteralType _LiteralVariant Nothing [
    _LiteralType_binary  @-> _LiteralVariant_binary,
    _LiteralType_boolean @-> _LiteralVariant_boolean,
    _LiteralType_float   @-> _LiteralVariant_float,
    _LiteralType_integer @-> _LiteralVariant_integer,
    _LiteralType_string  @-> _LiteralVariant_string]

literalVariantDef :: Definition (Literal -> LiteralVariant)
literalVariantDef = basicsDefinition "literalVariant" $
  doc "Find the literal variant (constructor) for a given literal value" $
  function (TypeVariable _Literal) (TypeVariable _LiteralVariant) $
  ref literalTypeVariantDef <.> ref literalTypeDef

literalVariantsDef :: Definition [LiteralVariant]
literalVariantsDef = basicsDefinition "literalVariants" $
  doc "All literal variants, in a canonical order" $
  list $ unitVariant _LiteralVariant <$> [
    _LiteralVariant_binary,
    _LiteralVariant_boolean,
    _LiteralVariant_float,
    _LiteralVariant_integer,
    _LiteralVariant_string]

termMetaDef :: Definition (Graph a -> Term a -> a)
termMetaDef = basicsDefinition "termMeta" $
  function (Types.apply (TypeVariable _Graph) (Types.var "a")) (Types.function (Types.apply (TypeVariable _Term) (Types.var "a")) (Types.var "a")) $
  (project _AnnotationClass _AnnotationClass_termAnnotation) <.> (project _Graph _Graph_annotations)

termVariantDef :: Definition (Term a -> TermVariant)
termVariantDef = basicsDefinition "termVariant" $
  doc "Find the term variant (constructor) for a given term" $
  function (Types.apply (TypeVariable _Term) (Types.var "a")) (TypeVariable _TermVariant) $
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
      _Term_stream      @-> _TermVariant_stream,
      _Term_sum         @-> _TermVariant_sum,
      _Term_union       @-> _TermVariant_union,
      _Term_variable    @-> _TermVariant_variable,
      _Term_wrap        @-> _TermVariant_wrap]

termVariantsDef :: Definition [TermVariant]
termVariantsDef = basicsDefinition "termVariants" $
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
    _TermVariant_stream,
    _TermVariant_sum,
    _TermVariant_union,
    _TermVariant_variable,
    _TermVariant_wrap]

typeVariantDef :: Definition (Type a -> TypeVariant)
typeVariantDef = basicsDefinition "typeVariant" $
  doc "Find the type variant (constructor) for a given type" $
  function (Types.apply (TypeVariable _Type) (Types.var "a")) (TypeVariable _TypeVariant) $
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
      _Type_stream      @-> _TypeVariant_stream,
      _Type_sum         @-> _TypeVariant_sum,
      _Type_union       @-> _TypeVariant_union,
      _Type_variable    @-> _TypeVariant_variable,
      _Type_wrap        @-> _TypeVariant_wrap]

typeVariantsDef :: Definition [TypeVariant]
typeVariantsDef = basicsDefinition "typeVariants" $
  doc "All type variants, in a canonical order" $
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
    _TypeVariant_stream,
    _TypeVariant_sum,
    _TypeVariant_union,
    _TypeVariant_variable]

-- Common.hs

fieldMapDef :: Definition ([Field a] -> M.Map FieldName (Term a))
fieldMapDef = basicsDefinition "fieldMap" $
  function (TypeList fieldA) (Types.map (TypeVariable _FieldName) termA) $
    (lambda "fields" $ fromList @@ (Lists.map @@ var "toPair" @@ var "fields"))
  `with` [
    "toPair">: lambda "f" $ pair (project _Field _Field_name @@ var "f", project _Field _Field_term @@ var "f")]

fieldTypeMapDef :: Definition ([FieldType a] -> M.Map FieldName (Type a))
fieldTypeMapDef = basicsDefinition "fieldTypeMap" $
  function (TypeList fieldTypeA) (Types.map (TypeVariable _FieldName) typeA) $
    (lambda "fields" $ fromList @@ (Lists.map @@ var "toPair" @@ var "fields"))
  `with` [
    "toPair">: lambda "f" $ pair (project _FieldType _FieldType_name @@ var "f", project _FieldType _FieldType_type @@ var "f")]

ignoredVariableDef :: Definition String
ignoredVariableDef = basicsDefinition "ignoredVariable" $
  string "_"

isEncodedTypeDef :: Definition (Term a -> Bool)
isEncodedTypeDef = basicsDefinition "isEncodedType" $
  function termA Types.boolean $
  lambda "t" $ (match _Term (Just false) [
      Case _Term_application --> lambda "a" $
        ref isEncodedTypeDef @@ (project _Application _Application_function @@ var "a"),
      Case _Term_union       --> lambda "i" $
        equalString @@ (string $ unName _Type) @@ (unwrap _Name @@ (project _Injection _Injection_typeName @@ var "i"))
    ]) @@ (ref stripTermDef @@ var "t")

isTypeDef :: Definition (Type a -> Bool)
isTypeDef = basicsDefinition "isType" $
  functionWithClasses typeA Types.boolean eqA $
  lambda "t" $ (match _Type (Just false) [
      Case _Type_application --> lambda "a" $
        ref isTypeDef @@ (project _ApplicationType _ApplicationType_function @@ var "a"),
      Case _Type_lambda --> lambda "l" $
        ref isTypeDef @@ (project _LambdaType _LambdaType_body @@ var "l"),
      Case _Type_union --> lambda "rt" $
        equalString @@ (string $ unName _Type) @@ (unwrap _Name @@ (project _RowType _RowType_typeName @@ var "rt")),
      Case _Type_variable --> constant true
    ]) @@ (ref stripTypeDef @@ var "t")

isUnitTermDef :: Definition (Term a -> Bool)
isUnitTermDef = basicsDefinition "isUnitTerm" $
  functionWithClasses termA Types.boolean eqA $
  lambda "t" $ equalTerm @@ (ref stripTermDef @@ var "t") @@ Datum (coreEncodeTerm Terms.unit)

isUnitTypeDef :: Definition (Term a -> Bool)
isUnitTypeDef = basicsDefinition "isUnitType" $
  functionWithClasses typeA Types.boolean eqA $
  lambda "t" $ equalType @@ (ref stripTypeDef @@ var "t") @@ Datum (coreEncodeType Types.unit)


-- localNameOfLazy :: Name -> String
-- localNameOfLazy = qualifiedNameLocal . qualifyNameLazy
--
-- localNameOfEager :: Name -> String
-- localNameOfEager = qualifiedNameLocal . qualifyNameEager
--
-- namespaceOfLazy :: Name -> Maybe Namespace
-- namespaceOfLazy = qualifiedNameNamespace . qualifyNameLazy
--
-- namespaceOfEager :: Name -> Maybe Namespace
-- namespaceOfEager = qualifiedNameNamespace . qualifyNameEager

placeholderNameDef :: Definition Name
placeholderNameDef = basicsDefinition "placeholderName" $
  doc "A placeholder name for row types as they are being constructed" $
  wrap _Name $ string "Placeholder"

skipAnnotationsDef :: Definition ((a -> Maybe (Annotated a m)) -> a -> a)
skipAnnotationsDef = basicsDefinition "skipAnnotations" $
  function getAnnType (Types.function (Types.var "x") (Types.var "x")) $
  lambda "getAnn" $ lambda "t" $
    (var "skip" @@ var "t") `with` [
      "skip">:
        function (Types.var "x") (Types.var "x") $
        lambda "t1" $
          (matchOpt
            (var "t1")
            (lambda "ann" $ var "skip" @@ (project _Annotated _Annotated_subject @@ var "ann")))
          @@ (var "getAnn" @@ var "t1")]
  where
    getAnnType = (Types.function
      (Types.var "x")
      (Types.optional $ Types.apply (Types.apply (TypeVariable _Annotated) (Types.var "x")) (Types.var "a")))

stripTermDef :: Definition (Term a -> Term a)
stripTermDef = basicsDefinition "stripTerm" $
    doc "Strip all annotations from a term" $
    function termA termA $
      lambda "x" (ref skipAnnotationsDef @@ (match _Term (Just nothing) [
        Case _Term_annotated --> lambda "ann" (just $ var "ann")]) @@ var "x")

stripTypeDef :: Definition (Type a -> Type a)
stripTypeDef = basicsDefinition "stripType" $
    doc "Strip all annotations from a type" $
    function typeA typeA $
      lambda "x" (ref skipAnnotationsDef @@ (match _Type (Just nothing) [
        Case _Type_annotated --> lambda "ann" (just $ var "ann")]) @@ var "x")
  where
    typeA = Types.apply (TypeVariable _Type) (Types.var "a")

unqualifyNameDef :: Definition (QualifiedName -> Name)
unqualifyNameDef = basicsDefinition "unqualifyName" $
  doc "Convert a qualified name to a dot-separated name" $
  lambda "qname" $ (wrap _Name $ var "prefix" ++ (project _QualifiedName _QualifiedName_local @@ var "qname"))
    `with` [
      "prefix">: matchOpt (string "") (lambda "n" $ (unwrap _Namespace @@ var "n") ++ string ".")
        @@ (project _QualifiedName _QualifiedName_namespace @@ var "qname")]
