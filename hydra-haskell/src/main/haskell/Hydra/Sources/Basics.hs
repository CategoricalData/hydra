module Hydra.Sources.Basics where

import Hydra.Kernel
import Hydra.Sources.Graph
import Hydra.Sources.Mantle
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Lib.Lists as Lists
import Hydra.Dsl.Lib.Math as Math
import Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types


basicsDefinition :: String -> Datum a -> Definition a
basicsDefinition = Definition . fromQname (moduleNamespace hydraBasicsModule)

hydraBasicsModule :: Module Kv
hydraBasicsModule = Module (Namespace "hydra/basics") elements [hydraGraphModule, hydraMantleModule] $
    Just "Basic functions for working with types and terms"
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
      el primitiveArityDef,
      el qnameDef,
      el termVariantDef,
      el termVariantsDef,
      el testListsDef,
      el typeArityDef,
      el typeVariantDef,
      el typeVariantsDef]

eliminationVariantDef :: Definition (Elimination a -> EliminationVariant)
eliminationVariantDef = basicsDefinition "eliminationVariant" $
  doc "Find the elimination variant (constructor) for a given elimination term" $
  typed (Types.function (Types.apply (Types.wrap _Elimination) (Types.variable "a")) (Types.wrap _EliminationVariant)) $
  matchToEnum _Elimination _EliminationVariant Nothing [
    _Elimination_element  @-> _EliminationVariant_element,
    _Elimination_list     @-> _EliminationVariant_list,
    _Elimination_optional @-> _EliminationVariant_optional,
    _Elimination_record   @-> _EliminationVariant_record,
    _Elimination_union    @-> _EliminationVariant_union,
    _Elimination_wrap     @-> _EliminationVariant_wrap]

eliminationVariantsDef :: Definition [EliminationVariant]
eliminationVariantsDef = basicsDefinition "eliminationVariants" $
  doc "All elimination variants (constructors), in a canonical order" $
  typed (Types.list $ Types.wrap _EliminationVariant) $
  list $ unitVariant _EliminationVariant <$> [
    _EliminationVariant_element,
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
  typed (Types.list $ Types.wrap _FloatType) $
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
  typed (Types.function (Types.apply (Types.wrap _Function) (Types.variable "a")) (Types.wrap _FunctionVariant)) $
  matchToEnum _Function _FunctionVariant Nothing [
    _Function_elimination @-> _FunctionVariant_elimination,
    _Function_lambda      @-> _FunctionVariant_lambda,
    _Function_primitive   @-> _FunctionVariant_primitive]

functionVariantsDef :: Definition [FunctionVariant]
functionVariantsDef = basicsDefinition "functionVariants" $
  doc "All function variants (constructors), in a canonical order" $
    typed (Types.list $ Types.wrap _FunctionVariant) $
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
    typed (Types.list $ Types.wrap _IntegerType) $
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
  match _Literal (Types.wrap _LiteralType) Nothing [
    Case _Literal_binary  --> constant $ variant _LiteralType _LiteralType_binary unit,
    Case _Literal_boolean --> constant $ variant _LiteralType _LiteralType_boolean unit,
    Case _Literal_float   --> union2 _LiteralType _LiteralType_float <.> ref floatValueTypeDef,
    Case _Literal_integer --> union2 _LiteralType _LiteralType_integer <.> ref integerValueTypeDef,
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
  function (Types.wrap _Literal) (Types.wrap _LiteralVariant) $
  ref literalTypeVariantDef <.> ref literalTypeDef

literalVariantsDef :: Definition [LiteralVariant]
literalVariantsDef = basicsDefinition "literalVariants" $
  doc "All literal variants, in a canonical order" $
  typed (Types.list $ Types.wrap _LiteralVariant) $
  list $ unitVariant _LiteralVariant <$> [
    _LiteralVariant_binary,
    _LiteralVariant_boolean,
    _LiteralVariant_float,
    _LiteralVariant_integer,
    _LiteralVariant_string]

primitiveArityDef :: Definition (Primitive a -> Int)
primitiveArityDef = basicsDefinition "primitiveArity" $
  doc "Find the arity (expected number of arguments) of a primitive constant or function" $
  function (Types.apply (Types.wrap _Primitive) (Types.variable "a")) Types.int32 $
  (ref typeArityDef <.> (project _Primitive _Primitive_type))

qnameDef :: Definition (Namespace -> String -> Name)
qnameDef = basicsDefinition "qname" $
  doc "Construct a qualified (dot-separated) name" $
  functionN [Types.wrap _Namespace, Types.string] (Types.wrap _Name) $
  lambda "ns" $
    lambda "name" $
      nom _Name $
        apply cat $
          list [apply (denom _Namespace) (var "ns"), string ".", var "name"]

typeArityDef :: Definition (Type a -> Int)
typeArityDef = basicsDefinition "typeArity" $
  function (Types.apply (Types.wrap _Type) (Types.variable "a")) Types.int32 $
  match _Type Types.int32 (Just $ Terms.int32 0) [
    Case _Type_annotated --> ref typeArityDef <.> (project _Annotated _Annotated_subject),
    Case _Type_application --> ref typeArityDef <.> (project _ApplicationType _ApplicationType_function),
    Case _Type_lambda --> ref typeArityDef <.> (project _LambdaType _LambdaType_body),
    Case _Type_function --> lambda "f" $
      Math.add @@ (int32 1) @@ (ref typeArityDef @@ (apply (project _FunctionType _FunctionType_codomain) (var "f")))]

termVariantDef :: Definition (Term a -> TermVariant)
termVariantDef = basicsDefinition "termVariant" $
  doc "Find the term variant (constructor) for a given term" $
  function (Types.apply (Types.wrap _Term) (Types.variable "a")) (Types.wrap _TermVariant) $
  lambda "term" $ apply
    (matchToEnum _Term _TermVariant Nothing [
      _Term_annotated       @-> _TermVariant_annotated,
      _Term_application     @-> _TermVariant_application,
      _Term_element         @-> _TermVariant_element,
      _Term_function        @-> _TermVariant_function,
      _Term_let             @-> _TermVariant_let,
      _Term_list            @-> _TermVariant_list,
      _Term_literal         @-> _TermVariant_literal,
      _Term_map             @-> _TermVariant_map,
      _Term_optional        @-> _TermVariant_optional,
      _Term_product         @-> _TermVariant_product,
      _Term_record          @-> _TermVariant_record,
      _Term_set             @-> _TermVariant_set,
      _Term_stream          @-> _TermVariant_stream,
      _Term_sum             @-> _TermVariant_sum,
      _Term_union           @-> _TermVariant_union,
      _Term_variable        @-> _TermVariant_variable,
      _Term_wrap            @-> _TermVariant_wrap])
    (var "term")

termVariantsDef :: Definition [TermVariant]
termVariantsDef = basicsDefinition "termVariants" $
  doc "All term (expression) variants, in a canonical order" $
  typed (Types.list $ Types.wrap _TermVariant) $
  list $ unitVariant _TermVariant <$> [
    _TermVariant_annotated,
    _TermVariant_application,
    _TermVariant_literal,
    _TermVariant_element,
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

-- TODO: remove once there are other polymorphic functions in use
testListsDef :: Definition ([[a]] -> Int)
testListsDef = basicsDefinition "testLists" $
  doc "TODO: temporary. Just a token polymorphic function for testing" $
  function (Types.list $ Types.list $ Types.variable "a") Types.int32 $
  (lambda "els" (apply Lists.length (apply Lists.concat $ var "els")))

typeVariantDef :: Definition (Type a -> TypeVariant)
typeVariantDef = basicsDefinition "typeVariant" $
  doc "Find the type variant (constructor) for a given type" $
  function (Types.apply (Types.wrap _Type) (Types.variable "a")) (Types.wrap _TypeVariant) $
  lambda "typ" $ apply
    (matchToEnum _Type _TypeVariant Nothing [
      _Type_annotated   @-> _TypeVariant_annotated,
      _Type_application @-> _TypeVariant_application,
      _Type_element     @-> _TypeVariant_element,
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
      _Type_wrap        @-> _TypeVariant_wrap])
    (var "typ")

typeVariantsDef :: Definition [TypeVariant]
typeVariantsDef = basicsDefinition "typeVariants" $
  doc "All type variants, in a canonical order" $
  typed (Types.list $ Types.wrap _TypeVariant) $
  list $ unitVariant _TypeVariant <$> [
    _TypeVariant_annotated,
    _TypeVariant_application,
    _TypeVariant_element,
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
