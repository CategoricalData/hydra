module Hydra.Impl.Haskell.Sources.Basics where

import Hydra.All
import Hydra.Impl.Haskell.Dsl.Base as Base
import Hydra.Impl.Haskell.Sources.Module
import Hydra.Impl.Haskell.Sources.Mantle
import qualified Hydra.Impl.Haskell.Dsl.Standard as Standard
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Lib.Lists as Lists
import Hydra.Impl.Haskell.Dsl.Lib.Strings as Strings


basicsNs = Namespace "hydra/basics"

basics :: String -> Datum a -> Definition a
basics = Definition . fromQname basicsNs

hydraBasicsModule :: Module Meta
hydraBasicsModule = Module basicsNs elements [hydraMantleModule] $
    Just "Basic functions for working with types and terms"
  where
    elements = [
      el eliminationVariantSource,
      el eliminationVariantsSource,
      el floatTypePrecisionSource,
      el floatTypesSource,
      el floatValueTypeSource,
      el functionVariantSource,
      el functionVariantsSource,
      el integerTypeIsSignedSource,
      el integerTypePrecisionSource,
      el integerTypesSource,
      el integerValueTypeSource,
      el literalTypeSource,
      el literalTypeVariantSource,
      el literalVariantSource,
      el literalVariantsSource,
      el qnameSource,
      el termVariantSource,
      el termVariantsSource,
      el testListsSource,
      el typeVariantSource,
      el typeVariantsSource]

eliminationVariantSource :: Definition (Elimination m -> EliminationVariant)
eliminationVariantSource = basics "eliminationVariant" $
  doc "Find the elimination variant (constructor) for a given elimination term" $
  typed (Types.function (Types.apply (Types.nominal _Elimination) (Types.variable "m")) (Types.nominal _EliminationVariant)) $
  matchToEnum _Elimination _EliminationVariant [
    _Elimination_element  @-> _EliminationVariant_element,
    _Elimination_list     @-> _EliminationVariant_list,
    _Elimination_nominal  @-> _EliminationVariant_nominal,
    _Elimination_optional @-> _EliminationVariant_optional,
    _Elimination_record   @-> _EliminationVariant_record,
    _Elimination_union    @-> _EliminationVariant_union]

eliminationVariantsSource :: Definition [EliminationVariant]
eliminationVariantsSource = basics "eliminationVariants" $
  doc "All elimination variants (constructors), in a canonical order" $
  typed (Types.list $ Types.nominal _EliminationVariant) $
  list $ unitVariant _EliminationVariant <$> [
    _EliminationVariant_element,
    _EliminationVariant_list,
    _EliminationVariant_nominal,
    _EliminationVariant_optional,
    _EliminationVariant_record,
    _EliminationVariant_union]

floatTypePrecisionSource :: Definition (FloatType -> Precision)
floatTypePrecisionSource = basics "floatTypePrecision" $
  doc "Find the precision of a given floating-point type" $
  matchToUnion _FloatType _Precision [
    _FloatType_bigfloat @-> field _Precision_arbitrary unit,
    _FloatType_float32  @-> field _Precision_bits $ int 32,
    _FloatType_float64  @-> field _Precision_bits $ int 64]

floatTypesSource :: Definition [FloatType]
floatTypesSource = basics "floatTypes" $
  doc "All floating-point types in a canonical order" $
  typed (Types.list $ Types.nominal _FloatType) $
  list $ unitVariant _FloatType <$> [
    _FloatType_bigfloat,
    _FloatType_float32,
    _FloatType_float64]

floatValueTypeSource :: Definition (FloatValue -> FloatType)
floatValueTypeSource = basics "floatValueType" $
  doc "Find the float type for a given floating-point value" $
  matchToEnum _FloatValue _FloatType [
    _FloatValue_bigfloat @-> _FloatType_bigfloat,
    _FloatValue_float32  @-> _FloatType_float32,
    _FloatValue_float64  @-> _FloatType_float64]

functionVariantSource :: Definition (Function m -> FunctionVariant)
functionVariantSource = basics "functionVariant" $
  doc "Find the function variant (constructor) for a given function" $
  typed (Types.function (Types.apply (Types.nominal _Function) (Types.variable "m")) (Types.nominal _FunctionVariant)) $
  matchToEnum _Function _FunctionVariant [
    _Function_compareTo   @-> _FunctionVariant_compareTo,
    _Function_elimination @-> _FunctionVariant_elimination,
    _Function_lambda      @-> _FunctionVariant_lambda,
    _Function_primitive   @-> _FunctionVariant_primitive]

functionVariantsSource :: Definition [FunctionVariant]
functionVariantsSource = basics "functionVariants" $
  doc "All function variants (constructors), in a canonical order" $
    typed (Types.list $ Types.nominal _FunctionVariant) $
  list $ unitVariant _FunctionVariant <$> [
    _FunctionVariant_compareTo,
    _FunctionVariant_elimination,
    _FunctionVariant_lambda,
    _FunctionVariant_primitive]

integerTypeIsSignedSource :: Definition (IntegerType -> Bool)
integerTypeIsSignedSource = basics "integerTypeIsSigned" $
  doc "Find whether a given integer type is signed (true) or unsigned (false)" $
  matchData _IntegerType [
    _IntegerType_bigint @-> constant true,
    _IntegerType_int8   @-> constant true,
    _IntegerType_int16  @-> constant true,
    _IntegerType_int32  @-> constant true,
    _IntegerType_int64  @-> constant true,
    _IntegerType_uint8  @-> constant false,
    _IntegerType_uint16 @-> constant false,
    _IntegerType_uint32 @-> constant false,
    _IntegerType_uint64 @-> constant false]

integerTypePrecisionSource :: Definition (IntegerType -> Precision)
integerTypePrecisionSource = basics "integerTypePrecision" $
  doc "Find the precision of a given integer type" $
  matchToUnion _IntegerType _Precision [
    _IntegerType_bigint @-> field _Precision_arbitrary unit,
    _IntegerType_int8   @-> field _Precision_bits $ int 8,
    _IntegerType_int16  @-> field _Precision_bits $ int 16,
    _IntegerType_int32  @-> field _Precision_bits $ int 32,
    _IntegerType_int64  @-> field _Precision_bits $ int 64,
    _IntegerType_uint8  @-> field _Precision_bits $ int 8,
    _IntegerType_uint16 @-> field _Precision_bits $ int 16,
    _IntegerType_uint32 @-> field _Precision_bits $ int 32,
    _IntegerType_uint64 @-> field _Precision_bits $ int 64]

integerTypesSource :: Definition [IntegerType]
integerTypesSource = basics "integerTypes" $
    doc "All integer types, in a canonical order" $
    typed (Types.list $ Types.nominal _IntegerType) $
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

integerValueTypeSource :: Definition (IntegerValue -> IntegerType)
integerValueTypeSource = basics "integerValueType" $
  doc "Find the integer type for a given integer value" $
  matchToEnum _IntegerValue _IntegerType [
    _IntegerValue_bigint @-> _IntegerType_bigint,
    _IntegerValue_int8   @-> _IntegerType_int8,
    _IntegerValue_int16  @-> _IntegerType_int16,
    _IntegerValue_int32  @-> _IntegerType_int32,
    _IntegerValue_int64  @-> _IntegerType_int64,
    _IntegerValue_uint8  @-> _IntegerType_uint8,
    _IntegerValue_uint16 @-> _IntegerType_uint16,
    _IntegerValue_uint32 @-> _IntegerType_uint32,
    _IntegerValue_uint64 @-> _IntegerType_uint64]

literalTypeSource :: Definition (Literal -> LiteralType)
literalTypeSource = basics "literalType" $
  doc "Find the literal type for a given literal value" $
  match _Literal (Types.nominal _LiteralType) [
    Case _Literal_binary  --> constant $ variant _LiteralType _LiteralType_binary unit,
    Case _Literal_boolean --> constant $ variant _LiteralType _LiteralType_boolean unit,
    Case _Literal_float   --> union2 _LiteralType _LiteralType_float <.> ref floatValueTypeSource,
    Case _Literal_integer --> union2 _LiteralType _LiteralType_integer <.> ref integerValueTypeSource,
    Case _Literal_string  --> constant $ variant _LiteralType _LiteralType_string unit]

literalTypeVariantSource :: Definition (LiteralType -> LiteralVariant)
literalTypeVariantSource = basics "literalTypeVariant" $
  doc "Find the literal type variant (constructor) for a given literal value" $
  matchToEnum _LiteralType _LiteralVariant [
    _LiteralType_binary  @-> _LiteralVariant_binary,
    _LiteralType_boolean @-> _LiteralVariant_boolean,
    _LiteralType_float   @-> _LiteralVariant_float,
    _LiteralType_integer @-> _LiteralVariant_integer,
    _LiteralType_string  @-> _LiteralVariant_string]

literalVariantSource :: Definition (Literal -> LiteralVariant)
literalVariantSource = basics "literalVariant" $
  doc "Find the literal variant (constructor) for a given literal value" $
  function (Types.nominal _Literal) (Types.nominal _LiteralVariant) $
  ref literalTypeVariantSource <.> ref literalTypeSource

literalVariantsSource :: Definition [LiteralVariant]
literalVariantsSource = basics "literalVariants" $
  doc "All literal variants, in a canonical order" $
  typed (Types.list $ Types.nominal _LiteralVariant) $
  list $ unitVariant _LiteralVariant <$> [
    _LiteralVariant_binary,
    _LiteralVariant_boolean,
    _LiteralVariant_float,
    _LiteralVariant_integer,
    _LiteralVariant_string]

qnameSource :: Definition (Namespace -> String -> Name)
qnameSource = basics "qname" $
  doc "Construct a qualified (dot-separated) name" $
  function (Types.nominal _Namespace) (Types.function Types.string $ Types.nominal _Name) $
  lambda "ns" $
    lambda "name" $
      nom _Name $
        apply cat $
          list [apply (denom _Namespace) (var "ns"), string ".", var "name"]

termVariantSource :: Definition (Term m -> TermVariant)
termVariantSource = basics "termVariant" $
  doc "Find the term variant (constructor) for a given term" $
  function (Types.apply (Types.nominal _Term) (Types.variable "m")) (Types.nominal _TermVariant) $
  lambda "term" $ apply
    (matchToEnum _Term _TermVariant [
      _Term_annotated       @-> _TermVariant_annotated,
      _Term_application     @-> _TermVariant_application,
      _Term_element         @-> _TermVariant_element,
      _Term_function        @-> _TermVariant_function,
      _Term_let             @-> _TermVariant_let,
      _Term_list            @-> _TermVariant_list,
      _Term_literal         @-> _TermVariant_literal,
      _Term_map             @-> _TermVariant_map,
      _Term_nominal         @-> _TermVariant_nominal,
      _Term_optional        @-> _TermVariant_optional,
      _Term_product         @-> _TermVariant_product,
      _Term_record          @-> _TermVariant_record,
      _Term_set             @-> _TermVariant_set,
      _Term_stream          @-> _TermVariant_stream,
      _Term_sum             @-> _TermVariant_sum,
      _Term_union           @-> _TermVariant_union,
      _Term_variable        @-> _TermVariant_variable])
    (var "term")

termVariantsSource :: Definition [TermVariant]
termVariantsSource = basics "termVariants" $
  doc "All term (expression) variants, in a canonical order" $
  typed (Types.list $ Types.nominal _TermVariant) $
  list $ unitVariant _TermVariant <$> [
    _TermVariant_annotated,
    _TermVariant_application,
    _TermVariant_literal,
    _TermVariant_element,
    _TermVariant_function,
    _TermVariant_list,
    _TermVariant_map,
    _TermVariant_nominal,
    _TermVariant_optional,
    _TermVariant_product,
    _TermVariant_record,
    _TermVariant_set,
    _TermVariant_stream,
    _TermVariant_sum,
    _TermVariant_union,
    _TermVariant_variable]

-- TODO: remove once there are other polymorphic functions in use
testListsSource :: Definition ([[a]] -> Int)
testListsSource = basics "testLists" $
  doc "TODO: temporary. Just a token polymorphic function for testing" $
  function (Types.list $ Types.list $ Types.variable "a") Types.int32 $
  (lambda "els" (apply Lists.length (apply Lists.concat $ var "els")))

typeVariantSource :: Definition (Type m -> TypeVariant)
typeVariantSource = basics "typeVariant" $
  doc "Find the type variant (constructor) for a given type" $
  function (Types.apply (Types.nominal _Type) (Types.variable "m")) (Types.nominal _TypeVariant) $
  lambda "typ" $ apply
    (matchToEnum _Type _TypeVariant [
      _Type_annotated   @-> _TypeVariant_annotated,
      _Type_application @-> _TypeVariant_application,
      _Type_element     @-> _TypeVariant_element,
      _Type_function    @-> _TypeVariant_function,
      _Type_lambda      @-> _TypeVariant_lambda,
      _Type_list        @-> _TypeVariant_list,
      _Type_literal     @-> _TypeVariant_literal,
      _Type_map         @-> _TypeVariant_map,
      _Type_nominal     @-> _TypeVariant_nominal,
      _Type_optional    @-> _TypeVariant_optional,
      _Type_product     @-> _TypeVariant_product,
      _Type_record      @-> _TypeVariant_record,
      _Type_set         @-> _TypeVariant_set,
      _Type_stream      @-> _TypeVariant_stream,
      _Type_sum         @-> _TypeVariant_sum,
      _Type_union       @-> _TypeVariant_union,
      _Type_variable    @-> _TypeVariant_variable])
    (var "typ")

typeVariantsSource :: Definition [TypeVariant]
typeVariantsSource = basics "typeVariants" $
  doc "All type variants, in a canonical order" $
  typed (Types.list $ Types.nominal _TypeVariant) $
  list $ unitVariant _TypeVariant <$> [
    _TypeVariant_annotated,
    _TypeVariant_application,
    _TypeVariant_element,
    _TypeVariant_function,
    _TypeVariant_lambda,
    _TypeVariant_list,
    _TypeVariant_literal,
    _TypeVariant_map,
    _TypeVariant_nominal,
    _TypeVariant_optional,
    _TypeVariant_product,
    _TypeVariant_record,
    _TypeVariant_set,
    _TypeVariant_stream,
    _TypeVariant_sum,
    _TypeVariant_union,
    _TypeVariant_variable]
