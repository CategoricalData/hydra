module Hydra.Impl.Haskell.Sources.Basics where

import Hydra.Common
import Hydra.Core
import Hydra.Evaluation
import qualified Hydra.Graph as Graph
import Hydra.Impl.Haskell.Dsl.Base as Base
import Hydra.Impl.Haskell.Sources.Graph
import qualified Hydra.Impl.Haskell.Dsl.Standard as Standard
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Lib.Lists as Lists
import Hydra.Impl.Haskell.Dsl.Lib.Strings as Strings


hydraBasicsModule :: Result (Graph.Module Meta)
hydraBasicsModule = do
  g <- Standard.graph hydraBasicsName [
    el eliminationVariant,
    el eliminationVariants,
    el floatTypePrecision,
    el floatTypes,
    el floatValueType,
    el functionVariant,
    el functionVariants,
    el integerTypeIsSigned,
    el integerTypePrecision,
    el integerTypes,
    el integerValueType,
    el literalType,
    el literalTypeVariant,
    el literalVariant,
    el literalVariants,
    el qname,
    el termVariant,
    el termVariants,
    el testLists,
    el typeVariant,
    el typeVariants]
  return $ Graph.Module g [hydraGraphModule]

hydraBasicsName :: Graph.GraphName
hydraBasicsName = Graph.GraphName "hydra/basics"

basics :: String -> Data a -> Element a
basics = Element . fromQname hydraBasicsName

eliminationVariant :: Element (Elimination m -> EliminationVariant)
eliminationVariant = basics "eliminationVariant" $
  doc "Find the elimination variant (constructor) for a given elimination term" $
  typed (Types.function (Types.apply (Types.nominal _Elimination) (Types.variable "m")) (Types.nominal _EliminationVariant)) $
  matchToEnum _Elimination _EliminationVariant [
    _Elimination_element  @-> _EliminationVariant_element,
    _Elimination_nominal  @-> _EliminationVariant_nominal,
    _Elimination_optional @-> _EliminationVariant_optional,
    _Elimination_record   @-> _EliminationVariant_record,
    _Elimination_union    @-> _EliminationVariant_union]

eliminationVariants :: Element [EliminationVariant]
eliminationVariants = basics "eliminationVariants" $
  doc "All elimination variants (constructors), in a canonical order" $
  typed (Types.list $ Types.nominal _EliminationVariant) $
  list $ unitVariant _EliminationVariant <$> [
    _EliminationVariant_element,
    _EliminationVariant_nominal,
    _EliminationVariant_optional,
    _EliminationVariant_record,
    _EliminationVariant_union]

floatTypePrecision :: Element (FloatType -> Precision)
floatTypePrecision = basics "floatTypePrecision" $
  doc "Find the precision of a given floating-point type" $
  matchToUnion _FloatType _Precision [
    _FloatType_bigfloat @-> field _Precision_arbitrary unit,
    _FloatType_float32  @-> field _Precision_bits $ int 32,
    _FloatType_float64  @-> field _Precision_bits $ int 64]

floatTypes :: Element [FloatType]
floatTypes = basics "floatTypes" $
  doc "All floating-point types in a canonical order" $
  typed (Types.list $ Types.nominal _FloatType) $
  list $ unitVariant _FloatType <$> [
    _FloatType_bigfloat,
    _FloatType_float32,
    _FloatType_float64]

floatValueType :: Element (FloatValue -> FloatType)
floatValueType = basics "floatValueType" $
  doc "Find the float type for a given floating-point value" $
  matchToEnum _FloatValue _FloatType [
    _FloatValue_bigfloat @-> _FloatType_bigfloat,
    _FloatValue_float32  @-> _FloatType_float32,
    _FloatValue_float64  @-> _FloatType_float64]

functionVariant :: Element (Function m -> FunctionVariant)
functionVariant = basics "functionVariant" $
  doc "Find the function variant (constructor) for a given function" $
  typed (Types.function (Types.apply (Types.nominal _Function) (Types.variable "m")) (Types.nominal _FunctionVariant)) $
  matchToEnum _Function _FunctionVariant [
    _Function_compareTo   @-> _FunctionVariant_compareTo,
    _Function_elimination @-> _FunctionVariant_elimination,
    _Function_lambda      @-> _FunctionVariant_lambda,
    _Function_primitive   @-> _FunctionVariant_primitive]

functionVariants :: Element [FunctionVariant]
functionVariants = basics "functionVariants" $
  doc "All function variants (constructors), in a canonical order" $
    typed (Types.list $ Types.nominal _FunctionVariant) $
  list $ unitVariant _FunctionVariant <$> [
    _FunctionVariant_compareTo,
    _FunctionVariant_elimination,
    _FunctionVariant_lambda,
    _FunctionVariant_primitive]

integerTypeIsSigned :: Element (IntegerType -> Bool)
integerTypeIsSigned = basics "integerTypeIsSigned" $
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

integerTypePrecision :: Element (IntegerType -> Precision)
integerTypePrecision = basics "integerTypePrecision" $
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

integerTypes :: Element [IntegerType]
integerTypes = basics "integerTypes" $
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

integerValueType :: Element (IntegerValue -> IntegerType)
integerValueType = basics "integerValueType" $
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

literalType :: Element (Literal -> LiteralType)
literalType = basics "literalType" $
  doc "Find the literal type for a given literal value" $
  match _Literal (Types.nominal _LiteralType) [
    Case _Literal_binary  --> constant $ variant _LiteralType _LiteralType_binary unit,
    Case _Literal_boolean --> constant $ variant _LiteralType _LiteralType_boolean unit,
    Case _Literal_float   --> union2 _LiteralType _LiteralType_float <.> ref floatValueType,
    Case _Literal_integer --> union2 _LiteralType _LiteralType_integer <.> ref integerValueType,
    Case _Literal_string  --> constant $ variant _LiteralType _LiteralType_string unit]

literalTypeVariant :: Element (LiteralType -> LiteralVariant)
literalTypeVariant = basics "literalTypeVariant" $
  doc "Find the literal type variant (constructor) for a given literal value" $
  matchToEnum _LiteralType _LiteralVariant [
    _LiteralType_binary  @-> _LiteralVariant_binary,
    _LiteralType_boolean @-> _LiteralVariant_boolean,
    _LiteralType_float   @-> _LiteralVariant_float,
    _LiteralType_integer @-> _LiteralVariant_integer,
    _LiteralType_string  @-> _LiteralVariant_string]

literalVariant :: Element (Literal -> LiteralVariant)
literalVariant = basics "literalVariant" $
  doc "Find the literal variant (constructor) for a given literal value" $
  function (Types.nominal _Literal) (Types.nominal _LiteralVariant) $
  ref literalTypeVariant <.> ref literalType

literalVariants :: Element [LiteralVariant]
literalVariants = basics "literalVariants" $
  doc "All literal variants, in a canonical order" $
  typed (Types.list $ Types.nominal _LiteralVariant) $
  list $ unitVariant _LiteralVariant <$> [
    _LiteralVariant_binary,
    _LiteralVariant_boolean,
    _LiteralVariant_float,
    _LiteralVariant_integer,
    _LiteralVariant_string]

qname :: Element (Graph.GraphName -> String -> Name)
qname = basics "qname" $
  doc "Construct a qualified (dot-separated) name" $
  function (Types.nominal Graph._GraphName) (Types.function Types.string $ Types.nominal _Name) $
  lambda "ns" $
    lambda "name" $
      nom _Name $
        apply cat $
          list [apply (denom Graph._GraphName) (var "ns"), string ".", var "name"]

termVariant :: Element (Term m -> TermVariant)
termVariant = basics "termVariant" $
  doc "Find the term variant (constructor) for a given term" $
  function (Types.apply (Types.nominal _Term) (Types.variable "m")) (Types.nominal _TermVariant) $
  lambda "term" $ apply
    (matchToEnum _Term _TermVariant [
      _Term_annotated       @-> _TermVariant_annotated,
      _Term_application     @-> _TermVariant_application,
      _Term_element         @-> _TermVariant_element,
      _Term_function        @-> _TermVariant_function,
      _Term_list            @-> _TermVariant_list,
      _Term_literal         @-> _TermVariant_literal,
      _Term_map             @-> _TermVariant_map,
      _Term_nominal         @-> _TermVariant_nominal,
      _Term_optional        @-> _TermVariant_optional,
      _Term_record          @-> _TermVariant_record,
      _Term_set             @-> _TermVariant_set,
      _Term_union           @-> _TermVariant_union,
      _Term_variable        @-> _TermVariant_variable])
    (var "term")

termVariants :: Element [TermVariant]
termVariants = basics "termVariants" $
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
    _TermVariant_record,
    _TermVariant_set,
    _TermVariant_union,
    _TermVariant_variable]

-- TODO: remove once there are other polymorphic functions in use
testLists :: Element ([[a]] -> Int)
testLists = basics "testLists" $
  doc "TODO: temporary. Just a token polymorphic function for testing" $
  function (Types.list $ Types.list $ Types.variable "a") Types.int32 $
  (lambda "els" (apply Lists.length (apply Lists.concat $ var "els")))

typeVariant :: Element (Type m -> TypeVariant)
typeVariant = basics "typeVariant" $
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
      _Type_record      @-> _TypeVariant_record,
      _Type_set         @-> _TypeVariant_set,
      _Type_union       @-> _TypeVariant_union,
      _Type_variable    @-> _TypeVariant_variable])
    (var "typ")

typeVariants :: Element [TypeVariant]
typeVariants = basics "typeVariants" $
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
    _TypeVariant_record,
    _TypeVariant_set,
    _TypeVariant_union,
    _TypeVariant_variable]
