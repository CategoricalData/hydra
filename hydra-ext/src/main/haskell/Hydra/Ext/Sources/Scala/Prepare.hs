module Hydra.Ext.Sources.Scala.Prepare where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Set                                  as S


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.scala.prepare"

module_ :: Module
module_ = Module ns elements
    [Rewriting.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "Type preparation functions for Scala code generation"
  where
    elements = [
      toBinding prepareLiteralType,
      toBinding prepareFloatType,
      toBinding prepareIntegerType,
      toBinding prepareType,
      toBinding same]


prepareLiteralType :: TBinding (LiteralType -> (LiteralType, Literal -> Literal, S.Set String))
prepareLiteralType = def "prepareLiteralType" $
  doc "Prepare a literal type for Scala, substituting unsupported types" $
  lambda "at" $
    (cases _LiteralType (var "at") (Just (same @@ var "at")) [
      _LiteralType_binary>>: (constant $
        triple
          (Core.literalTypeString)
          ("v" ~> cases _Literal (var "v") (Just (var "v")) [
            _Literal_binary>>: ("b" ~> inject _Literal _Literal_string (Literals.binaryToString (var "b")))])
          (Sets.fromList $ list [string "replace binary strings with character strings"])),
      _LiteralType_float>>: ("ft" ~> lets [
        "result">: prepareFloatType @@ var "ft",
        "rtyp">: Pairs.first (var "result"),
        "rep">: Pairs.first (Pairs.second (var "result")),
        "msgs">: Pairs.second (Pairs.second (var "result"))] $
        triple
          (Core.literalTypeFloat (var "rtyp"))
          ("v" ~> cases _Literal (var "v") (Just (var "v")) [
            _Literal_float>>: ("fv" ~> inject _Literal _Literal_float (var "rep" @@ var "fv"))])
          (var "msgs")),
      _LiteralType_integer>>: ("it" ~> lets [
        "result">: prepareIntegerType @@ var "it",
        "rtyp">: Pairs.first (var "result"),
        "rep">: Pairs.first (Pairs.second (var "result")),
        "msgs">: Pairs.second (Pairs.second (var "result"))] $
        triple
          (Core.literalTypeInteger (var "rtyp"))
          ("v" ~> cases _Literal (var "v") (Just (var "v")) [
            _Literal_integer>>: ("iv" ~> inject _Literal _Literal_integer (var "rep" @@ var "iv"))])
          (var "msgs"))])

prepareFloatType :: TBinding (FloatType -> (FloatType, FloatValue -> FloatValue, S.Set String))
prepareFloatType = def "prepareFloatType" $
  doc "Prepare a float type for Scala" $
  lambda "ft" $
    (cases _FloatType (var "ft") (Just (same @@ var "ft")) [
      _FloatType_bigfloat>>: (constant $
        triple
          Core.floatTypeFloat64
          ("v" ~> cases _FloatValue (var "v") (Just (var "v")) [
            _FloatValue_bigfloat>>: ("d" ~> inject _FloatValue _FloatValue_float64 (var "d"))])
          (Sets.fromList $ list [string "replace arbitrary-precision floating-point numbers with 64-bit floating-point numbers (doubles)"]))])

prepareIntegerType :: TBinding (IntegerType -> (IntegerType, IntegerValue -> IntegerValue, S.Set String))
prepareIntegerType = def "prepareIntegerType" $
  doc "Prepare an integer type for Scala" $
  lambda "it" $
    (cases _IntegerType (var "it") (Just (same @@ var "it")) [
      _IntegerType_bigint>>: (constant $
        triple
          Core.integerTypeInt64
          ("v" ~> cases _IntegerValue (var "v") (Just (var "v")) [
            _IntegerValue_bigint>>: ("i" ~> inject _IntegerValue _IntegerValue_int64 (var "i"))])
          (Sets.fromList $ list [string "replace arbitrary-precision integers with 64-bit integers"])),
      _IntegerType_uint8>>: (constant $
        triple
          Core.integerTypeInt8
          ("v" ~> cases _IntegerValue (var "v") (Just (var "v")) [
            _IntegerValue_uint8>>: ("i" ~> inject _IntegerValue _IntegerValue_int8 (var "i"))])
          (Sets.fromList $ list [string "replace unsigned 8-bit integers with signed 8-bit integers"])),
      _IntegerType_uint32>>: (constant $
        triple
          Core.integerTypeInt32
          ("v" ~> cases _IntegerValue (var "v") (Just (var "v")) [
            _IntegerValue_uint32>>: ("i" ~> inject _IntegerValue _IntegerValue_int32 (var "i"))])
          (Sets.fromList $ list [string "replace unsigned 32-bit integers with signed 32-bit integers"])),
      _IntegerType_uint64>>: (constant $
        triple
          Core.integerTypeInt64
          ("v" ~> cases _IntegerValue (var "v") (Just (var "v")) [
            _IntegerValue_uint64>>: ("i" ~> inject _IntegerValue _IntegerValue_int64 (var "i"))])
          (Sets.fromList $ list [string "replace unsigned 64-bit integers with signed 64-bit integers"]))])

prepareType :: TBinding (Graph -> Type -> (Type, Term -> Term, S.Set String))
prepareType = def "prepareType" $
  doc "Prepare a type for Scala code generation, substituting unsupported types" $
  lambda "cx" $ lambda "typ" $
    (cases _Type (Rewriting.deannotateType @@ var "typ") (Just (same @@ var "typ")) [
      _Type_literal>>: ("at" ~> lets [
        "result">: prepareLiteralType @@ var "at",
        "rtyp">: Pairs.first (var "result"),
        "rep">: Pairs.first (Pairs.second (var "result")),
        "msgs">: Pairs.second (Pairs.second (var "result"))] $
        triple
          (MetaTypes.literal (var "rtyp"))
          ("v" ~> cases _Term (var "v") (Just (var "v")) [
            _Term_literal>>: ("av" ~> inject _Term _Term_literal (var "rep" @@ var "av"))])
          (var "msgs"))])

same :: TBinding (a -> (a, b -> b, S.Set c))
same = def "same" $
  doc "Return a type unchanged with identity transform and no messages" $
  lambda "x" $
    triple (var "x") ("y" ~> var "y") (Sets.empty)
