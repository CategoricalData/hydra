{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Adapt.Literals where

-- Standard imports for kernel terms modules
import Hydra.Kernel
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

import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils  as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Monads       as Monads
import qualified Hydra.Sources.Kernel.Terms.Reflect      as Reflect
import qualified Hydra.Sources.Kernel.Terms.Show.Core    as ShowCore


module_ :: Module
module_ = Module (Namespace "hydra.adapt.literals") elements
    [AdaptUtils.module_, ExtractCore.module_, Monads.module_, Reflect.module_, ShowCore.module_]
    kernelTypesModules $
    Just "Adapter framework for literal types and terms"
  where
   elements = [
     el comparePrecisionDef,
     el convertFloatValueDef,
     el convertIntegerValueDef,
     el disclaimerDef,
     el literalAdapterDef,
     el floatAdapterDef,
     el integerAdapterDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

comparePrecisionDef :: TBinding (Precision -> Precision -> Comparison)
comparePrecisionDef = define "comparePrecision" $
  doc "Compare two precision values" $
  "p1" ~> "p2" ~>
  cases _Precision (var "p1")
    Nothing [
    _Precision_arbitrary>>: constant (
      cases _Precision (var "p2")
        Nothing [
        _Precision_arbitrary>>: constant Graph.comparisonEqualTo,
        _Precision_bits>>: constant Graph.comparisonGreaterThan]),
    _Precision_bits>>: "b1" ~>
      cases _Precision (var "p2")
        Nothing [
        _Precision_arbitrary>>: constant Graph.comparisonLessThan,
        _Precision_bits>>: "b2" ~>
          Logic.ifElse (Equality.lt (var "b1") (var "b2"))
            Graph.comparisonLessThan
            Graph.comparisonGreaterThan]]

convertFloatValueDef :: TBinding (FloatType -> FloatValue -> FloatValue)
convertFloatValueDef = define "convertFloatValue" $
  doc "Convert a float value to a different float type" $
  "target" ~> "fv" ~>
  "decoder" <~ ("fv" ~>
    cases _FloatValue (var "fv")
      Nothing [
      _FloatValue_bigfloat>>: "d" ~> var "d",
      _FloatValue_float32>>: "f" ~> Literals.float32ToBigfloat (var "f"),
      _FloatValue_float64>>: "d" ~> Literals.float64ToBigfloat (var "d")]) $
  "encoder" <~ ("d" ~>
    cases _FloatType (var "target")
      Nothing [
      _FloatType_bigfloat>>: constant (Core.floatValueBigfloat (var "d")),
      _FloatType_float32>>: constant (Core.floatValueFloat32 (Literals.bigfloatToFloat32 (var "d"))),
      _FloatType_float64>>: constant (Core.floatValueFloat64 (Literals.bigfloatToFloat64 (var "d")))]) $
  var "encoder" @@ (var "decoder" @@ var "fv")

convertIntegerValueDef :: TBinding (IntegerType -> IntegerValue -> IntegerValue)
convertIntegerValueDef = define "convertIntegerValue" $
  doc "Convert an integer value to a different integer type" $
  "target" ~> "iv" ~>
  "decoder" <~ ("iv" ~>
    cases _IntegerValue (var "iv")
      Nothing [
      _IntegerValue_bigint>>: "v" ~> var "v",
      _IntegerValue_int8>>: "v" ~> Literals.int8ToBigint (var "v"),
      _IntegerValue_int16>>: "v" ~> Literals.int16ToBigint (var "v"),
      _IntegerValue_int32>>: "v" ~> Literals.int32ToBigint (var "v"),
      _IntegerValue_int64>>: "v" ~> Literals.int64ToBigint (var "v"),
      _IntegerValue_uint8>>: "v" ~> Literals.uint8ToBigint (var "v"),
      _IntegerValue_uint16>>: "v" ~> Literals.uint16ToBigint (var "v"),
      _IntegerValue_uint32>>: "v" ~> Literals.uint32ToBigint (var "v"),
      _IntegerValue_uint64>>: "v" ~> Literals.uint64ToBigint (var "v")]) $
  "encoder" <~ ("d" ~>
    cases _IntegerType (var "target")
      Nothing [
      _IntegerType_bigint>>: constant (Core.integerValueBigint (var "d")),
      _IntegerType_int8>>: constant (Core.integerValueInt8 (Literals.bigintToInt8 (var "d"))),
      _IntegerType_int16>>: constant (Core.integerValueInt16 (Literals.bigintToInt16 (var "d"))),
      _IntegerType_int32>>: constant (Core.integerValueInt32 (Literals.bigintToInt32 (var "d"))),
      _IntegerType_int64>>: constant (Core.integerValueInt64 (Literals.bigintToInt64 (var "d"))),
      _IntegerType_uint8>>: constant (Core.integerValueUint8 (Literals.bigintToUint8 (var "d"))),
      _IntegerType_uint16>>: constant (Core.integerValueUint16 (Literals.bigintToUint16 (var "d"))),
      _IntegerType_uint32>>: constant (Core.integerValueUint32 (Literals.bigintToUint32 (var "d"))),
      _IntegerType_uint64>>: constant (Core.integerValueUint64 (Literals.bigintToUint64 (var "d")))]) $
  var "encoder" @@ (var "decoder" @@ var "iv")

disclaimerDef :: TBinding (Bool -> String -> String -> String)
disclaimerDef = define "disclaimer" $
  doc "Generate a disclaimer message for type conversions" $
  "lossy" ~> "source" ~> "target" ~>
  Strings.cat (list [
    "replace ",
    var "source",
    " with ",
    var "target",
    Logic.ifElse (var "lossy") " (lossy)" ""])

floatAdapterDef :: TBinding (FloatType -> Flow AdapterContext (SymmetricAdapter s FloatType FloatValue))
floatAdapterDef = define "floatAdapter" $
  doc "Create an adapter for float types" $
  "ft" ~>
  "makeAdapter" <~ ("source" ~> "target" ~>
    "lossy" <~ Equality.equal
      (ref comparePrecisionDef
        @@ (ref Reflect.floatTypePrecisionDef @@ var "source")
        @@ (ref Reflect.floatTypePrecisionDef @@ var "target"))
      Graph.comparisonGreaterThan $
    "step" <~ Compute.coder
      ("fv" ~> produce (ref convertFloatValueDef @@ var "target" @@ var "fv"))
      ("fv" ~> produce (ref convertFloatValueDef @@ var "source" @@ var "fv")) $
    "msg" <~ ref disclaimerDef
      @@ var "lossy"
      @@ (ref ShowCore.floatTypeDef @@ var "source")
      @@ (ref ShowCore.floatTypeDef @@ var "target") $
    ref Monads.warnDef
      @@ var "msg"
      @@ (produce (Compute.adapter (var "lossy") (var "source") (var "target") (var "step")))) $
  "altTypes" <~ ("t" ~> cases _FloatType (var "t")
    Nothing [
    _FloatType_bigfloat>>: constant (list [Core.floatTypeFloat64, Core.floatTypeFloat32]),
    _FloatType_float32>>: constant (list [Core.floatTypeFloat64, Core.floatTypeBigfloat]),
    _FloatType_float64>>: constant (list [Core.floatTypeBigfloat, Core.floatTypeFloat32])]) $
  "alts" <~ ("t" ~> Flows.mapList (var "makeAdapter" @@ var "t") (var "altTypes" @@ var "t")) $
  "cx" <<~ ref Monads.getStateDef $
  "supported" <~ ref AdaptUtils.floatTypeIsSupportedDef
    @@ (Coders.languageConstraintsProjection (Coders.adapterContextLanguage (var "cx"))) $
  ref AdaptUtils.chooseAdapterDef
    @@ var "alts"
    @@ var "supported"
    @@ ref ShowCore.floatTypeDef
    @@ ref ShowCore.floatTypeDef
    @@ var "ft"

integerAdapterDef :: TBinding (IntegerType -> Flow AdapterContext (SymmetricAdapter s IntegerType IntegerValue))
integerAdapterDef = define "integerAdapter" $
  doc "Create an adapter for integer types" $
  "it" ~>
  "interleave" <~ ("xs" ~> "ys" ~> Lists.concat (Lists.transpose (list [var "xs", var "ys"]))) $
  "signedOrdered" <~ Lists.filter
    ("v" ~> Logic.and
      (ref Reflect.integerTypeIsSignedDef @@ var "v")
      (Logic.not (Equality.equal (ref Reflect.integerTypePrecisionDef @@ var "v") Util.precisionArbitrary)))
    (ref Reflect.integerTypesDef) $
  "unsignedOrdered" <~ Lists.filter
    ("v" ~> Logic.and
      (Logic.not (ref Reflect.integerTypeIsSignedDef @@ var "v"))
      (Logic.not (Equality.equal (ref Reflect.integerTypePrecisionDef @@ var "v") Util.precisionArbitrary)))
    (ref Reflect.integerTypesDef) $
  "signedPref" <~ var "interleave" @@ var "signedOrdered" @@ var "unsignedOrdered" $
  "unsignedPref" <~ var "interleave" @@ var "unsignedOrdered" @@ var "signedOrdered" $
  "signedNonPref" <~ Lists.reverse (var "unsignedPref") $
  "unsignedNonPref" <~ Lists.reverse (var "signedPref") $
  "signed" <~ ("i" ~> Lists.concat (list [
    Lists.drop (Math.mul (var "i") (int32 2)) (var "signedPref"),
    list [Core.integerTypeBigint],
    Lists.drop (Math.add (Math.sub (int32 8) (Math.mul (var "i") (int32 2))) (int32 1)) (var "signedNonPref")])) $
  "unsigned" <~ ("i" ~> Lists.concat (list [
    Lists.drop (Math.mul (var "i") (int32 2)) (var "unsignedPref"),
    list [Core.integerTypeBigint],
    Lists.drop (Math.add (Math.sub (int32 8) (Math.mul (var "i") (int32 2))) (int32 1)) (var "unsignedNonPref")])) $
  "makeAdapter" <~ ("source" ~> "target" ~>
    "lossy" <~ Logic.not (Equality.equal
      (ref comparePrecisionDef
        @@ (ref Reflect.integerTypePrecisionDef @@ var "source")
        @@ (ref Reflect.integerTypePrecisionDef @@ var "target"))
      Graph.comparisonLessThan) $
    "step" <~ Compute.coder
      ("iv" ~> produce (ref convertIntegerValueDef @@ var "target" @@ var "iv"))
      ("iv" ~> produce (ref convertIntegerValueDef @@ var "source" @@ var "iv")) $
    "msg" <~ ref disclaimerDef
      @@ var "lossy"
      @@ (ref ShowCore.integerTypeDef @@ var "source")
      @@ (ref ShowCore.integerTypeDef @@ var "target") $
    ref Monads.warnDef
      @@ var "msg"
      @@ (produce (Compute.adapter (var "lossy") (var "source") (var "target") (var "step")))) $
  "altTypes" <~ ("t" ~> cases _IntegerType (var "t")
    Nothing [
    _IntegerType_bigint>>: constant (Lists.reverse (var "unsignedPref")),
    _IntegerType_int8>>: constant (var "signed" @@ int32 1),
    _IntegerType_int16>>: constant (var "signed" @@ int32 2),
    _IntegerType_int32>>: constant (var "signed" @@ int32 3),
    _IntegerType_int64>>: constant (var "signed" @@ int32 4),
    _IntegerType_uint8>>: constant (var "unsigned" @@ int32 1),
    _IntegerType_uint16>>: constant (var "unsigned" @@ int32 2),
    _IntegerType_uint32>>: constant (var "unsigned" @@ int32 3),
    _IntegerType_uint64>>: constant (var "unsigned" @@ int32 4)]) $
  "alts" <~ ("t" ~> Flows.mapList (var "makeAdapter" @@ var "t") (var "altTypes" @@ var "t")) $
  "cx" <<~ ref Monads.getStateDef $
  "supported" <~ ref AdaptUtils.integerTypeIsSupportedDef
    @@ (Coders.languageConstraintsProjection (Coders.adapterContextLanguage (var "cx"))) $
  ref AdaptUtils.chooseAdapterDef
    @@ var "alts"
    @@ var "supported"
    @@ ref ShowCore.integerTypeDef
    @@ ref ShowCore.integerTypeDef
    @@ var "it"

literalAdapterDef :: TBinding (LiteralType -> Flow AdapterContext (SymmetricAdapter s LiteralType Literal))
literalAdapterDef = define "literalAdapter" $
  doc "Create an adapter for literal types" $
  "lt" ~>
  "forBinary" <~ ("t" ~>
    "matchBinary" <~ ("lit" ~> cases _Literal (var "lit")
      Nothing [
      _Literal_binary>>: "b" ~> Flows.pure (Core.literalString (Literals.binaryToString (var "b")))]) $
    "matchString" <~ ("lit" ~> cases _Literal (var "lit")
      Nothing [
      _Literal_string>>: "s" ~> Flows.pure (Core.literalBinary (Literals.stringToBinary (var "s")))]) $
    "step" <~ Compute.coder (var "matchBinary") (var "matchString") $
    produce (list [Compute.adapter false (var "t") Core.literalTypeString (var "step")])) $
  "forBoolean" <~ ("t" ~>
    "matchBoolean" <~ ("step'" ~> "lit" ~> cases _Literal (var "lit")
      Nothing [
      _Literal_boolean>>: "bv" ~>
        "iv" <<~ Compute.coderEncode (var "step'") @@ (Core.integerValueUint8 (Logic.ifElse (var "bv") (uint8 1) (uint8 0))) $
        produce (Core.literalInteger (var "iv"))]) $
    "matchInteger" <~ ("step'" ~> "lit" ~>
      "forValue" <~ ("val" ~> cases _IntegerValue (var "val")
        Nothing [
        _IntegerValue_uint8>>: "v" ~> Core.literalBoolean (Equality.equal (var "v") (uint8 1))]) $
      cases _Literal (var "lit")
        Nothing [
        _Literal_integer>>: "iv" ~>
          "val" <<~ Compute.coderDecode (var "step'") @@ var "iv" $
          produce $ var "forValue" @@ var "val"]) $
    "cx" <<~ ref Monads.getStateDef $
    "constraints" <~ Coders.languageConstraintsProjection (Coders.adapterContextLanguage (var "cx")) $
    "hasIntegers" <~ Logic.not (Sets.null (Coders.languageConstraintsIntegerTypes (var "constraints"))) $
    "hasStrings" <~ Sets.member Variants.literalVariantString (Coders.languageConstraintsLiteralVariants (var "constraints")) $
    "withIntegers" <~ (
      "withAdapter" <~ ("adapter" ~>
        "step'" <~ Compute.adapterCoder (var "adapter") $
        "step" <~ Compute.coder (var "matchBoolean" @@ var "step'") (var "matchInteger" @@ var "step'") $
        produce (list [Compute.adapter false (var "t") (Core.literalTypeInteger (Compute.adapterTarget (var "adapter"))) (var "step")])) $
      "adapter" <<~ ref integerAdapterDef @@ Core.integerTypeUint8 $
      var "withAdapter" @@ var "adapter") $
    "withStrings" <~ (
      "encode" <~ ("lit" ~>
        "b" <<~ ref ExtractCore.booleanLiteralDef @@ var "lit" $
        produce (Core.literalString (Logic.ifElse (var "b") "true" "false"))) $
      "decode" <~ ("lit" ~>
        "s" <<~ ref ExtractCore.stringLiteralDef @@ var "lit" $
        Logic.ifElse (Equality.equal (var "s") "true")
          (produce (Core.literalBoolean true))
          (Logic.ifElse (Equality.equal (var "s") "false")
            (produce (Core.literalBoolean false))
            (ref Monads.unexpectedDef @@ "boolean literal" @@ var "s"))) $
      list [Compute.adapter false (var "t") Core.literalTypeString (Compute.coder (var "encode") (var "decode"))]) $
    Logic.ifElse (var "hasIntegers")
      (var "withIntegers")
      (Logic.ifElse (var "hasStrings")
        (produce $ var "withStrings")
        (Flows.fail "no alternatives available for boolean encoding"))) $
  "forFloat" <~ ("t" ~> "ft" ~>
      "withFloats" <~ (
        "adapt" <~ ("adapter" ~> "dir" ~> "l" ~> cases _Literal (var "l")
          (Just (ref Monads.unexpectedDef
            @@ "floating-point literal"
            @@ (ref ShowCore.literalDef @@ var "l"))) [
          _Literal_float>>: "fv" ~> Flows.map (unaryFunction Core.literalFloat) (
            ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "adapter")) @@ var "fv")]) $
        "adapter" <<~ ref floatAdapterDef @@ var "ft" $
        "step" <~ ref AdaptUtils.bidirectionalDef @@ (var "adapt" @@ var "adapter") $
        produce (list [Compute.adapter (Compute.adapterIsLossy (var "adapter")) (var "t") (Core.literalTypeFloat (Compute.adapterTarget (var "adapter"))) (var "step")])) $
      "cx" <<~ ref Monads.getStateDef $
      "constraints" <~ Coders.languageConstraintsProjection (Coders.adapterContextLanguage (var "cx")) $
      "hasFloats" <~ Logic.not (Sets.null (Coders.languageConstraintsFloatTypes (var "constraints"))) $
      Logic.ifElse (var "hasFloats")
        (var "withFloats")
        (Flows.fail "no float types available")) $
  "forInteger" <~ ("t" ~> "it" ~>
      "withIntegers" <~ (
        "adapt" <~ ("adapter" ~> "dir" ~> "lit" ~> cases _Literal (var "lit")
          (Just (ref Monads.unexpectedDef
            @@ "integer literal"
            @@ (ref ShowCore.literalDef @@ var "lit"))) [
          _Literal_integer>>: "iv" ~> Flows.map (unaryFunction Core.literalInteger) (
            ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder (var "adapter")) @@ var "iv")]) $
        "adapter" <<~ ref integerAdapterDef @@ var "it" $
        "step" <~ ref AdaptUtils.bidirectionalDef @@ (var "adapt" @@ var "adapter") $
        produce (list [Compute.adapter (Compute.adapterIsLossy (var "adapter")) (var "t") (Core.literalTypeInteger (Compute.adapterTarget (var "adapter"))) (var "step")])) $
      "cx" <<~ ref Monads.getStateDef $
      "constraints" <~ Coders.languageConstraintsProjection (Coders.adapterContextLanguage (var "cx")) $
      "hasIntegers" <~ Logic.not (Sets.null (Coders.languageConstraintsIntegerTypes (var "constraints"))) $
      Logic.ifElse (var "hasIntegers")
        (var "withIntegers")
        (Flows.fail "no integer types available")) $
  "alts" <~ ("t" ~> cases _LiteralType (var "t")
    Nothing [
    _LiteralType_binary>>: constant $ var "forBinary" @@ var "t",
    _LiteralType_boolean>>: constant $ var "forBoolean" @@ var "t",
    _LiteralType_float>>: "ft" ~> var "forFloat" @@ var "t" @@ var "ft",
    _LiteralType_integer>>: "it" ~> var "forInteger" @@ var "t" @@ var "it",
    _LiteralType_string>>: constant (Flows.fail "no substitute for the literal string type")]) $
  "cx" <<~ ref Monads.getStateDef $
  "supported" <~ ref AdaptUtils.literalTypeIsSupportedDef @@ (Coders.languageConstraintsProjection (Coders.adapterContextLanguage (var "cx"))) $
  ref AdaptUtils.chooseAdapterDef
    @@ var "alts"
    @@ var "supported"
    @@ ref ShowCore.literalTypeDef
    @@ ref ShowCore.literalTypeDef
    @@ var "lt"
