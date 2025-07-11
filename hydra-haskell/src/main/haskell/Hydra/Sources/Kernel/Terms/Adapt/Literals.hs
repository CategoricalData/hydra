{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Adapt.Literals where

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
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
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

import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Describe.Core as DescribeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Variants as Variants


module_ :: Module
module_ = Module (Namespace "hydra.adapt.literals") elements
    [ExtractCore.module_, Monads.module_, DescribeCore.module_, AdaptUtils.module_, ShowCore.module_, Variants.module_]
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

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

comparePrecisionDef :: TElement (Precision -> Precision -> Comparison)
comparePrecisionDef = define "comparePrecision" $
  doc "Compare two precision values" $
  lambdas ["p1", "p2"] $
    cases _Precision (var "p1") Nothing [
      _Precision_arbitrary>>: constant $
        cases _Precision (var "p2") Nothing [
          _Precision_arbitrary>>: constant Graph.comparisonEqualTo,
          _Precision_bits>>: constant Graph.comparisonGreaterThan],
      _Precision_bits>>: lambda "b1" $
        cases _Precision (var "p2") Nothing [
          _Precision_arbitrary>>: constant Graph.comparisonLessThan,
          _Precision_bits>>: lambda "b2" $
            Logic.ifElse (Equality.lt (var "b1") (var "b2"))
              Graph.comparisonLessThan
              Graph.comparisonGreaterThan]]

convertFloatValueDef :: TElement (FloatType -> FloatValue -> FloatValue)
convertFloatValueDef = define "convertFloatValue" $
  doc "Convert a float value to a different float type" $
  lambdas ["target", "fv"] $ lets [
    "decoder">: lambda "fv" $
      cases _FloatValue (var "fv") Nothing [
        _FloatValue_bigfloat>>: lambda "d" $ var "d",
        _FloatValue_float32>>: lambda "f" $ Literals.float32ToBigfloat $ var "f",
        _FloatValue_float64>>: lambda "d" $ Literals.float64ToBigfloat $ var "d"],
    "encoder">: lambda "d" $
      cases _FloatType (var "target") Nothing [
        _FloatType_bigfloat>>: constant $ Core.floatValueBigfloat $ var "d",
        _FloatType_float32>>: constant $ Core.floatValueFloat32 $ Literals.bigfloatToFloat32 $ var "d",
        _FloatType_float64>>: constant $ Core.floatValueFloat64 $ Literals.bigfloatToFloat64 $ var "d"]]
    $ var "encoder" @@ (var "decoder" @@ var "fv")

convertIntegerValueDef :: TElement (IntegerType -> IntegerValue -> IntegerValue)
convertIntegerValueDef = define "convertIntegerValue" $
  doc "Convert an integer value to a different integer type" $
  lambdas ["target", "iv"] $ lets [
    "decoder">: lambda "iv" $
      cases _IntegerValue (var "iv") Nothing [
        _IntegerValue_bigint>>: lambda "v" $ var "v",
        _IntegerValue_int8>>: lambda "v" $ Literals.int8ToBigint $ var "v",
        _IntegerValue_int16>>: lambda "v" $ Literals.int16ToBigint $ var "v",
        _IntegerValue_int32>>: lambda "v" $ Literals.int32ToBigint $ var "v",
        _IntegerValue_int64>>: lambda "v" $ Literals.int64ToBigint $ var "v",
        _IntegerValue_uint8>>: lambda "v" $ Literals.uint8ToBigint $ var "v",
        _IntegerValue_uint16>>: lambda "v" $ Literals.uint16ToBigint $ var "v",
        _IntegerValue_uint32>>: lambda "v" $ Literals.uint32ToBigint $ var "v",
        _IntegerValue_uint64>>: lambda "v" $ Literals.uint64ToBigint $ var "v"],
    "encoder">: lambda "d" $
      cases _IntegerType (var "target") Nothing [
        _IntegerType_bigint>>: constant $ Core.integerValueBigint $ var "d",
        _IntegerType_int8>>: constant $ Core.integerValueInt8 $ Literals.bigintToInt8 $ var "d",
        _IntegerType_int16>>: constant $ Core.integerValueInt16 $ Literals.bigintToInt16 $ var "d",
        _IntegerType_int32>>: constant $ Core.integerValueInt32 $ Literals.bigintToInt32 $ var "d",
        _IntegerType_int64>>: constant $ Core.integerValueInt64 $ Literals.bigintToInt64 $ var "d",
        _IntegerType_uint8>>: constant $ Core.integerValueUint8 $ Literals.bigintToUint8 $ var "d",
        _IntegerType_uint16>>: constant $ Core.integerValueUint16 $ Literals.bigintToUint16 $ var "d",
        _IntegerType_uint32>>: constant $ Core.integerValueUint32 $ Literals.bigintToUint32 $ var "d",
        _IntegerType_uint64>>: constant $ Core.integerValueUint64 $ Literals.bigintToUint64 $ var "d"]]
    $ var "encoder" @@ (var "decoder" @@ var "iv")

disclaimerDef :: TElement (Bool -> String -> String -> String)
disclaimerDef = define "disclaimer" $
  doc "Generate a disclaimer message for type conversions" $
  lambdas ["lossy", "source", "target"] $
    Strings.cat $ list [
      string "replace ",
      var "source",
      string " with ",
      var "target",
      Logic.ifElse (var "lossy") (string " (lossy)") (string "")]

literalAdapterDef :: TElement (LiteralType -> Flow AdapterContext (SymmetricAdapter s LiteralType Literal))
literalAdapterDef = define "literalAdapter" $
  doc "Create an adapter for literal types" $
  lambda "lt" $ lets [
    "alts">: lambda "t" $ cases _LiteralType (var "t") Nothing [
      _LiteralType_binary>>: constant $ lets [
        "step">: Compute.coder
          (lambda "lit" $ cases _Literal (var "lit") Nothing [
            _Literal_binary>>: lambda "b" $ Flows.pure $ Core.literalString $ Literals.binaryToString $ var "b"])
          (lambda "lit" $ cases _Literal (var "lit") Nothing [
            _Literal_string>>: lambda "s" $ Flows.pure $ Core.literalBinary $ Literals.stringToBinary $ var "s"])] $
        Flows.pure $ list [Compute.adapter false (var "t") Core.literalTypeString (var "step")],
      _LiteralType_boolean>>: constant $
        bind "cx" (ref Monads.getStateDef) $ lets [
        "constraints">: Coders.languageConstraints $ Coders.adapterContextLanguage $ var "cx",
        "hasIntegers">: Logic.not $ Sets.null $ Coders.languageConstraintsIntegerTypes $ var "constraints",
        "hasStrings">: Sets.member Mantle.literalVariantString (Coders.languageConstraintsLiteralVariants $ var "constraints")] $
        Logic.ifElse (var "hasIntegers")
          (bind "adapter" (ref integerAdapterDef @@ Core.integerTypeUint8) $ lets [
            "step'">: Compute.adapterCoder $ var "adapter",
            "step">: Compute.coder
              (lambda "lit" $ cases _Literal (var "lit") Nothing [
                _Literal_boolean>>: lambda "bv" $ Flows.bind
                  (Compute.coderEncode (var "step'") @@ (Core.integerValueUint8 $ Logic.ifElse (var "bv") (uint8 1) (uint8 0)))
                  (lambda "iv" $ Flows.pure $ Core.literalInteger $ var "iv")])
              (lambda "lit" $ cases _Literal (var "lit") Nothing [
                _Literal_integer>>: lambda "iv" $ Flows.bind
                  (Compute.coderDecode (var "step'") @@ var "iv")
                  (lambda "val" $ cases _IntegerValue (var "val") Nothing [
                    _IntegerValue_uint8>>: lambda "v" $ Flows.pure $ Core.literalBoolean $ Equality.equal (var "v") (uint8 1)])])] $
            Flows.pure $ list [Compute.adapter false (var "t") (Core.literalTypeInteger $ Compute.adapterTarget $ var "adapter") (var "step")])
          (Logic.ifElse (var "hasStrings")
            (Flows.pure $ lets [
              "encode">: lambda "lit" $
                bind "b" (ref ExtractCore.booleanLiteralDef @@ var "lit") $
                Flows.pure $ Core.literalString $ Logic.ifElse (var "b") "true" "false",
              "decode">: lambda "lit" $
                bind "s" (ref ExtractCore.stringLiteralDef @@ var "lit") $
                Logic.ifElse (Equality.equal (var "s") (string "true"))
                  (Flows.pure $ Core.literalBoolean true)
                  (Logic.ifElse (Equality.equal (var "s") (string "false"))
                    (Flows.pure $ Core.literalBoolean false)
                    (ref Monads.unexpectedDef @@ "boolean literal" @@ var "s"))] $
              list [Compute.adapter false (var "t") Core.literalTypeString (Compute.coder (var "encode") (var "decode"))])
            (Flows.fail $ string "no alternatives available for boolean encoding")),
      _LiteralType_float>>: lambda "ft" $
        Flows.bind (ref Monads.getStateDef) $ lambda "cx" $ lets [
          "constraints">: Coders.languageConstraints $ Coders.adapterContextLanguage $ var "cx",
          "hasFloats">: Logic.not $ Sets.null $ Coders.languageConstraintsFloatTypes $ var "constraints"] $
        Logic.ifElse (var "hasFloats")
          (Flows.bind (ref floatAdapterDef @@ var "ft") $ lambda "adapter" $ lets [
            "step">: ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "l"] $
              cases _Literal (var "l") (Just $ ref Monads.unexpectedDef @@ string "floating-point literal" @@ (ref ShowCore.literalDef @@ var "l")) [
                _Literal_float>>: lambda "fv" $ Flows.map (unaryFunction Core.literalFloat) $
                  ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "adapter") @@ var "fv"])] $
            Flows.pure $ list [Compute.adapter (Compute.adapterIsLossy $ var "adapter") (var "t") (Core.literalTypeFloat $ Compute.adapterTarget $ var "adapter") (var "step")])
          (Flows.fail $ string "no float types available"),
      _LiteralType_integer>>: lambda "it" $
        Flows.bind (ref Monads.getStateDef) $ lambda "cx" $ lets [
          "constraints">: Coders.languageConstraints $ Coders.adapterContextLanguage $ var "cx",
          "hasIntegers">: Logic.not $ Sets.null $ Coders.languageConstraintsIntegerTypes $ var "constraints"] $
        Logic.ifElse (var "hasIntegers")
          (Flows.bind (ref integerAdapterDef @@ var "it") $ lambda "adapter" $ lets [
            "step">: ref AdaptUtils.bidirectionalDef @@ (lambdas ["dir", "lit"] $
              cases _Literal (var "lit") (Just $ ref Monads.unexpectedDef @@ string "integer literal" @@ (ref ShowCore.literalDef @@ var "lit")) [
                _Literal_integer>>: lambda "iv" $ Flows.map (unaryFunction Core.literalInteger) $
                  ref AdaptUtils.encodeDecodeDef @@ var "dir" @@ (Compute.adapterCoder $ var "adapter") @@ var "iv"])] $
            Flows.pure $ list [Compute.adapter (Compute.adapterIsLossy $ var "adapter") (var "t") (Core.literalTypeInteger $ Compute.adapterTarget $ var "adapter") (var "step")])
          (Flows.fail $ string "no integer types available"),
      _LiteralType_string>>: constant $ Flows.fail $ string "no substitute for the literal string type"]] $
  Flows.bind (ref Monads.getStateDef) $ lambda "cx" $ lets [
    "supported">: ref AdaptUtils.literalTypeIsSupportedDef @@ (Coders.languageConstraints $ Coders.adapterContextLanguage $ var "cx")] $
  ref AdaptUtils.chooseAdapterDef
    @@ var "alts"
    @@ var "supported"
    @@ ref ShowCore.literalTypeDef
    @@ ref DescribeCore.literalTypeDef
    @@ var "lt"

floatAdapterDef :: TElement (FloatType -> Flow AdapterContext (SymmetricAdapter s FloatType FloatValue))
floatAdapterDef = define "floatAdapter" $
  doc "Create an adapter for float types" $
  lambda "ft" $ lets [
      "alts">: lambda "t" $ Flows.mapList (var "makeAdapter" @@ var "t") $ cases _FloatType (var "t") Nothing [
        _FloatType_bigfloat>>: constant $ list [Core.floatTypeFloat64, Core.floatTypeFloat32],
        _FloatType_float32>>: constant $ list [Core.floatTypeFloat64, Core.floatTypeBigfloat],
        _FloatType_float64>>: constant $ list [Core.floatTypeBigfloat, Core.floatTypeFloat32]],
      "makeAdapter">: lambdas ["source", "target"] $ lets [
          "lossy">: Equality.equal
            (ref comparePrecisionDef
              @@ (ref Variants.floatTypePrecisionDef @@ var "source")
              @@ (ref Variants.floatTypePrecisionDef @@ var "target"))
            Graph.comparisonGreaterThan,
          "step">: Compute.coder
            (lambda "fv" $ Flows.pure $ ref convertFloatValueDef @@ var "target" @@ var "fv")
            (lambda "fv" $ Flows.pure $ ref convertFloatValueDef @@ var "source" @@ var "fv"),
          "msg">: ref disclaimerDef
            @@ var "lossy"
            @@ (ref DescribeCore.floatTypeDef @@ var "source")
            @@ (ref DescribeCore.floatTypeDef @@ var "target")] $
        ref Monads.warnDef
          @@ var "msg"
          @@ (Flows.pure (Compute.adapter (var "lossy") (var "source") (var "target") (var "step")))] $
    Flows.bind (ref Monads.getStateDef) $ lambda "cx" $
      lets [
        "supported">: ref AdaptUtils.floatTypeIsSupportedDef
          @@ (Coders.languageConstraints $ Coders.adapterContextLanguage $ var "cx")] $
      ref AdaptUtils.chooseAdapterDef
        @@ var "alts"
        @@ var "supported"
        @@ ref ShowCore.floatTypeDef
        @@ ref DescribeCore.floatTypeDef
        @@ var "ft"

integerAdapterDef :: TElement (IntegerType -> Flow AdapterContext (SymmetricAdapter s IntegerType IntegerValue))
integerAdapterDef = define "integerAdapter" $
  doc "Create an adapter for integer types" $
  lambda "it" $ lets [
    "interleave">: lambdas ["xs", "ys"] $ Lists.concat $ Lists.transpose $ list [var "xs", var "ys"],
    "signedOrdered">: Lists.filter
      (lambda "v" $ Logic.and
        (ref Variants.integerTypeIsSignedDef @@ var "v")
        (Logic.not $ Equality.equal (ref Variants.integerTypePrecisionDef @@ var "v") Mantle.precisionArbitrary))
      (ref Variants.integerTypesDef),
    "unsignedOrdered">: Lists.filter
      (lambda "v" $ Logic.and
        (Logic.not $ ref Variants.integerTypeIsSignedDef @@ var "v")
        (Logic.not $ Equality.equal (ref Variants.integerTypePrecisionDef @@ var "v") Mantle.precisionArbitrary))
      (ref Variants.integerTypesDef),
    "signedPref">: var "interleave" @@ var "signedOrdered" @@ var "unsignedOrdered",
    "unsignedPref">: var "interleave" @@ var "unsignedOrdered" @@ var "signedOrdered",
    "signedNonPref">: Lists.reverse $ var "unsignedPref",
    "unsignedNonPref">: Lists.reverse $ var "signedPref",
    "signed">: lambda "i" $ Lists.concat $ list [
      Lists.drop (Math.mul (var "i") (int32 2)) (var "signedPref"),
      list [Core.integerTypeBigint],
      Lists.drop (Math.add (Math.sub (int32 8) (Math.mul (var "i") (int32 2))) (int32 1)) (var "signedNonPref")],
    "unsigned">: lambda "i" $ Lists.concat $ list [
      Lists.drop (Math.mul (var "i") (int32 2)) (var "unsignedPref"),
      list [Core.integerTypeBigint],
      Lists.drop (Math.add (Math.sub (int32 8) (Math.mul (var "i") (int32 2))) (int32 1)) (var "unsignedNonPref")],
    "alts">: lambda "t" $ Flows.mapList (var "makeAdapter" @@ var "t") $ cases _IntegerType (var "t") Nothing [
      _IntegerType_bigint>>: constant $ Lists.reverse $ var "unsignedPref",
      _IntegerType_int8>>: constant $ var "signed" @@ int32 1,
      _IntegerType_int16>>: constant $ var "signed" @@ int32 2,
      _IntegerType_int32>>: constant $ var "signed" @@ int32 3,
      _IntegerType_int64>>: constant $ var "signed" @@ int32 4,
      _IntegerType_uint8>>: constant $ var "unsigned" @@ int32 1,
      _IntegerType_uint16>>: constant $ var "unsigned" @@ int32 2,
      _IntegerType_uint32>>: constant $ var "unsigned" @@ int32 3,
      _IntegerType_uint64>>: constant $ var "unsigned" @@ int32 4],
    "makeAdapter">: lambdas ["source", "target"] $ lets [
      "lossy">: Logic.not $ Equality.equal
        (ref comparePrecisionDef
          @@ (ref Variants.integerTypePrecisionDef @@ var "source")
          @@ (ref Variants.integerTypePrecisionDef @@ var "target"))
        Graph.comparisonLessThan,
      "step">: Compute.coder
        (lambda "iv" $ Flows.pure $ ref convertIntegerValueDef @@ var "target" @@ var "iv")
        (lambda "iv" $ Flows.pure $ ref convertIntegerValueDef @@ var "source" @@ var "iv"),
      "msg">: ref disclaimerDef
        @@ var "lossy"
        @@ (ref DescribeCore.integerTypeDef @@ var "source")
        @@ (ref DescribeCore.integerTypeDef @@ var "target")] $
      ref Monads.warnDef
        @@ var "msg"
        @@ (Flows.pure $ Compute.adapter (var "lossy") (var "source") (var "target") (var "step"))] $
  Flows.bind (ref Monads.getStateDef) $ lambda "cx" $
    lets [
      "supported">: ref AdaptUtils.integerTypeIsSupportedDef
        @@ (Coders.languageConstraints $ Coders.adapterContextLanguage $ var "cx")] $
    ref AdaptUtils.chooseAdapterDef
      @@ var "alts"
      @@ var "supported"
      @@ ref ShowCore.integerTypeDef
      @@ ref DescribeCore.integerTypeDef
      @@ var "it"
