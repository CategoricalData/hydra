{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Templates where

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
import qualified Hydra.Dsl.Json          as Json
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

import qualified Hydra.Sources.Kernel.Terms.Decode.Core as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


module_ :: Module
module_ = Module (Namespace "hydra.templates") elements
    [DecodeCore.module_, ShowCore.module_]
    kernelTypesModules $
    Just "A utility which instantiates a nonrecursive type with default values"
  where
   elements = [
     el graphToSchemaDef,
     el instantiateTemplateDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

graphToSchemaDef :: TElement (Graph -> Flow Graph (M.Map Name Type))
graphToSchemaDef = define "graphToSchema" $
  doc "Create a graph schema from a graph which contains nothing but encoded type definitions" $
  lambda "g" $ lets [
    "toPair">: lambda "nameAndEl" $ lets [
      "name">: first $ var "nameAndEl",
      "el">: second $ var "nameAndEl"]
      $ Flows.bind (ref DecodeCore.typeDef @@ (Graph.elementTerm $ var "el")) $
        lambda "t" $ Flows.pure $ pair (var "name") (var "t")]
    $ Flows.bind (Flows.mapList (var "toPair") $ Maps.toList $ Graph.graphElements $ var "g") $
      lambda "pairs" $ Flows.pure $ Maps.fromList $ var "pairs"

instantiateTemplateDef :: TElement (Bool -> M.Map Name Type -> Type -> Flow s Term)
instantiateTemplateDef = define "instantiateTemplate" $
  doc ("Given a graph schema and a nonrecursive type, instantiate it with default values."
    <> " If the minimal flag is set, the smallest possible term is produced; otherwise, exactly one subterm"
    <> " is produced for constructors which do not otherwise require one, e.g. in lists and optionals") $
  lambdas ["minimal", "schema", "t"] $ lets [
    "inst">: ref instantiateTemplateDef @@ var "minimal" @@ var "schema",
    "noPoly">: Flows.fail $ string "Polymorphic and function types are not currently supported"]
    $ match _Type Nothing [
      _Type_annotated>>: lambda "at" $ var "inst" @@ (Core.annotatedTypeSubject $ var "at"),
      _Type_application>>: constant $ var "noPoly",
      _Type_function>>: constant $ var "noPoly",
      _Type_forall>>: constant $ var "noPoly",
      _Type_list>>: lambda "et" $ Logic.ifElse (var "minimal")
        (Flows.pure $ Core.termList $ list [])
        (Flows.bind (var "inst" @@ var "et") $
          lambda "e" $ Flows.pure $ Core.termList $ list [var "e"]),
      _Type_literal>>: lambda "lt" $ Flows.pure $ Core.termLiteral $
        cases _LiteralType (var "lt") Nothing [
          _LiteralType_binary>>: constant $ Core.literalString $ string "",
          _LiteralType_boolean>>: constant $ Core.literalBoolean false,
          _LiteralType_integer>>: lambda "it" $ Core.literalInteger $
            match _IntegerType Nothing [
              _IntegerType_bigint>>: constant $ Core.integerValueBigint $ bigint 0,
              _IntegerType_int8>>: constant $ Core.integerValueInt8 $ int8 0,
              _IntegerType_int16>>: constant $ Core.integerValueInt16 $ int16 0,
              _IntegerType_int32>>: constant $ Core.integerValueInt32 $ int32 0,
              _IntegerType_int64>>: constant $ Core.integerValueInt64 $ int64 0,
              _IntegerType_uint8>>: constant $ Core.integerValueUint8 $ uint8 0,
              _IntegerType_uint16>>: constant $ Core.integerValueUint16 $ uint16 0,
              _IntegerType_uint32>>: constant $ Core.integerValueUint32 $ uint32 0,
              _IntegerType_uint64>>: constant $ Core.integerValueUint64 $ uint64 0] @@ var "it",
          _LiteralType_float>>: lambda "ft" $ Core.literalFloat $
            cases _FloatType (var "ft") Nothing [
              _FloatType_bigfloat>>: constant $ Core.floatValueBigfloat $ bigfloat 0.0,
              _FloatType_float32>>: constant $ Core.floatValueFloat32 $ float32 0.0,
              _FloatType_float64>>: constant $ Core.floatValueFloat64 $ float64 0.0],
          _LiteralType_string>>: constant $ Core.literalString $ string ""],
      _Type_map>>: lambda "mt" $ lets [
        "kt">: Core.mapTypeKeys $ var "mt",
        "vt">: Core.mapTypeValues $ var "mt"]
        $ Logic.ifElse (var "minimal")
          (Flows.pure $ Core.termMap Maps.empty)
          (Flows.bind (var "inst" @@ var "kt") $
            lambda "ke" $
              Flows.bind (var "inst" @@ var "vt") $
                lambda "ve" $ Flows.pure $ Core.termMap $ Maps.singleton (var "ke") (var "ve")),
      _Type_optional>>: lambda "ot" $ Logic.ifElse (var "minimal")
        (Flows.pure $ Core.termOptional nothing)
        (Flows.bind (var "inst" @@ var "ot") $
          lambda "e" $ Flows.pure $ Core.termOptional $ just $ var "e"),
      _Type_product>>: lambda "types" $
        Flows.bind (Flows.mapList (var "inst") (var "types")) $
          lambda "es" $ Flows.pure $ Core.termProduct $ var "es",
      _Type_record>>: lambda "rt" $ lets [
        "tname">: Core.rowTypeTypeName $ var "rt",
        "fields">: Core.rowTypeFields $ var "rt",
        "toField">: lambda "ft" $
          Flows.bind (var "inst" @@ (Core.fieldTypeType $ var "ft")) $
            lambda "e" $ Flows.pure $ Core.field (Core.fieldTypeName $ var "ft") (var "e")]
        $ Flows.bind (Flows.mapList (var "toField") (var "fields")) $
          lambda "dfields" $ Flows.pure $ Core.termRecord $ Core.record (var "tname") (var "dfields"),
      _Type_set>>: lambda "et" $ Logic.ifElse (var "minimal")
        (Flows.pure $ Core.termSet Sets.empty)
        (Flows.bind (var "inst" @@ var "et") $
          lambda "e" $ Flows.pure $ Core.termSet $ Sets.fromList $ list [var "e"]),
      -- TODO: _Type_sum
      -- TODO: _Type_union
      -- TODO: _Type_unit>>: constant $ Flows.pure Core.termUnit,
      _Type_variable>>: lambda "tname" $
        Optionals.maybe
          (Flows.fail $ Strings.cat2 (string "Type variable ") $ Strings.cat2 (ref ShowCore.termDef @@ (Core.termVariable $ var "tname")) (string " not found in schema"))
          (var "inst")
          (Maps.lookup (var "tname") (var "schema")),
      _Type_wrap>>: lambda "wt" $ lets [
        "tname">: Core.wrappedTypeTypeName $ var "wt",
        "t'">: Core.wrappedTypeObject $ var "wt"]
        $ Flows.bind (var "inst" @@ var "t'") $
          lambda "e" $ Flows.pure $ Core.termWrap $ Core.wrappedTerm (var "tname") (var "e")] @@ var "t"

{-

-- Example of type-to-term instantiation which creates a YAML-based template out of the OpenCypher feature model.

import Hydra.Staging.Yaml.Model as Yaml
import Hydra.Monads
import Data.Map as M
import Data.Maybe as Y

ff = flowToIo bootstrapGraph

schema <- ff $ graphToSchema $ modulesToGraph [openCypherFeaturesModule]

typ <- ff $ inlineType schema $ Y.fromJust $ M.lookup _CypherFeatures schema
term <- ff $ insantiateTemplate False schema typ

encoder <- ff (coderEncode <$> yamlCoder typ)
yaml <- ff $ encoder term
putStrLn $ hydraYamlToString yaml

-}
