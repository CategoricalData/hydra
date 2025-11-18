{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Json.Extract where

-- Standard imports for term-level sources outside of the kernel
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
import qualified Hydra.Sources.Kernel.Terms.All             as KernelTerms
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple    as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking        as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Encode.Core     as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util    as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect         as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta       as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y

-- Additional imports
import Hydra.Json
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads


module_ :: Module
module_ = Module (Namespace "hydra.extract.json") elements
    [Monads.module_]
    KernelTypes.kernelTypesModules $
    Just "Utilities for extracting values from JSON objects"
  where
    elements = [
      el expectArrayDef,
      el expectNumberDef,
      el expectObjectDef,
      el expectStringDef,
      el optDef,
      el optArrayDef,
      el optStringDef,
      el requireDef,
      el requireArrayDef,
      el requireNumberDef,
      el requireStringDef,
      el showValueDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

expectArrayDef :: TBinding (Value -> Flow s [Value])
expectArrayDef = define "expectArray" $
  doc "Extract an array from a JSON value, failing if the value is not an array" $
  lambda "value" $ cases _Value (var "value")
    (Just $ ref Monads.unexpectedDef @@ string "JSON array" @@ (ref showValueDef @@ var "value")) [
    _Value_array>>: lambda "els" $ Flows.pure $ var "els"]

expectNumberDef :: TBinding (Value -> Flow s Double)
expectNumberDef = define "expectNumber" $
  doc "Extract a number from a JSON value, failing if the value is not a number" $
  lambda "value" $ cases _Value (var "value")
    (Just $ ref Monads.unexpectedDef @@ string "JSON number" @@ (ref showValueDef @@ var "value")) [
    _Value_number>>: lambda "d" $ Flows.pure $ var "d"]

expectObjectDef :: TBinding (Value -> Flow s (M.Map String Value))
expectObjectDef = define "expectObject" $
  doc "Extract an object from a JSON value, failing if the value is not an object" $
  lambda "value" $ cases _Value (var "value")
    (Just $ ref Monads.unexpectedDef @@ string "JSON object" @@ (ref showValueDef @@ var "value")) [
    _Value_object>>: lambda "m" $ Flows.pure $ var "m"]

expectStringDef :: TBinding (Value -> Flow s String)
expectStringDef = define "expectString" $
  doc "Extract a string from a JSON value, failing if the value is not a string" $
  lambda "value" $ cases _Value (var "value")
    (Just $ ref Monads.unexpectedDef @@ string "JSON string" @@ (ref showValueDef @@ var "value")) [
    _Value_string>>: lambda "s" $ Flows.pure $ var "s"]

optDef :: TBinding (String -> M.Map String Value -> Maybe Value)
optDef = define "opt" $
  doc "Look up an optional field in a JSON object" $
  lambdas ["fname", "m"] $ Maps.lookup (var "fname") (var "m")

optArrayDef :: TBinding (String -> M.Map String Value -> Flow s (Maybe [Value]))
optArrayDef = define "optArray" $
  doc "Look up an optional array field in a JSON object" $
  lambdas ["fname", "m"] $ Maybes.maybe
    (Flows.pure nothing)
    (lambda "a" $ Flows.map (unaryFunction just) $ ref expectArrayDef @@ var "a")
    (ref optDef @@ var "fname" @@ var "m")

optStringDef :: TBinding (String -> M.Map String Value -> Flow s (Maybe String))
optStringDef = define "optString" $
  doc "Look up an optional string field in a JSON object" $
  lambdas ["fname", "m"] $ Maybes.maybe
    (Flows.pure nothing)
    (lambda "s" $ Flows.map (unaryFunction just) $ ref expectStringDef @@ var "s")
    (ref optDef @@ var "fname" @@ var "m")

requireDef :: TBinding (String -> M.Map String Value -> Flow s Value)
requireDef = define "require" $
  doc "Look up a required field in a JSON object, failing if not found" $
  lambdas ["fname", "m"] $ Maybes.maybe
    (Flows.fail $ Strings.cat $ list [
      string "required attribute ",
      ref showValueDef @@ var "fname",
      string " not found"])
    (lambda "value" $ Flows.pure $ var "value")
    (Maps.lookup (var "fname") (var "m"))

requireArrayDef :: TBinding (String -> M.Map String Value -> Flow s [Value])
requireArrayDef = define "requireArray" $
  doc "Look up a required array field in a JSON object" $
  lambdas ["fname", "m"] $ Flows.bind
    (ref requireDef @@ var "fname" @@ var "m")
    (ref expectArrayDef)

requireNumberDef :: TBinding (String -> M.Map String Value -> Flow s Double)
requireNumberDef = define "requireNumber" $
  doc "Look up a required number field in a JSON object" $
  lambdas ["fname", "m"] $ Flows.bind
    (ref requireDef @@ var "fname" @@ var "m")
    (ref expectNumberDef)

requireStringDef :: TBinding (String -> M.Map String Value -> Flow s String)
requireStringDef = define "requireString" $
  doc "Look up a required string field in a JSON object" $
  lambdas ["fname", "m"] $ Flows.bind
    (ref requireDef @@ var "fname" @@ var "m")
    (ref expectStringDef)

-- TODO: implement this function, and deduplicate with hydra.json.coder.showValue
showValueDef :: TBinding (Value -> String)
showValueDef = define "showValue" $
  doc "Show a JSON value as a string (placeholder implementation)" $
  lambda "value" $ string "TODO: implement showValue"
