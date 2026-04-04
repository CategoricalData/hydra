
module Hydra.Sources.Json.Extract where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms hiding (opt)
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                  as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants      as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Json.Model


ns :: Namespace
ns = Namespace "hydra.extract.json"

module_ :: Module
module_ = Module ns elements
    []
    KernelTypes.kernelTypesNamespaces $
    Just "Utilities for extracting values from JSON objects"
  where
    elements = [
      toDefinition expectArray,
      toDefinition expectNumber,
      toDefinition expectObject,
      toDefinition expectString,
      toDefinition opt,
      toDefinition optArray,
      toDefinition optString,
      toDefinition require,
      toDefinition requireArray,
      toDefinition requireNumber,
      toDefinition requireString,
      toDefinition showValue]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

expectArray :: TTermDefinition (Value -> Either String [Value])
expectArray = define "expectArray" $
  doc "Extract an array from a JSON value, failing if the value is not an array" $
  lambda "value" $ cases _Value (var "value")
    (Just $ left $ Strings.cat2 (Strings.cat2 (string "expected ") (string "JSON array")) (Strings.cat2 (string " but found ") (showValue @@ var "value"))) [
    _Value_array>>: lambda "els" $ right $ var "els"]

expectNumber :: TTermDefinition (Value -> Either String Double)
expectNumber = define "expectNumber" $
  doc "Extract a number from a JSON value, failing if the value is not a number" $
  lambda "value" $ cases _Value (var "value")
    (Just $ left $ Strings.cat2 (Strings.cat2 (string "expected ") (string "JSON number")) (Strings.cat2 (string " but found ") (showValue @@ var "value"))) [
    _Value_number>>: lambda "d" $ right $ var "d"]

expectObject :: TTermDefinition (Value -> Either String (M.Map String Value))
expectObject = define "expectObject" $
  doc "Extract an object from a JSON value, failing if the value is not an object" $
  lambda "value" $ cases _Value (var "value")
    (Just $ left $ Strings.cat2 (Strings.cat2 (string "expected ") (string "JSON object")) (Strings.cat2 (string " but found ") (showValue @@ var "value"))) [
    _Value_object>>: lambda "m" $ right $ var "m"]

expectString :: TTermDefinition (Value -> Either String String)
expectString = define "expectString" $
  doc "Extract a string from a JSON value, failing if the value is not a string" $
  lambda "value" $ cases _Value (var "value")
    (Just $ left $ Strings.cat2 (Strings.cat2 (string "expected ") (string "JSON string")) (Strings.cat2 (string " but found ") (showValue @@ var "value"))) [
    _Value_string>>: lambda "s" $ right $ var "s"]

opt :: TTermDefinition (String -> M.Map String Value -> Maybe Value)
opt = define "opt" $
  doc "Look up an optional field in a JSON object" $
  lambdas ["fname", "m"] $ Maps.lookup (var "fname") (var "m")

optArray :: TTermDefinition (String -> M.Map String Value -> Either String (Maybe [Value]))
optArray = define "optArray" $
  doc "Look up an optional array field in a JSON object" $
  lambdas ["fname", "m"] $ Maybes.maybe
    (right nothing)
    (lambda "a" $ Eithers.map (lambda "x" (just $ var "x")) $ expectArray @@ var "a")
    (opt @@ var "fname" @@ var "m")

optString :: TTermDefinition (String -> M.Map String Value -> Either String (Maybe String))
optString = define "optString" $
  doc "Look up an optional string field in a JSON object" $
  lambdas ["fname", "m"] $ Maybes.maybe
    (right nothing)
    (lambda "s" $ Eithers.map (lambda "x" (just $ var "x")) $ expectString @@ var "s")
    (opt @@ var "fname" @@ var "m")

require :: TTermDefinition (String -> M.Map String Value -> Either String Value)
require = define "require" $
  doc "Look up a required field in a JSON object, failing if not found" $
  lambdas ["fname", "m"] $ Maybes.maybe
    (left $ Strings.cat $ list [
      string "required attribute ",
      showValue @@ var "fname",
      string " not found"])
    (lambda "value" $ right $ var "value")
    (Maps.lookup (var "fname") (var "m"))

requireArray :: TTermDefinition (String -> M.Map String Value -> Either String [Value])
requireArray = define "requireArray" $
  doc "Look up a required array field in a JSON object" $
  lambdas ["fname", "m"] $ Eithers.bind
    (require @@ var "fname" @@ var "m")
    (asTerm expectArray)

requireNumber :: TTermDefinition (String -> M.Map String Value -> Either String Double)
requireNumber = define "requireNumber" $
  doc "Look up a required number field in a JSON object" $
  lambdas ["fname", "m"] $ Eithers.bind
    (require @@ var "fname" @@ var "m")
    (asTerm expectNumber)

requireString :: TTermDefinition (String -> M.Map String Value -> Either String String)
requireString = define "requireString" $
  doc "Look up a required string field in a JSON object" $
  lambdas ["fname", "m"] $ Eithers.bind
    (require @@ var "fname" @@ var "m")
    (asTerm expectString)

-- TODO: implement this function, and deduplicate with hydra.json.coder.showValue
showValue :: TTermDefinition (Value -> String)
showValue = define "showValue" $
  doc "Show a JSON value as a string (placeholder implementation)" $
  lambda "value" $ string "TODO: implement showValue"
