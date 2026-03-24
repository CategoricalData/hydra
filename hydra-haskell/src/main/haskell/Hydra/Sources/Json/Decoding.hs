
module Hydra.Sources.Json.Decoding where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
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
import qualified Hydra.Dsl.Module                     as Module
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
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
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
import qualified Hydra.Sources.Json.Model as JsonModel


ns :: Namespace
ns = Namespace "hydra.ext.org.json.decoding"

module_ :: Module
module_ = Module ns elements
    []
    KernelTypes.kernelTypesNamespaces $
    Just "Decoding functions for JSON data"
  where
   elements = [
     Phantoms.toTermDefinition decodeArray,
     Phantoms.toTermDefinition decodeBoolean,
     Phantoms.toTermDefinition decodeField,
--     Phantoms.toTermDefinition decodeNumber, TODO: restore
     Phantoms.toTermDefinition decodeObject,
     Phantoms.toTermDefinition decodeOptionalField,
     Phantoms.toTermDefinition decodeString]

define :: String -> TTerm a -> TBinding a
define label = definitionInModule module_ ("decode" <> label)

decodeArray :: TBinding ((Value -> Either String a) -> Value -> Either String [a])
decodeArray  = define "Array" $
  doc "Decode a JSON array using a decoder for elements" $
  lambda "decodeElem" $ match _Value (Just $ left (string "expected an array")) [
    _Value_array>>: lambda "a" $ Eithers.mapList (var "decodeElem") $ var "a"]

decodeBoolean :: TBinding (Value -> Either String Bool)
decodeBoolean  = define "Boolean" $
  doc "Decode a JSON boolean value" $
  match _Value (Just $ left (string "expected a boolean")) [
    _Value_boolean>>: lambda "b" $ right $ var "b"]

decodeField :: TBinding ((Value -> Either String a) -> String -> (M.Map String Value) -> Either String a)
decodeField  = define "Field" $
  doc "Decode a required field from a JSON object" $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    Eithers.bind
      (decodeOptionalField @@ var "decodeValue" @@ var "name" @@ var "m")
      (primitive _maybes_maybe
        @@ (left $ Strings.cat2 (string "missing field: ") (var "name"))
        @@ (lambda "f" $ right $ var "f"))

decodeNumber :: TBinding (Value -> Either String Double)
decodeNumber  = define "Number" $
  doc "Decode a JSON number value" $
  match _Value (Just $ left (string "expected a number")) [
    _Value_number>>: lambda "n" $ right $ var "n"]

decodeObject :: TBinding (Value -> Either String (M.Map String Value))
decodeObject  = define "Object" $
  doc "Decode a JSON object value" $
  match _Value (Just $ left (string "expected an object")) [
    _Value_object>>: lambda "o" $ right $ var "o"]

decodeOptionalField :: TBinding ((Value -> Either String a) -> String -> (M.Map String Value) -> Either String (Maybe a))
decodeOptionalField  = define "OptionalField" $
  doc "Decode an optional field from a JSON object" $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    (primitive _maybes_maybe
        @@ (right nothing)
        @@ (lambda "v" (Eithers.map (lambda "x" (just $ var "x")) (var "decodeValue" @@ var "v"))))
      @@ (Maps.lookup (var "name") (var "m"))

decodeString :: TBinding (Value -> Either String String)
decodeString  = define "String" $
  doc "Decode a JSON string value" $
  match _Value (Just $ left (string "expected a string")) [
    _Value_string>>: lambda "s" $ right $ var "s"]
