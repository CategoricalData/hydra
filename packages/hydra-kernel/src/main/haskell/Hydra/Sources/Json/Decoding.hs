{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Json.Decoding where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import qualified Hydra.Dsl.Lib.Strings                as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                   as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                  as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core                       as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms                      as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants                   as Variants
import qualified Hydra.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular                         as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Overlay.Haskell.Dsl.Types                           as Types
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
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Graph     as PrintGraph
import qualified Hydra.Sources.Kernel.Terms.Print.Variants      as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing    as PrintTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Scientific                           as Sci
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Json.Model
import qualified Hydra.Sources.Json.Model as JsonModel
import qualified Hydra.Lib.Optionals as DefOptionals


ns :: ModuleName
ns = ModuleName "hydra.json.decoding"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> (KernelTypes.kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Decoding functions for JSON data")}
  where
   definitions = [
     Phantoms.toDefinition decodeArray,
     Phantoms.toDefinition decodeBoolean,
     Phantoms.toDefinition decodeField,
--     Phantoms.toDefinition decodeNumber, TODO: restore
     Phantoms.toDefinition decodeObject,
     Phantoms.toDefinition decodeOptionalField,
     Phantoms.toDefinition decodeString]

define :: String -> TypedTerm a -> TypedTermDefinition a
define label = definitionInModule module_ ("decode" <> label)

decodeArray :: TypedTermDefinition ((Value -> Either String a) -> Value -> Either String [a])
decodeArray  = define "Array" $
  doc "Decode a JSON array using a decoder for elements" $
  lambda "decodeElem" $ match _Value (Just $ left (string "expected an array")) [
    _Value_array>>: lambda "a" $ Eithers.mapList (var "decodeElem") $ var "a"]

decodeBoolean :: TypedTermDefinition (Value -> Either String Bool)
decodeBoolean  = define "Boolean" $
  doc "Decode a JSON boolean value" $
  match _Value (Just $ left (string "expected a boolean")) [
    _Value_boolean>>: lambda "b" $ right $ var "b"]

decodeField :: TypedTermDefinition ((Value -> Either String a) -> String -> (M.Map String Value) -> Either String a)
decodeField  = define "Field" $
  doc "Decode a required field from a JSON object" $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    Eithers.bind
      (decodeOptionalField @@ var "decodeValue" @@ var "name" @@ var "m")
      (lambda "mf" $ primitive DefOptionals.cases
        @@ var "mf"
        @@ (left $ Strings.cat2 (string "missing field: ") (var "name"))
        @@ (lambda "f" $ right $ var "f"))

decodeNumber :: TypedTermDefinition (Value -> Either String Sci.Scientific)
decodeNumber  = define "Number" $
  doc "Decode a JSON number value" $
  match _Value (Just $ left (string "expected a number")) [
    _Value_number>>: lambda "n" $ right $ var "n"]

decodeObject :: TypedTermDefinition (Value -> Either String (M.Map String Value))
decodeObject  = define "Object" $
  doc "Decode a JSON object value to a name-keyed map. Field order is not preserved; decoding looks fields up by name." $
  match _Value (Just $ left (string "expected an object")) [
    _Value_object>>: lambda "o" $ right $ (Maps.fromList (var "o") :: TypedTerm (M.Map String Value))]

decodeOptionalField :: TypedTermDefinition ((Value -> Either String a) -> String -> (M.Map String Value) -> Either String (Maybe a))
decodeOptionalField  = define "OptionalField" $
  doc "Decode an optional field from a JSON object" $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    primitive DefOptionals.cases
        @@ (Maps.lookup (var "name") (var "m" :: TypedTerm (M.Map String Value)))
        @@ (right nothing)
        @@ (lambda "v" (Eithers.map (lambda "x" (just $ var "x")) (var "decodeValue" @@ var "v")))

decodeString :: TypedTermDefinition (Value -> Either String String)
decodeString  = define "String" $
  doc "Decode a JSON string value" $
  match _Value (Just $ left (string "expected a string")) [
    _Value_string>>: lambda "s" $ right $ var "s"]
