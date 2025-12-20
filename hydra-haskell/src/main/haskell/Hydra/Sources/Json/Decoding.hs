
module Hydra.Sources.Json.Decoding where

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
import qualified Hydra.Sources.Kernel.Types.Json as Json


ns :: Namespace
ns = Namespace "hydra.ext.org.json.decoding"

module_ :: Module
module_ = Module ns elements
    []
    KernelTypes.kernelTypesNamespaces $
    Just "Decoding functions for JSON data"
  where
   elements = [
     Phantoms.toBinding decodeArray,
     Phantoms.toBinding decodeBoolean,
     Phantoms.toBinding decodeField,
--     Phantoms.toBinding decodeNumber, TODO: restore
     Phantoms.toBinding decodeObject,
     Phantoms.toBinding decodeOptionalField,
     Phantoms.toBinding decodeString]

define :: String -> TTerm a -> TBinding a
define label = definitionInModule module_ ("decode" <> label)

decodeArray :: TBinding ((Value -> Flow s a) -> Value -> Flow s [a])
decodeArray  = define "Array" $
  lambda "decodeElem" $ match _Value (Just $ Flows.fail (string "expected an array")) [
    _Value_array>>: lambda "a" $ Flows.mapList (var "decodeElem") $ var "a"]

decodeBoolean :: TBinding (Value -> Flow s Bool)
decodeBoolean  = define "Boolean" $
  match _Value (Just $ Flows.fail (string "expected a boolean")) [
    _Value_boolean>>: lambda "b" $ Flows.pure $ var "b"]

decodeField :: TBinding ((Value -> Flow s a) -> String -> (M.Map String Value) -> Flow s a)
decodeField  = define "Field" $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    Flows.bind
      (decodeOptionalField @@ var "decodeValue" @@ var "name" @@ var "m")
      (primitive _maybes_maybe
        @@ (Flows.fail $ Strings.cat2 (string "missing field: ") (var "name"))
        @@ (lambda "f" $ Flows.pure $ var "f"))

decodeNumber :: TBinding (Value -> Flow s Double)
decodeNumber  = define "Number" $
  match _Value (Just $ Flows.fail (string "expected a number")) [
    _Value_number>>: lambda "n" $ Flows.pure $ var "n"]

decodeObject :: TBinding (Value -> Flow s (M.Map String Value))
decodeObject  = define "Object" $
  match _Value (Just $ Flows.fail (string "expected an object")) [
    _Value_object>>: lambda "o" $ Flows.pure $ var "o"]

decodeOptionalField :: TBinding ((Value -> Flow s a) -> String -> (M.Map String Value) -> Flow s (Maybe a))
decodeOptionalField  = define "OptionalField" $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    (primitive _maybes_maybe
        @@ (Flows.pure nothing)
        @@ (lambda "v" (Flows.map (lambda "x" (just $ var "x")) (var "decodeValue" @@ var "v"))))
      @@ (Maps.lookup (var "name") (var "m"))

decodeString :: TBinding (Value -> Flow s String)
decodeString  = define "String" $
  match _Value (Just $ Flows.fail (string "expected a string")) [
    _Value_string>>: lambda "s" $ Flows.pure $ var "s"]
