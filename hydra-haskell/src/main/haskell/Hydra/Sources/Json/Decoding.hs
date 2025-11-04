{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Json.Decoding where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors                        as Accessors
import qualified Hydra.Dsl.Annotations                      as Anns
import qualified Hydra.Dsl.Ast                              as Ast
import qualified Hydra.Dsl.Coders                           as Coders
import qualified Hydra.Dsl.Compute                          as Compute
import qualified Hydra.Dsl.Core                             as Core
import qualified Hydra.Dsl.Grammar                          as Grammar
import qualified Hydra.Dsl.Graph                            as Graph
import qualified Hydra.Dsl.Json                             as Json
import qualified Hydra.Dsl.Lib.Chars                        as Chars
import qualified Hydra.Dsl.Lib.Equality                     as Equality
import qualified Hydra.Dsl.Lib.Flows                        as Flows
import qualified Hydra.Dsl.Lib.Lists                        as Lists
import qualified Hydra.Dsl.Lib.Literals                     as Literals
import qualified Hydra.Dsl.Lib.Logic                        as Logic
import qualified Hydra.Dsl.Lib.Maps                         as Maps
import qualified Hydra.Dsl.Lib.Math                         as Math
import qualified Hydra.Dsl.Lib.Maybes                    as Maybes
import qualified Hydra.Dsl.Lib.Sets                         as Sets
import           Hydra.Dsl.Lib.Strings                      as Strings
import qualified Hydra.Dsl.Mantle                           as Mantle
import qualified Hydra.Dsl.Module                           as Module
import           Hydra.Dsl.Phantoms                         as Phantoms
import qualified Hydra.Dsl.TTerms                           as TTerms
import qualified Hydra.Dsl.TTypes                           as TTypes
import qualified Hydra.Dsl.Tabular                          as Tabular
import qualified Hydra.Dsl.Terms                            as Terms
import qualified Hydra.Dsl.Topology                         as Topology
import qualified Hydra.Dsl.Types                            as Types
import qualified Hydra.Dsl.Typing                           as Typing
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Describe.Core   as DescribeCore
import qualified Hydra.Sources.Kernel.Terms.Describe.Mantle as DescribeMantle
import qualified Hydra.Sources.Kernel.Terms.Encode.Core     as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Mantle  as ExtractMantle
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Mantle     as ShowMantle
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import qualified Hydra.Sources.Kernel.Terms.Variants        as Variants
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y

import Hydra.Json
import qualified Hydra.Sources.Kernel.Types.Json as Json


module_ :: Module
module_ = Module (Namespace "hydra.ext.org.json.decoding") elements
    []
    KernelTypes.kernelTypesModules $
    Just "Decoding functions for JSON data"
  where
   elements = [
     Phantoms.el decodeArrayDef,
     Phantoms.el decodeBooleanDef,
     Phantoms.el decodeFieldDef,
--     Phantoms.el decodeNumberDef, TODO: restore
     Phantoms.el decodeObjectDef,
     Phantoms.el decodeOptionalFieldDef,
     Phantoms.el decodeStringDef]

define :: String -> TTerm a -> TBinding a
define label = definitionInModule module_ ("decode" <> label)

decodeArrayDef :: TBinding ((Value -> Flow s a) -> Value -> Flow s [a])
decodeArrayDef  = define "Array" $
  lambda "decodeElem" $ match _Value (Just $ Flows.fail "expected an array") [
    _Value_array>>: lambda "a" $ Flows.mapList (var "decodeElem") $ var "a"]

decodeBooleanDef :: TBinding (Value -> Flow s Bool)
decodeBooleanDef  = define "Boolean" $
  match _Value (Just $ Flows.fail $ "expected a boolean") [
    _Value_boolean>>: lambda "b" $ Flows.pure $ var "b"]

decodeFieldDef :: TBinding ((Value -> Flow s a) -> String -> (M.Map String Value) -> Flow s a)
decodeFieldDef  = define "Field" $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    Flows.bind
      (ref decodeOptionalFieldDef @@ var "decodeValue" @@ var "name" @@ var "m")
      (primitive _maybes_maybe
        @@ (Flows.fail $ Strings.cat2 "missing field: " (var "name"))
        @@ (lambda "f" $ Flows.pure $ var "f"))

decodeNumberDef :: TBinding (Value -> Flow s Double)
decodeNumberDef  = define "Number" $
  match _Value (Just $ Flows.fail "expected a number") [
    _Value_number>>: lambda "n" $ Flows.pure $ var "n"]

decodeObjectDef :: TBinding (Value -> Flow s (M.Map String Value))
decodeObjectDef  = define "Object" $
  match _Value (Just $ Flows.fail "expected an object") [
    _Value_object>>: lambda "o" $ Flows.pure $ var "o"]

decodeOptionalFieldDef :: TBinding ((Value -> Flow s a) -> String -> (M.Map String Value) -> Flow s (Maybe a))
decodeOptionalFieldDef  = define "OptionalField" $
  lambda "decodeValue" $ lambda "name" $ lambda "m" $
    (primitive _maybes_maybe
        @@ (Flows.pure nothing)
        @@ (lambda "v" (Flows.map (lambda "x" (just $ var "x")) (var "decodeValue" @@ var "v"))))
      @@ (Maps.lookup (var "name") (var "m"))

decodeStringDef :: TBinding (Value -> Flow s String)
decodeStringDef  = define "String" $
  match _Value (Just $ Flows.fail "expected a string") [
    _Value_string>>: lambda "s" $ Flows.pure $ var "s"]
