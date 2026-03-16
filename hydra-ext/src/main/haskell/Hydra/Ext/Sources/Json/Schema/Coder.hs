module Hydra.Ext.Sources.Json.Schema.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
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
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
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
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
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
import qualified Hydra.Ext.Org.Json.Schema as JS
import qualified Hydra.Json.Model as JM
import qualified Hydra.Ext.Sources.Json.Schema as JsonSchema
import qualified Hydra.Ext.Sources.Json.Schema.Serde as JsonSchemaSerde
import qualified Hydra.Sources.CoderUtils as CoderUtils

-- Phantom type for JsonSchemaOptions (was previously in Staging module)
data JsonSchemaOptions


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

jsonSchemaPhantomNs :: Namespace
jsonSchemaPhantomNs = Namespace "hydra.ext.org.json.schema"

jsonModelNs :: Namespace
jsonModelNs = Namespace "hydra.json.model"

ns :: Namespace
ns = Namespace "hydra.ext.json.schema.coder"

module_ :: Module
module_ = Module ns elements
    [Formatting.ns, Names.ns, Rewriting.ns, Annotations.ns, Constants.ns, Schemas.ns, Reflect.ns, JsonSchemaSerde.ns, moduleNamespace CoderUtils.module_]
    (moduleNamespace JsonSchema.module_:jsonModelNs:KernelTypes.kernelTypesNamespaces) $
    Just "JSON Schema code generator: converts Hydra modules to JSON Schema documents"
  where
    elements = [
      toBinding moduleToJsonSchema,
      toBinding constructModule,
      toBinding encodeField,
      toBinding encodeName,
      toBinding encodeNamedType,
      toBinding encodeType,
      toBinding isRequiredField,
      toBinding referenceRestriction]


-- | Result type alias
type Result a = Either (InContext Error) a


moduleToJsonSchema :: TBinding (JsonSchemaOptions -> Module -> [Definition] -> Context -> Graph -> Result (M.Map FilePath String))
moduleToJsonSchema = define "moduleToJsonSchema" $
  doc "Convert a Hydra module to a map of JSON Schema documents" $
  lambda "opts" $ lambda "mod" $ lambda "defs" $ lambda "cx" $ lambda "g" $
    var "hydra.ext.json.schema.coder.moduleToJsonSchema" @@ var "opts" @@ var "mod" @@ var "defs" @@ var "cx" @@ var "g"

constructModule :: TBinding (Context -> Graph -> JsonSchemaOptions -> Module -> [TypeDefinition] -> Result (M.Map FilePath JS.Document))
constructModule = define "constructModule" $
  doc "Construct JSON Schema documents from type definitions" $
  lambda "cx" $ lambda "g" $ lambda "opts" $ lambda "mod" $ lambda "typeDefs" $
    var "hydra.ext.json.schema.coder.constructModule" @@ var "cx" @@ var "g" @@ var "opts" @@ var "mod" @@ var "typeDefs"

encodeField :: TBinding (Context -> Graph -> FieldType -> Result (JS.Keyword, JS.Schema))
encodeField = define "encodeField" $
  doc "Encode a field type as a JSON Schema keyword-schema pair" $
  lambda "cx" $ lambda "g" $ lambda "ft" $ lets [
    "name">: project _FieldType _FieldType_name @@ var "ft",
    "typ">: project _FieldType _FieldType_type @@ var "ft"] $
    Eithers.map
      (lambda "res" $ pair (wrap JS._Keyword (Core.unName $ var "name")) (wrap JS._Schema (var "res")))
      (encodeType @@ var "cx" @@ var "g" @@ false @@ var "typ")

encodeName :: TBinding (Name -> String)
encodeName = define "encodeName" $
  doc "Encode a Hydra name as a safe identifier string, replacing non-alphanumeric characters with underscores" $
  lambda "name" $
    Formatting.nonAlnumToUnderscores @@ Core.unName (var "name")

encodeNamedType :: TBinding (Context -> Graph -> Name -> Type -> Result [JS.Restriction])
encodeNamedType = define "encodeNamedType" $
  doc "Encode a named type as a list of JSON Schema restrictions with a title" $
  lambda "cx" $ lambda "g" $ lambda "name" $ lambda "typ" $
    Eithers.map
      (lambda "res" $ Lists.concat $ list [
        list [inject JS._Restriction JS._Restriction_title (Core.unName $ var "name")],
        var "res"])
      (encodeType @@ var "cx" @@ var "g" @@ false @@ (Rewriting.deannotateType @@ var "typ"))

encodeType :: TBinding (Context -> Graph -> Bool -> Type -> Result [JS.Restriction])
encodeType = define "encodeType" $
  doc "Encode a Hydra type as a list of JSON Schema restrictions" $
  lambda "cx" $ lambda "g" $ lambda "optional" $ lambda "typ" $
    var "hydra.ext.json.schema.coder.encodeType" @@ var "cx" @@ var "g" @@ var "optional" @@ var "typ"

isRequiredField :: TBinding (FieldType -> Bool)
isRequiredField = define "isRequiredField" $
  doc "Determine whether a field is required (i.e., not optional/Maybe)" $
  lambda "ft" $ lets [
    "typ">: project _FieldType _FieldType_type @@ var "ft"] $
    cases _Type (Rewriting.deannotateType @@ var "typ") (Just true) [
      _Type_maybe>>: constant false]

referenceRestriction :: TBinding (Name -> JS.Restriction)
referenceRestriction = define "referenceRestriction" $
  doc "Create a JSON Schema reference restriction for a named type" $
  lambda "name" $
    inject JS._Restriction JS._Restriction_reference
      (wrap JS._SchemaReference (Strings.cat $ list [string "#/$defs/", encodeName @@ var "name"]))
