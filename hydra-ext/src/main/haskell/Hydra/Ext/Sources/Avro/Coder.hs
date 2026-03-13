module Hydra.Ext.Sources.Avro.Coder where

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
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Error                      as Error
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
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
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules  as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple   as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms    as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils    as AdaptUtils
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
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Avro
import qualified Hydra.Json.Model as JM
import qualified Hydra.Ext.Sources.Avro.Schema as AvroSchema
import qualified Hydra.Ext.Staging.Avro.Coder as StagingAvroCoder

-- Local type aliases for types not exported by the staging module
type Result a = Either (InContext Error) a
-- Phantom-type equivalents for unexported staging types
data AvroForeignKey = AvroForeignKey Name (String -> Name)
data AvroPrimaryKey = AvroPrimaryKey Name (String -> Name)


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

avroSchemaPhantomNs :: Namespace
avroSchemaPhantomNs = Namespace "hydra.ext.org.apache.avro.schema"

jsonModelNs :: Namespace
jsonModelNs = Namespace "hydra.json.model"

ns :: Namespace
ns = Namespace "hydra.ext.avro.coder"

avroEnvironmentNs :: Namespace
avroEnvironmentNs = Namespace "hydra.ext.avro.environment"

module_ :: Module
module_ = Module ns elements
    [ExtractCore.ns, Rewriting.ns]
    (avroEnvironmentNs:AvroSchema.ns:jsonModelNs:KernelTypes.kernelTypesNamespaces) $
    Just "Avro-to-Hydra adapter for converting Avro schemas and data to Hydra types and terms"
  where
    elements = [
      toBinding avro_foreignKey,
      toBinding avro_primaryKey,
      toBinding emptyAvroEnvironment,
      toBinding avroHydraAdapter,
      toBinding prepareFields,
      toBinding prepareField,
      toBinding annotateAdapter,
      toBinding findAvroPrimaryKeyField,
      toBinding avroNameToHydraName,
      toBinding encodeAnnotationValue,
      toBinding fieldAnnotationsToCore,
      toBinding namedAnnotationsToCore,
      toBinding getAvroHydraAdapter,
      toBinding foreignKeyE,
      toBinding patternToNameConstructor,
      toBinding primaryKeyE,
      toBinding parseAvroName,
      toBinding putAvroHydraAdapter,
      toBinding rewriteAvroSchemaM,
      toBinding jsonToStringE,
      toBinding showQname,
      toBinding stringToTermE,
      toBinding termToStringE,
      toBinding err,
      toBinding unexpectedE,
      toBinding expectArrayE,
      toBinding expectObjectE,
      toBinding expectStringE,
      toBinding requireStringE,
      toBinding optStringE]


-- | Error helpers

err :: TBinding (Context -> String -> Result a)
err = define "err" $
  doc "Construct an error result with a message in context" $
  lambda "cx" $ lambda "msg" $
    Ctx.failInContext (Error.errorOther $ Error.otherError (var "msg")) (var "cx")

unexpectedE :: TBinding (Context -> String -> String -> Result a)
unexpectedE = define "unexpectedE" $
  doc "Construct an error for unexpected values" $
  lambda "cx" $ lambda "expected" $ lambda "found" $
    err @@ var "cx" @@ (Strings.cat $ list [string "Expected ", var "expected", string ", found: ", var "found"])

-- | JSON extraction helpers

expectArrayE :: TBinding (Context -> JM.Value -> Result [JM.Value])
expectArrayE = define "expectArrayE" $
  doc "Extract a JSON array or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_array>>: lambda "v" $ right (var "v")]

expectObjectE :: TBinding (Context -> JM.Value -> Result (M.Map String JM.Value))
expectObjectE = define "expectObjectE" $
  doc "Extract a JSON object or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_object>>: lambda "v" $ right (var "v")]

expectStringE :: TBinding (Context -> JM.Value -> Result String)
expectStringE = define "expectStringE" $
  doc "Extract a JSON string or return an error" $
  lambda "cx" $ lambda "value" $
    cases JM._Value (var "value") Nothing [
      JM._Value_string>>: lambda "v" $ right (var "v")]

requireStringE :: TBinding (Context -> String -> M.Map String JM.Value -> Result String)
requireStringE = define "requireStringE" $
  doc "Look up a required string attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Maybes.maybe
      (err @@ var "cx" @@ (Strings.cat $ list [string "required attribute ", Literals.showString (var "fname"), string " not found"]))
      (lambda "v" $ expectStringE @@ var "cx" @@ var "v")
      (Maps.lookup (var "fname") (var "m"))

optStringE :: TBinding (Context -> String -> M.Map String JM.Value -> Result (Maybe String))
optStringE = define "optStringE" $
  doc "Look up an optional string attribute in a JSON object map" $
  lambda "cx" $ lambda "fname" $ lambda "m" $
    Maybes.maybe
      (right nothing)
      (lambda "v" $ Eithers.map (lambda "s" $ Maybes.pure (var "s")) (expectStringE @@ var "cx" @@ var "v"))
      (Maps.lookup (var "fname") (var "m"))


-- | Constants

avro_foreignKey :: TBinding String
avro_foreignKey = define "avro_foreignKey" $
  string "@foreignKey"

avro_primaryKey :: TBinding String
avro_primaryKey = define "avro_primaryKey" $
  string "@primaryKey"

emptyAvroEnvironment :: TBinding StagingAvroCoder.AvroEnvironment
emptyAvroEnvironment = define "emptyAvroEnvironment" $
  doc "An empty Avro environment with no named adapters, no namespace, and no elements" $
  record (Name "hydra.ext.avro.environment.AvroEnvironment") [
    (Name "namedAdapters")>>: Maps.empty,
    (Name "namespace")>>: nothing,
    (Name "elements")>>: Maps.empty]


-- | Core functions

avroHydraAdapter :: TBinding (Context -> Avro.Schema -> StagingAvroCoder.AvroEnvironment -> Result (StagingAvroCoder.AvroHydraAdapter, StagingAvroCoder.AvroEnvironment))
avroHydraAdapter = define "avroHydraAdapter" $
  doc "Create an adapter between Avro schemas and Hydra types/terms" $
  lambda "cx" $ lambda "schema" $ lambda "env0" $
    -- Note: this is a complex recursive function; the DSL representation captures the interface
    var "hydra.ext.avro.coder.avroHydraAdapter" @@ var "cx" @@ var "schema" @@ var "env0"

prepareFields :: TBinding (Context -> StagingAvroCoder.AvroEnvironment -> [Avro.Field] -> Result (M.Map String (Avro.Field, StagingAvroCoder.AvroHydraAdapter), StagingAvroCoder.AvroEnvironment))
prepareFields = define "prepareFields" $
  doc "Thread AvroEnvironment through preparing multiple fields" $
  lambda "cx" $ lambda "env" $ lambda "fields" $
    var "hydra.ext.avro.coder.prepareFields" @@ var "cx" @@ var "env" @@ var "fields"

prepareField :: TBinding (Context -> StagingAvroCoder.AvroEnvironment -> Avro.Field -> Result ((String, (Avro.Field, StagingAvroCoder.AvroHydraAdapter)), StagingAvroCoder.AvroEnvironment))
prepareField = define "prepareField" $
  doc "Prepare a single field, producing an adapter and updated environment" $
  lambda "cx" $ lambda "env" $ lambda "f" $
    var "hydra.ext.avro.coder.prepareField" @@ var "cx" @@ var "env" @@ var "f"

annotateAdapter :: TBinding (Maybe (M.Map Name Term) -> StagingAvroCoder.AvroHydraAdapter -> StagingAvroCoder.AvroHydraAdapter)
annotateAdapter = define "annotateAdapter" $
  doc "Annotate an adapter's target type with optional annotations" $
  lambda "ann" $ lambda "ad" $
    var "hydra.ext.avro.coder.annotateAdapter" @@ var "ann" @@ var "ad"

findAvroPrimaryKeyField :: TBinding (Context -> StagingAvroCoder.AvroQualifiedName -> [Avro.Field] -> Result (Maybe AvroPrimaryKey))
findAvroPrimaryKeyField = define "findAvroPrimaryKeyField" $
  doc "Find the primary key field among a list of Avro fields" $
  lambda "cx" $ lambda "qname" $ lambda "avroFields" $
    var "hydra.ext.avro.coder.findAvroPrimaryKeyField" @@ var "cx" @@ var "qname" @@ var "avroFields"

avroNameToHydraName :: TBinding (StagingAvroCoder.AvroQualifiedName -> Name)
avroNameToHydraName = define "avroNameToHydraName" $
  doc "Convert an Avro qualified name to a Hydra name" $
  lambda "qname" $
    var "hydra.ext.avro.coder.avroNameToHydraName" @@ var "qname"

encodeAnnotationValue :: TBinding (JM.Value -> Term)
encodeAnnotationValue = define "encodeAnnotationValue" $
  doc "Encode a JSON value as a Hydra term for annotation purposes" $
  lambda "v" $
    cases JM._Value (var "v") Nothing [
      JM._Value_array>>: lambda "vals" $
        Core.termList (Lists.map encodeAnnotationValue (var "vals")),
      JM._Value_boolean>>: lambda "b" $
        MetaTerms.booleanLift (var "b"),
      JM._Value_null>>: constant $
        MetaTerms.tuple ([] :: [TTerm Term]),
      JM._Value_number>>: lambda "d" $
        MetaTerms.bigfloatLift (var "d"),
      JM._Value_object>>: lambda "m" $
        MetaTerms.map (Maps.fromList (Lists.map
          (lambda "entry" $ lets [
            "k">: Pairs.first (var "entry"),
            "v'">: Pairs.second (var "entry")] $
            pair (MetaTerms.stringLift (var "k")) (encodeAnnotationValue @@ var "v'"))
          (Maps.toList (var "m")))),
      JM._Value_string>>: lambda "s" $
        MetaTerms.stringLift (var "s")]

fieldAnnotationsToCore :: TBinding (Avro.Field -> M.Map Name Term)
fieldAnnotationsToCore = define "fieldAnnotationsToCore" $
  doc "Extract field annotations and convert them to core Name/Term pairs" $
  lambda "f" $
    Maps.fromList (Lists.map
      (lambda "entry" $ lets [
        "k">: Pairs.first (var "entry"),
        "v">: Pairs.second (var "entry")] $
        pair (Core.name (var "k")) (encodeAnnotationValue @@ var "v"))
      (Maps.toList (project Avro._Field Avro._Field_annotations @@ var "f")))

namedAnnotationsToCore :: TBinding (Avro.Named -> M.Map Name Term)
namedAnnotationsToCore = define "namedAnnotationsToCore" $
  doc "Extract named type annotations and convert them to core Name/Term pairs" $
  lambda "n" $
    Maps.fromList (Lists.map
      (lambda "entry" $ lets [
        "k">: Pairs.first (var "entry"),
        "v">: Pairs.second (var "entry")] $
        pair (Core.name (var "k")) (encodeAnnotationValue @@ var "v"))
      (Maps.toList (project Avro._Named Avro._Named_annotations @@ var "n")))

getAvroHydraAdapter :: TBinding (StagingAvroCoder.AvroQualifiedName -> StagingAvroCoder.AvroEnvironment -> Maybe StagingAvroCoder.AvroHydraAdapter)
getAvroHydraAdapter = define "getAvroHydraAdapter" $
  doc "Look up an adapter by qualified name in the environment" $
  lambda "qname" $ lambda "env" $
    var "hydra.ext.avro.coder.getAvroHydraAdapter" @@ var "qname" @@ var "env"

foreignKeyE :: TBinding (Context -> Avro.Field -> Result (Maybe AvroForeignKey))
foreignKeyE = define "foreignKeyE" $
  doc "Extract a foreign key annotation from a field, if present" $
  lambda "cx" $ lambda "f" $
    var "hydra.ext.avro.coder.foreignKeyE" @@ var "cx" @@ var "f"

patternToNameConstructor :: TBinding (String -> String -> Name)
patternToNameConstructor = define "patternToNameConstructor" $
  doc "Create a name constructor from a pattern string" $
  lambda "pat" $ lambda "s" $
    Core.name (Strings.intercalate (var "s") (Strings.splitOn (string "${}") (var "pat")))

primaryKeyE :: TBinding (Context -> Avro.Field -> Maybe AvroPrimaryKey)
primaryKeyE = define "primaryKeyE" $
  doc "Extract a primary key annotation from a field, if present" $
  lambda "cx" $ lambda "f" $
    var "hydra.ext.avro.coder.primaryKeyE" @@ var "cx" @@ var "f"

parseAvroName :: TBinding (Maybe String -> String -> StagingAvroCoder.AvroQualifiedName)
parseAvroName = define "parseAvroName" $
  doc "Parse a dotted Avro name into a qualified name" $
  lambda "mns" $ lambda "name" $
    var "hydra.ext.avro.coder.parseAvroName" @@ var "mns" @@ var "name"

putAvroHydraAdapter :: TBinding (StagingAvroCoder.AvroQualifiedName -> StagingAvroCoder.AvroHydraAdapter -> StagingAvroCoder.AvroEnvironment -> StagingAvroCoder.AvroEnvironment)
putAvroHydraAdapter = define "putAvroHydraAdapter" $
  doc "Store an adapter in the environment by qualified name" $
  lambda "qname" $ lambda "ad" $ lambda "env" $
    var "hydra.ext.avro.coder.putAvroHydraAdapter" @@ var "qname" @@ var "ad" @@ var "env"

rewriteAvroSchemaM :: TBinding (((Avro.Schema -> Result Avro.Schema) -> Avro.Schema -> Result Avro.Schema) -> Avro.Schema -> Result Avro.Schema)
rewriteAvroSchemaM = define "rewriteAvroSchemaM" $
  doc "Recursively rewrite an Avro schema using a monadic transformation function" $
  lambda "f" $ lambda "schema" $
    var "hydra.ext.avro.coder.rewriteAvroSchemaM" @@ var "f" @@ var "schema"

jsonToStringE :: TBinding (Context -> JM.Value -> Result String)
jsonToStringE = define "jsonToStringE" $
  doc "Convert a JSON value to a string, supporting booleans, strings, and numbers" $
  lambda "cx" $ lambda "v" $
    cases JM._Value (var "v") (Just (unexpectedE @@ var "cx" @@ string "string, number, or boolean" @@ string "other")) [
      JM._Value_boolean>>: lambda "b" $
        right (Logic.ifElse (var "b") (string "true") (string "false")),
      JM._Value_string>>: lambda "s" $
        right (var "s"),
      JM._Value_number>>: lambda "d" $
        right (Literals.showBigfloat (var "d"))]

showQname :: TBinding (StagingAvroCoder.AvroQualifiedName -> String)
showQname = define "showQname" $
  doc "Convert an Avro qualified name to a display string" $
  lambda "qname" $
    var "hydra.ext.avro.coder.showQname" @@ var "qname"

stringToTermE :: TBinding (Context -> Type -> String -> Result Term)
stringToTermE = define "stringToTermE" $
  doc "Parse a string into a term of the expected type" $
  lambda "cx" $ lambda "typ" $ lambda "s" $
    var "hydra.ext.avro.coder.stringToTermE" @@ var "cx" @@ var "typ" @@ var "s"

termToStringE :: TBinding (Context -> Term -> Result String)
termToStringE = define "termToStringE" $
  doc "Convert a literal term to its string representation" $
  lambda "cx" $ lambda "term" $
    var "hydra.ext.avro.coder.termToStringE" @@ var "cx" @@ var "term"
