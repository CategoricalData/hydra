module Hydra.Sources.Kernel.Types.Diff where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Core.Overlay.Haskell.Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: ModuleName
ns = ModuleName "hydra.core.diff"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just
              ("A generic, domain-agnostic framework for diffs, patches, and migrations over any 'type'/'value'"
              <> " grammar (hydra.core.model.Type/Term, property-graph types, RDF shapes, ...). Fully parameterized:"
              <> " no Type/Term/Name/step type is hardcoded. Type parameters: a = the thing being edited"
              <> " (a type or a value); t = a 'type' type; v = a value/instance type; s = a step into a value;"
              <> " n = a name/key. Fill and Migrator hold host functions and are runtime-only (not serialized)."))}
  where
    definitions = [
      edit,
      fill,
      hole,
      holeFiller,
      invalidFillError,
      migrationError,
      migrationPlan,
      migrator,
      mistyped,
      schemaHole,
      stepEdit]

edit :: TypeDefinition
edit = define "Edit" $
  doc ("A base-relative edit (delta) transforming a value into a new value. Applied forward against a base;"
    <> " a symmetric before/after 'diff' is derived from (base, Edit), not stored here. Canonical by"
    <> " construction: descend carries single-step StepEdits, so there is exactly one encoding of any change.") $
  T.forAlls ["a", "s"] $ T.union [
    "retain">:
      doc "Leave this position unchanged" T.unit,
    "set">:
      doc "Set this position to the given value (upsert: prior presence is irrelevant)"
      "a",
    "delete">:
      doc "Remove this position (valid only at a removable position, e.g. a record field or collection element)"
      T.unit,
    "descend">:
      doc "Edit specific immediate children, one StepEdit per changed step" $
      T.list (stepEdit @@ "a" @@ "s")]

stepEdit :: TypeDefinition
stepEdit = define "StepEdit" $
  doc "An edit applied at a single immediate step of a value" $
  T.forAlls ["a", "s"] $ T.record [
    "step">:
      doc "The immediate step to descend into"
      "s",
    "edit">:
      doc "The edit to apply at that step" $
      edit @@ "a" @@ "s"]

hole :: TypeDefinition
hole = define "Hole" $
  doc ("A demand the migration cannot satisfy from the old value alone: at a value-path, a value of the given"
    <> " 'type' must be supplied.") $
  T.forAlls ["t", "s"] $ T.record [
    "path">:
      doc "The value-path (sequence of steps) to the position needing a value" $
      T.list "s",
    "type">:
      doc "The 'type' a fill must produce at this hole"
      "t"]

fill :: TypeDefinition
fill = define "Fill" $
  doc ("A user's answer to a hole. RUNTIME-ONLY (holds a host function; not serialized). fillNow supplies the"
    <> " value here (a constant is a function ignoring its input); defer leaves the hole open for an enclosing"
    <> " context to supply.") $
  T.forAlls ["v"] $ T.union [
    "fillNow">:
      doc "Fill the hole here: a function from the old value (context) to the filled value" $
      "v" ~> "v",
    "defer">:
      doc "Leave the hole open, to be supplied by an enclosing context where this value is embedded" T.unit]

holeFiller :: TypeDefinition
holeFiller = define "HoleFiller" $
  doc "A value supplied for a residual (deferred) hole, identified by its path (engine-internal)" $
  T.forAlls ["v", "s"] $ T.record [
    "path">:
      doc "The path of the residual hole being supplied" $
      T.list "s",
    "value">:
      doc "The value supplied for that hole"
      "v"]

schemaHole :: TypeDefinition
schemaHole = define "SchemaHole" $
  doc ("A hole located within a named type of a schema. A type with several holes yields several SchemaHoles"
    <> " sharing a typeName.") $
  T.forAlls ["n", "t", "s"] $ T.record [
    "typeName">:
      doc "The name of the type (a schema entry) containing this hole"
      "n",
    "hole">:
      doc "The hole within that type" $
      hole @@ "t" @@ "s"]

migrationPlan :: TypeDefinition
migrationPlan = define "MigrationPlan" $
  doc ("The result of diffing two schemas: the per-type edits (which drive the transform) bundled with the"
    <> " ordered holes the user must answer. Supplied once to constructMigrator so the diff is never re-passed.") $
  T.forAlls ["n", "t", "s"] $ T.record [
    "edits">:
      doc "The schema diff: an edit per changed type, keyed by type name" $
      T.map "n" (edit @@ "t" @@ "s"),
    "holes">:
      doc "The ordered holes across all changed types, for the user to answer" $
      T.list (schemaHole @@ "n" @@ "t" @@ "s")]

migrator :: TypeDefinition
migrator = define "Migrator" $
  doc ("A constructed instance migrator. RUNTIME-ONLY (apply is a host function; not serialized). Carries the"
    <> " residual (deferred) holes an enclosing context must supply, and applies to a value given values for"
    <> " those residual holes.") $
  T.forAlls ["t", "v", "s"] $ T.record [
    "residualHoles">:
      doc "Deferred holes that an enclosing context must supply when composing this migrator" $
      T.list (hole @@ "t" @@ "s"),
    "apply">:
      doc "Migrate a value, given values for the residual holes; fails with a MigrationError per instance" $
      T.list (holeFiller @@ "v" @@ "s") ~> ("v" ~> T.either_ (migrationError @@ "t" @@ "v" @@ "s") "v")]

mistyped :: TypeDefinition
mistyped = define "Mistyped" $
  doc "A value together with the 'type' it failed to inhabit" $
  T.forAlls ["t", "v"] $ T.record [
    "value">:
      doc "The offending value"
      "v",
    "type">:
      doc "The type the value did not inhabit"
      "t"]

invalidFillError :: TypeDefinition
invalidFillError = define "InvalidFillError" $
  doc "A validation-time failure: the user's fills do not correctly answer the plan's holes" $
  T.forAlls ["t", "v", "s"] $ T.union [
    "illTyped">:
      doc "A fillNow produced a value that does not inhabit the hole's type" $
      hole @@ "t" @@ "s",
    "missingFill">:
      doc "A hole received no fill" $
      hole @@ "t" @@ "s",
    "extraFill">:
      doc "A surplus fill at this index, with no hole to answer"
      T.int32]

migrationError :: TypeDefinition
migrationError = define "MigrationError" $
  doc "An application-time failure: a specific instance could not be migrated by a valid Migrator" $
  T.forAlls ["t", "v", "s"] $ T.union [
    "notAnInstance">:
      doc "The input value is not a valid instance of the old type"
      "v",
    "outOfRange">:
      doc "A value failed a narrowing conversion (e.g. numeric overflow); fail-fast rather than truncate"
      "v",
    "rejectedVariant">:
      doc "An instance of a removed union variant whose handler is reject"
      "v",
    "illTypedResult">:
      doc "A fill or resolver produced a value that does not inhabit the hole's type" $
      mistyped @@ "t" @@ "v",
    "unfilledHole">:
      doc "A deferred hole reached application time still unanswered" $
      hole @@ "t" @@ "s"]
