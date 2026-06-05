
module Hydra.Sources.Kernel.Terms.Lexical where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (buildGraph, chooseUniqueName, dereferenceSchemaType, dereferenceVariable, elementsToGraph, emptyGraph, emptyInferenceContext, fieldsOf, getField, graphToBindings, graphWithPrimitives, lookupBinding, lookupPrimitive, lookupTerm, matchEnum, matchRecord, matchUnion, matchUnitField, requireBinding, requirePrimitive, requirePrimitiveType, requireTerm, resolveTerm, stripAndDereferenceTerm, stripAndDereferenceTermEither)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Literals     as MetaLiterals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import qualified Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import qualified Hydra.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


import qualified Hydra.Sources.Kernel.Terms.Scoping as Scoping
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Strip as Strip
import qualified Hydra.Sources.Kernel.Terms.Show.Errors as ShowError


ns :: ModuleName
ns = ModuleName "hydra.lexical"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Strip.ns, ShowCore.ns, ShowError.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just ("A module for lexical operations over graphs."))}
  where
    definitions = [
      toDefinition buildGraph,
      toDefinition chooseUniqueName,
      toDefinition dereferenceSchemaType,
      toDefinition dereferenceVariable,
      toDefinition elementsToGraph,
      toDefinition emptyGraph,
      toDefinition emptyInferenceContext,
      toDefinition fieldsOf,
      toDefinition getField,
      toDefinition graphToBindings,
      toDefinition graphWithPrimitives,
      toDefinition lookupBinding,
      toDefinition lookupPrimitive,
      toDefinition lookupTerm,
      toDefinition matchEnum,
      toDefinition matchRecord,
      toDefinition matchUnion,
      toDefinition matchUnitField,
      toDefinition requireBinding,
      toDefinition requirePrimitive,
      toDefinition requirePrimitiveType,
      toDefinition requireTerm,
      toDefinition resolveTerm,
      toDefinition stripAndDereferenceTerm,
      toDefinition stripAndDereferenceTermEither]

-- | Build a Graph from element bindings, environment, and primitives.
-- Construction-time shadowing: any bound term or type whose name matches a primitive is removed,
-- so primitives always take priority by construction.
buildGraph :: TypedTermDefinition ([Binding] -> M.Map Name (Maybe Term) -> M.Map Name Primitive -> Graph)
buildGraph = define "buildGraph" $
  doc "Build a Graph from element bindings, environment, and primitives" $
  "elements" ~> "environment" ~> "primitives" ~>
  -- boundTerms: element bindings (name -> term) merged with let-bound vars from environment (Just term)
  "elementTerms" <~ Maps.fromList (Lists.map ("b" ~>
    pair (Core.bindingName (var "b")) (Core.bindingTerm (var "b"))) (var "elements")) $
  -- Keep only entries whose value is Just, and strip the Just wrapper.
  "letTerms" <~ Maps.fromList (Maybes.mapMaybe
    ("kv" ~> Maybes.map ("t" ~> pair (Pairs.first $ var "kv") (var "t")) (Pairs.second $ var "kv"))
    (Maps.toList $ var "environment")) $
  "mergedTerms" <~ Maps.union (var "elementTerms") (var "letTerms") $
  -- Construction-time shadowing: remove any binding whose name matches a primitive
  "filteredTerms" <~ Maps.filterWithKey ("k" ~> "_v" ~>
    Logic.not (Maps.member (var "k") (var "primitives"))) (var "mergedTerms") $
  -- boundTypes: extract bindingType from each element (preserving TypeScheme with constraints)
  "elementTypes" <~ Maps.fromList (Maybes.cat (Lists.map ("b" ~>
    Maybes.map ("ts" ~> pair (Core.bindingName (var "b")) (var "ts"))
      (Core.bindingTypeScheme (var "b"))) (var "elements"))) $
  "filteredTypes" <~ Maps.filterWithKey ("k" ~> "_v" ~>
    Logic.not (Maps.member (var "k") (var "primitives"))) (var "elementTypes") $
  Graph.graph
    (var "filteredTerms")
    (var "filteredTypes")
    Maps.empty
    (Sets.fromList (Maps.keys (Maps.filter ("mt" ~> Maybes.isNothing (var "mt")) (var "environment"))))
    Maps.empty
    (var "primitives")
    Maps.empty
    Sets.empty

chooseUniqueName :: TypedTermDefinition (S.Set Name -> Name -> Name)
chooseUniqueName = define "chooseUniqueName" $
  doc "Pick a name that does not collide with a reserved set, by appending a numeric suffix to the requested name when necessary" $
  "reserved" ~> "name" ~>
  "tryName" <~ ("index" ~>
    "candidate" <~ Logic.ifElse (Equality.equal (var "index") (int32 1))
      (var "name")
      (Core.name $ (Core.unName (var "name") ++ Literals.showInt32 (var "index"))) $
    Logic.ifElse (Sets.member (var "candidate") (var "reserved"))
      (var "tryName" @@ (Math.add (var "index") (int32 1)))
      (var "candidate")) $
  var "tryName" @@ (int32 1)

dereferenceSchemaType :: TypedTermDefinition (Name -> M.Map Name TypeScheme -> Maybe TypeScheme)
dereferenceSchemaType = define "dereferenceSchemaType" $
  doc "Resolve a schema type through a chain of zero or more typedefs" $
  "name" ~> "types" ~>
  "forType" <~ ("t" ~> cases _Type (var "t")
    (Just (just (Core.typeScheme (list ([] :: [TypedTerm Name])) (var "t") Phantoms.nothing))) [
    _Type_annotated>>: "at" ~> var "forType" @@ (Core.annotatedTypeBody (var "at")),
    _Type_forall>>: "ft" ~> Maybes.map
      ("ts" ~> Core.typeScheme
        -- Note: no alpha-renaming of type variables
        (Lists.cons (Core.forallTypeParameter (var "ft")) (Core.typeSchemeVariables (var "ts")))
        (Core.typeSchemeBody (var "ts"))
        (Core.typeSchemeConstraints (var "ts")))
      (var "forType" @@ (Core.forallTypeBody (var "ft"))),
    _Type_variable>>: "v" ~> dereferenceSchemaType @@ var "v" @@ var "types"]) $
  Maybes.bind
    (Maps.lookup (var "name") (var "types"))
    ("ts" ~> Maybes.map
      ("ts2" ~> Core.typeScheme
        -- Note: no alpha-renaming of type variables
        (Lists.concat2 (Core.typeSchemeVariables (var "ts")) (Core.typeSchemeVariables (var "ts2")))
        (Core.typeSchemeBody (var "ts2"))
        (Core.typeSchemeConstraints (var "ts2")))
      (var "forType" @@ (Core.typeSchemeBody (var "ts"))))

dereferenceVariable :: TypedTermDefinition (Graph -> Name -> Either Error Binding)
dereferenceVariable = define "dereferenceVariable" $
  doc "Look up a binding by name in a graph, returning Either an error or the binding" $
  "graph" ~> "name" ~>
  Maybes.cases (lookupBinding @@ var "graph" @@ var "name") (left (Error.errorResolution $ Error.resolutionErrorNoSuchBinding $ Error.noSuchBindingError (var "name"))) right_

elementsToGraph :: TypedTermDefinition (Graph -> M.Map Name TypeScheme -> [Binding] -> Graph)
elementsToGraph = define "elementsToGraph" $
  doc "Create a graph from a parent graph, schema types, and list of element bindings" $
  "parent" ~> "schemaTypes" ~> "elements" ~>
  "prims" <~ Graph.graphPrimitives (var "parent") $
  "g" <~ (buildGraph @@ var "elements" @@ Maps.empty @@ var "prims") $
  Graph.graphWithSchemaTypes (var "g") (var "schemaTypes")

emptyGraph :: TypedTermDefinition Graph
emptyGraph = define "emptyGraph" $
  doc "An empty graph; no elements, no primitives, no schema." $
  Graph.emptyGraph

emptyInferenceContext :: TypedTermDefinition InferenceContext
emptyInferenceContext = define "emptyInferenceContext" $
  doc "An empty inference context; fresh-variable counter at zero and empty trace." $
  Typing.inferenceContext (MetaLiterals.int32 0) (list ([] :: [TypedTerm SubtermStep]))

fieldsOf :: TypedTermDefinition (Type -> [FieldType])
fieldsOf = define "fieldsOf" $
  doc "Extract the fields of a record or union type" $
  "t" ~>
  "stripped" <~ Strip.deannotateType @@ var "t" $
  cases _Type (var "stripped")
    (Just (list ([] :: [TypedTerm FieldType]))) [
    _Type_forall>>: "forallType" ~> fieldsOf @@ (Core.forallTypeBody (var "forallType")),
    _Type_record>>: "rt" ~> var "rt",
    _Type_union>>: "rt" ~> var "rt"]

formatError :: TypedTerm (Error -> String)
formatError = "e" ~> ShowError.error_ @@ var "e"

getField :: TypedTermDefinition (M.Map Name Term -> Name -> (Term -> Either Error b) -> Either Error b)
getField = define "getField" $
  doc "Look up a field by name in a record's field map and decode its value, failing if the field is missing" $
  "m" ~> "fname" ~> "decode" ~>
  Maybes.cases (Maps.lookup (var "fname") (var "m")) (left (Error.errorResolution $ Error.resolutionErrorNoMatchingField $ Error.noMatchingFieldError (var "fname"))) (var "decode")

graphToBindings :: TypedTermDefinition (Graph -> [Binding])
graphToBindings = define "graphToBindings" $
  doc "Reconstruct a list of Bindings from a Graph's boundTerms and boundTypes" $
  "g" ~>
  Lists.map ("p" ~>
    "name" <~ Pairs.first (var "p") $
    "term" <~ Pairs.second (var "p") $
    Core.binding (var "name") (var "term")
      (Maps.lookup (var "name") (Graph.graphBoundTypes (var "g"))))
    (Maps.toList (Graph.graphBoundTerms (var "g")))

graphWithPrimitives :: TypedTermDefinition ([Primitive] -> [Primitive] -> Graph)
graphWithPrimitives = define "graphWithPrimitives" $
  doc "Build a graph with primitives assembled from built-in and user-provided lists. User-provided primitives shadow built-in ones." $
  "builtIn" ~> "userProvided" ~>
  "toMap" <~ ("ps" ~> Maps.fromList (Lists.map ("p" ~>
    pair (Packaging.primitiveDefinitionName $ Graph.primitiveDefinition (var "p")) (var "p")) (var "ps"))) $
  "prims" <~ Maps.union (var "toMap" @@ var "userProvided") (var "toMap" @@ var "builtIn") $
  buildGraph @@ list ([] :: [TypedTerm Binding]) @@ Maps.empty @@ var "prims"

lookupBinding :: TypedTermDefinition (Graph -> Name -> Maybe Binding)
lookupBinding = define "lookupBinding" $
  doc "Look up a binding in a graph by name" $
  "graph" ~> "name" ~>
  Maybes.map
    ("term" ~> Core.binding (var "name") (var "term")
      (Maps.lookup (var "name") (Graph.graphBoundTypes (var "graph"))))
    (Maps.lookup (var "name") (Graph.graphBoundTerms (var "graph")))

lookupPrimitive :: TypedTermDefinition (Graph -> Name -> Maybe Primitive)
lookupPrimitive = define "lookupPrimitive" $
  doc "Look up a primitive function in a graph by name" $
  "graph" ~> "name" ~>
  Maps.lookup (var "name") (Graph.graphPrimitives (var "graph"))

lookupTerm :: TypedTermDefinition (Graph -> Name -> Maybe Term)
lookupTerm = define "lookupTerm" $
  doc "Look up a term by name in a graph" $
  "graph" ~> "name" ~>
  Maps.lookup (var "name") (Graph.graphBoundTerms (var "graph"))

matchEnum :: TypedTermDefinition (Graph -> Name -> [(Name, b)] -> Term -> Either Error b)
matchEnum = define "matchEnum" $
  doc "Match a term against an enum type, dispatching on the variant name to a value from the supplied list" $
  "graph" ~> "tname" ~> "pairs" ~>
  matchUnion @@ var "graph" @@ var "tname" @@ (Lists.map ("pair" ~>
    matchUnitField @@ (Pairs.first (var "pair")) @@ (Pairs.second (var "pair"))) (var "pairs"))

matchRecord :: TypedTermDefinition (Graph -> (M.Map Name Term -> Either Error b) -> Term -> Either Error b)
matchRecord = define "matchRecord" $
  doc "Match a term against a record type and decode its fields, failing if the term is not a record" $
  "graph" ~> "decode" ~> "term" ~>
  "stripped" <~ Strip.deannotateAndDetypeTerm @@ var "term" $
  cases _Term (var "stripped")
    (Just (left (Error.errorResolution $ Error.resolutionErrorUnexpectedShape $ Error.unexpectedShapeError (string "record") (ShowCore.term @@ var "term")))) [
    _Term_record>>: "record" ~> var "decode" @@
      (Maps.fromList (Lists.map
        ("field" ~> pair (Core.fieldName (var "field")) (Core.fieldTerm (var "field")))
        (Core.recordFields (var "record"))))]

matchUnion :: TypedTermDefinition (Graph -> Name -> [(Name, Term -> Either Error b)] -> Term -> Either Error b)
matchUnion = define "matchUnion" $
  doc "Match a term against a union type, dispatching on the injected variant to the appropriate decoder. Variable terms are dereferenced through the graph before matching." $
  "graph" ~> "tname" ~> "pairs" ~> "term" ~>
  "stripped" <~ Strip.deannotateAndDetypeTerm @@ var "term" $
  "mapping" <~ Maps.fromList (var "pairs") $
  cases _Term (var "stripped")
    (Just (left (Error.errorResolution $ Error.resolutionErrorUnexpectedShape $ Error.unexpectedShapeError (Strings.cat2 (string "injection for type ") (Core.unName (var "tname"))) (ShowCore.term @@ var "stripped")))) [
    _Term_variable>>: "name" ~>
      "el" <<~ requireBinding @@ var "graph" @@ var "name" $
      matchUnion @@ var "graph" @@ var "tname" @@ var "pairs" @@ (Core.bindingTerm (var "el")),
    _Term_inject>>: "injection" ~>
      "exp" <~ (
        "fname" <~ Core.fieldName (Core.injectionField (var "injection")) $
        "val" <~ Core.fieldTerm (Core.injectionField (var "injection")) $
        Maybes.cases (Maps.lookup (var "fname") (var "mapping")) (left (Error.errorResolution $ Error.resolutionErrorNoMatchingField $ Error.noMatchingFieldError (var "fname"))) ("f" ~> var "f" @@ var "val")) $
      Logic.ifElse (Core.equalName_ (Core.injectionTypeName (var "injection")) (var "tname"))
        (var "exp")
        (left (Error.errorResolution $ Error.resolutionErrorUnexpectedShape $ Error.unexpectedShapeError (Strings.cat2 (string "injection for type ") (Core.unName (var "tname"))) (ShowCore.term @@ var "term")))]

matchUnitField :: TypedTermDefinition (Name -> y -> (Name, x -> Either Error y))
matchUnitField = define "matchUnitField" $
  doc "Build a (fieldName, decoder) pair for a unit-valued union variant: the decoder ignores its argument and returns a fixed value" $
  "fname" ~> "x" ~> pair (var "fname") ("ignored" ~> right (var "x"))

requireBinding :: TypedTermDefinition (Graph -> Name -> Either Error Binding)
requireBinding = define "requireBinding" $
  doc "Look up a binding in a graph by name, failing with a list of available names if it is not found" $
  "graph" ~> "name" ~>
  "showAll" <~ false $
  "ellipsis" <~ ("strings" ~>
    Logic.ifElse (Logic.and (Equality.gt (Lists.length (var "strings")) (int32 3)) (Logic.not (var "showAll")))
      (Lists.concat2 (Lists.take (int32 3) (var "strings")) (list [string "..."]))
      (var "strings")) $
  "errMsg" <~ (
    (string "no such element: ") ++ (Core.unName (var "name")) ++
    (string ". Available elements: {") ++
    (Strings.intercalate (string ", ") (var "ellipsis" @@ (Lists.map (reify Core.unName) (Maps.keys (Graph.graphBoundTerms (var "graph")))))) ++
    (string "}")) $
  Maybes.cases (lookupBinding @@ var "graph" @@ var "name") (left (Error.errorResolution $ Error.resolutionErrorOther $ Error.otherResolutionError (var "errMsg"))) (reify right)

requirePrimitive :: TypedTermDefinition (Graph -> Name -> Either Error Primitive)
requirePrimitive = define "requirePrimitive" $
  doc "Look up a primitive in a graph by name, failing if it is not registered" $
  "graph" ~> "name" ~>
  Maybes.cases (lookupPrimitive @@ var "graph" @@ var "name") (left (Error.errorResolution $ Error.resolutionErrorNoSuchPrimitive $ Error.noSuchPrimitiveError (var "name"))) (reify right)

requirePrimitiveType :: TypedTermDefinition (Graph -> Name -> Either Error TypeScheme)
requirePrimitiveType = define "requirePrimitiveType" $
  doc "Look up a primitive's type scheme in a graph by name, failing if the primitive is not registered" $
  "tx" ~> "name" ~>
  -- Look up the primitive directly and extract its type, avoiding O(p) map reconstruction.
  "mts" <~ Maybes.map ("_p" ~> Scoping.termSignatureToTypeScheme @@ (Packaging.primitiveDefinitionSignature $ Graph.primitiveDefinition (var "_p")))
    (Maps.lookup (var "name") (Graph.graphPrimitives $ var "tx")) $
  optCases (var "mts")
    (left (Error.errorResolution $ Error.resolutionErrorNoSuchPrimitive $ Error.noSuchPrimitiveError (var "name")))
    ("ts" ~> right $ var "ts")

requireTerm :: TypedTermDefinition (Graph -> Name -> Either Error Term)
requireTerm = define "requireTerm" $
  doc "Resolve a name to a term in the graph, following variable references, and fail if the name is not bound" $
  "graph" ~> "name" ~>
  Maybes.cases (resolveTerm @@ var "graph" @@ var "name") (left (Error.errorResolution $ Error.resolutionErrorNoSuchBinding $ Error.noSuchBindingError (var "name"))) (reify right)

resolveTerm :: TypedTermDefinition (Graph -> Name -> Maybe Term)
resolveTerm = define "resolveTerm" $
  doc "TODO: distinguish between lambda-bound and let-bound variables" $
  "graph" ~> "name" ~>
  "recurse" <~ ("term" ~>
    "stripped" <~ Strip.deannotateTerm @@ var "term" $
    cases _Term (var "stripped")
      (Just (just (var "term"))) [
      _Term_variable>>: "name'" ~> resolveTerm @@ var "graph" @@ var "name'"]) $
  Maybes.cases (lookupTerm @@ var "graph" @@ var "name") nothing (var "recurse")

stripAndDereferenceTerm :: TypedTermDefinition (Graph -> Term -> Either Error Term)
stripAndDereferenceTerm = define "stripAndDereferenceTerm" $
  doc "Strip annotations and type lambdas/applications from a term, then follow variable references through the graph until a non-variable term is reached" $
  "graph" ~> "term" ~>
  "stripped" <~ Strip.deannotateAndDetypeTerm @@ var "term" $
  cases _Term (var "stripped")
    (Just (right (var "stripped"))) [
    _Term_variable>>: "v" ~>
      Eithers.bind (requireTerm @@ var "graph" @@ var "v") (
        "t" ~> stripAndDereferenceTerm @@ var "graph" @@ var "t")]

stripAndDereferenceTermEither :: TypedTermDefinition (Graph -> Term -> Either Error Term)
stripAndDereferenceTermEither = define "stripAndDereferenceTermEither" $
  doc "Strip annotations and dereference variables, returning Either an error or the resolved term" $
  "graph" ~> "term" ~>
  "stripped" <~ Strip.deannotateAndDetypeTerm @@ var "term" $
  cases _Term (var "stripped")
    (Just (right (var "stripped"))) [
    _Term_variable>>: "v" ~>
      Eithers.either_
        left_  -- propagate error
        ("binding" ~> stripAndDereferenceTermEither @@ var "graph" @@ (Core.bindingTerm (var "binding")))
        (dereferenceVariable @@ var "graph" @@ var "v")]


