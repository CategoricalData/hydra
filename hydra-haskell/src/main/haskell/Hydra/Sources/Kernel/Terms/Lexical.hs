
module Hydra.Sources.Kernel.Terms.Lexical where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (buildGraph, chooseUniqueName, dereferenceSchemaType, dereferenceVariable, elementsToGraph, emptyContext, emptyGraph, fieldsOf, getField, graphToBindings, lookupBinding, lookupPrimitive, lookupTerm, matchEnum, matchRecord, matchUnion, matchUnitField, requireBinding, requirePrimitive, requirePrimitiveType, requireTerm, resolveTerm, stripAndDereferenceTerm, stripAndDereferenceTermEither)
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
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
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
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Strip as Strip
import qualified Hydra.Sources.Kernel.Terms.Show.Errors as ShowError


ns :: Namespace
ns = Namespace "hydra.lexical"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
   [Strip.ns, ShowCore.ns, ShowError.ns]
    kernelTypesNamespaces $
    Just ("A module for lexical operations over graphs.")
  where
    elements = [
      toDefinition buildGraph,
      toDefinition chooseUniqueName,
      toDefinition dereferenceSchemaType,
      toDefinition dereferenceVariable,
      toDefinition elementsToGraph,
      toDefinition emptyContext,
      toDefinition emptyGraph,
      toDefinition graphToBindings,
      toDefinition fieldsOf,
      toDefinition getField,
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

formatError :: TTerm (InContext Error -> String)
formatError = "ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")

-- | Build a Graph from element bindings, environment, and primitives.
buildGraph :: TTermDefinition ([Binding] -> M.Map Name (Maybe Term) -> M.Map Name Primitive -> Graph)
buildGraph = define "buildGraph" $
  doc "Build a Graph from element bindings, environment, and primitives" $
  "elements" ~> "environment" ~> "primitives" ~>
  -- boundTerms: element bindings (name -> term) merged with let-bound vars from environment (Just term)
  "elementTerms" <~ Maps.fromList (Lists.map ("b" ~>
    pair (Core.bindingName (var "b")) (Core.bindingTerm (var "b"))) (var "elements")) $
  "letTerms" <~ Maps.map ("mt" ~> Maybes.fromJust (var "mt"))
    (Maps.filter ("mt" ~> Maybes.isJust (var "mt")) (var "environment")) $
  -- boundTypes: extract bindingType from each element (preserving TypeScheme with constraints)
  "elementTypes" <~ Maps.fromList (Maybes.cat (Lists.map ("b" ~>
    Maybes.map ("ts" ~> pair (Core.bindingName (var "b")) (var "ts"))
      (Core.bindingType (var "b"))) (var "elements"))) $
  Graph.graph
    (Maps.union (var "elementTerms") (var "letTerms"))
    (var "elementTypes")
    Maps.empty
    (Sets.fromList (Maps.keys (Maps.filter ("mt" ~> Maybes.isNothing (var "mt")) (var "environment"))))
    Maps.empty
    (var "primitives")
    Maps.empty
    Sets.empty

chooseUniqueName :: TTermDefinition (S.Set Name -> Name -> Name)
chooseUniqueName = define "chooseUniqueName" $
  "reserved" ~> "name" ~>
  "tryName" <~ ("index" ~>
    "candidate" <~ Logic.ifElse (Equality.equal (var "index") (int32 1))
      (var "name")
      (Core.name $ (Core.unName (var "name") ++ Literals.showInt32 (var "index"))) $
    Logic.ifElse (Sets.member (var "candidate") (var "reserved"))
      (var "tryName" @@ (Math.add (var "index") (int32 1)))
      (var "candidate")) $
  var "tryName" @@ (int32 1)

dereferenceSchemaType :: TTermDefinition (Name -> M.Map Name TypeScheme -> Maybe TypeScheme)
dereferenceSchemaType = define "dereferenceSchemaType" $
  doc "Resolve a schema type through a chain of zero or more typedefs" $
  "name" ~> "types" ~>
  "forType" <~ ("t" ~> cases _Type (var "t")
    (Just (just (Core.typeScheme (list ([] :: [TTerm Name])) (var "t") Phantoms.nothing))) [
    _Type_annotated>>: "at" ~> var "forType" @@ (Core.annotatedTypeBody (var "at")),
    _Type_forall>>: "ft" ~> Maybes.map
      ("ts" ~> Core.typeScheme
        -- Note: no alpha-renaming of type variables
        (Lists.cons (Core.forallTypeParameter (var "ft")) (Core.typeSchemeVariables (var "ts")))
        (Core.typeSchemeType (var "ts"))
        (Core.typeSchemeConstraints (var "ts")))
      (var "forType" @@ (Core.forallTypeBody (var "ft"))),
    _Type_variable>>: "v" ~> dereferenceSchemaType @@ var "v" @@ var "types"]) $
  Maybes.bind
    (Maps.lookup (var "name") (var "types"))
    ("ts" ~> Maybes.map
      ("ts2" ~> Core.typeScheme
        -- Note: no alpha-renaming of type variables
        (Lists.concat2 (Core.typeSchemeVariables (var "ts")) (Core.typeSchemeVariables (var "ts2")))
        (Core.typeSchemeType (var "ts2"))
        (Core.typeSchemeConstraints (var "ts2")))
      (var "forType" @@ (Core.typeSchemeType (var "ts"))))

dereferenceVariable :: TTermDefinition (Graph -> Name -> Either String Binding)
dereferenceVariable = define "dereferenceVariable" $
  doc "Look up a binding by name in a graph, returning Either an error or the binding" $
  "graph" ~> "name" ~>
  Maybes.maybe
    (left ((string "no such element: ") ++ (Core.unName (var "name"))))
    right_
    (lookupBinding @@ var "graph" @@ var "name")

elementsToGraph :: TTermDefinition (Graph -> M.Map Name TypeScheme -> [Binding] -> Graph)
elementsToGraph = define "elementsToGraph" $
  doc "Create a graph from a parent graph, schema types, and list of element bindings" $
  "parent" ~> "schemaTypes" ~> "elements" ~>
  "prims" <~ Graph.graphPrimitives (var "parent") $
  "g" <~ (buildGraph @@ var "elements" @@ Maps.empty @@ var "prims") $
  Graph.graphWithSchemaTypes (var "g") (var "schemaTypes")

emptyContext :: TTermDefinition Context
emptyContext = define "emptyContext" $
  doc "An empty context; no trace, no messages, no other data." $
  record _Context [
    _Context_trace>>: list ([] :: [TTerm String]),
    _Context_messages>>: list ([] :: [TTerm String]),
    _Context_other>>: Maps.empty]

emptyGraph :: TTermDefinition Graph
emptyGraph = define "emptyGraph" $
  doc "An empty graph; no elements, no primitives, no schema." $
  Graph.emptyGraph

graphToBindings :: TTermDefinition (Graph -> [Binding])
graphToBindings = define "graphToBindings" $
  doc "Reconstruct a list of Bindings from a Graph's boundTerms and boundTypes" $
  "g" ~>
  Lists.map ("p" ~>
    "name" <~ Pairs.first (var "p") $
    "term" <~ Pairs.second (var "p") $
    Core.binding (var "name") (var "term")
      (Maps.lookup (var "name") (Graph.graphBoundTypes (var "g"))))
    (Maps.toList (Graph.graphBoundTerms (var "g")))

fieldsOf :: TTermDefinition (Type -> [FieldType])
fieldsOf = define "fieldsOf" $
  doc "Extract the fields of a record or union type" $
  "t" ~>
  "stripped" <~ Strip.deannotateType @@ var "t" $
  cases _Type (var "stripped")
    (Just (list ([] :: [TTerm FieldType]))) [
    _Type_forall>>: "forallType" ~> fieldsOf @@ (Core.forallTypeBody (var "forallType")),
    _Type_record>>: "rt" ~> var "rt",
    _Type_union>>: "rt" ~> var "rt"]

getField :: TTermDefinition (Context -> M.Map Name Term -> Name -> (Term -> Either (InContext Error) b) -> Either (InContext Error) b)
getField = define "getField" $
  "cx" ~> "m" ~> "fname" ~> "decode" ~>
  Maybes.maybe
    (Ctx.failInContext (Error.errorOther $ Error.otherError ((string "expected field ") ++ (Core.unName (var "fname")) ++ (string " not found"))) (var "cx"))
    (var "decode")
    (Maps.lookup (var "fname") (var "m"))

lookupBinding :: TTermDefinition (Graph -> Name -> Maybe Binding)
lookupBinding = define "lookupBinding" $
  doc "Look up a binding in a graph by name" $
  "graph" ~> "name" ~>
  Maybes.map
    ("term" ~> Core.binding (var "name") (var "term")
      (Maps.lookup (var "name") (Graph.graphBoundTypes (var "graph"))))
    (Maps.lookup (var "name") (Graph.graphBoundTerms (var "graph")))

lookupPrimitive :: TTermDefinition (Graph -> Name -> Maybe Primitive)
lookupPrimitive = define "lookupPrimitive" $
  doc "Look up a primitive function in a graph by name" $
  "graph" ~> "name" ~>
  Maps.lookup (var "name") (Graph.graphPrimitives (var "graph"))

lookupTerm :: TTermDefinition (Graph -> Name -> Maybe Term)
lookupTerm = define "lookupTerm" $
  doc "Look up a term by name in a graph" $
  "graph" ~> "name" ~>
  Maps.lookup (var "name") (Graph.graphBoundTerms (var "graph"))

matchEnum :: TTermDefinition (Context -> Graph -> Name -> [(Name, b)] -> Term -> Either (InContext Error) b)
matchEnum = define "matchEnum" $
  "cx" ~> "graph" ~> "tname" ~> "pairs" ~>
  matchUnion @@ var "cx" @@ var "graph" @@ var "tname" @@ (Lists.map ("pair" ~>
    matchUnitField @@ (Pairs.first (var "pair")) @@ (Pairs.second (var "pair"))) (var "pairs"))

matchRecord :: TTermDefinition (Context -> Graph -> (M.Map Name Term -> Either (InContext Error) b) -> Term -> Either (InContext Error) b)
matchRecord = define "matchRecord" $
  "cx" ~> "graph" ~> "decode" ~> "term" ~>
  "stripped" <~ Strip.deannotateAndDetypeTerm @@ var "term" $
  cases _Term (var "stripped")
    (Just (Ctx.failInContext (Error.errorOther $ Error.otherError ((string "expected a record, got ") ++ (ShowCore.term @@ var "term"))) (var "cx"))) [
    _Term_record>>: "record" ~> var "decode" @@
      (Maps.fromList (Lists.map
        ("field" ~> pair (Core.fieldName (var "field")) (Core.fieldTerm (var "field")))
        (Core.recordFields (var "record"))))]

matchUnion :: TTermDefinition (Context -> Graph -> Name -> [(Name, Term -> Either (InContext Error) b)] -> Term -> Either (InContext Error) b)
matchUnion = define "matchUnion" $
  "cx" ~> "graph" ~> "tname" ~> "pairs" ~> "term" ~>
  "stripped" <~ Strip.deannotateAndDetypeTerm @@ var "term" $
  "mapping" <~ Maps.fromList (var "pairs") $
  cases _Term (var "stripped")
    (Just (Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [
        string "expected inject(", Core.unName (var "tname"),
        string ") with one of {",
        (Strings.intercalate (string ", ") (Lists.map ("pair" ~> Core.unName (Pairs.first (var "pair"))) (var "pairs"))),
        string "}, got ",
        (ShowCore.term @@ var "stripped")])) (var "cx"))) [
    _Term_variable>>: "name" ~>
      "el" <<~ requireBinding @@ var "cx" @@ var "graph" @@ var "name" $
      matchUnion @@ var "cx" @@ var "graph" @@ var "tname" @@ var "pairs" @@ (Core.bindingTerm (var "el")),
    _Term_union>>: "injection" ~>
      "exp" <~ (
        "fname" <~ Core.fieldName (Core.injectionField (var "injection")) $
        "val" <~ Core.fieldTerm (Core.injectionField (var "injection")) $
        Maybes.maybe
          (Ctx.failInContext (Error.errorOther $ Error.otherError ((string "no matching case for field \"") ++ (Core.unName (var "fname"))
            ++ (string "\" in union type ") ++ (Core.unName (var "tname")))) (var "cx"))
          ("f" ~> var "f" @@ var "val")
          (Maps.lookup (var "fname") (var "mapping"))) $
      Logic.ifElse (Core.equalName_ (Core.injectionTypeName (var "injection")) (var "tname"))
        (var "exp")
        (Ctx.failInContext (Error.errorOther $ Error.otherError ((string "expected injection for type ") ++ (Core.unName (var "tname"))
          ++ (string ", got ") ++ (ShowCore.term @@ var "term"))) (var "cx"))]

matchUnitField :: TTermDefinition (Name -> y -> (Name, x -> Either (InContext Error) y))
matchUnitField = define "matchUnitField" $
  "fname" ~> "x" ~> pair (var "fname") ("ignored" ~> right (var "x"))

requireBinding :: TTermDefinition (Context -> Graph -> Name -> Either (InContext Error) Binding)
requireBinding = define "requireBinding" $
  "cx" ~> "graph" ~> "name" ~>
  "showAll" <~ false $
  "ellipsis" <~ ("strings" ~>
    Logic.ifElse (Logic.and (Equality.gt (Lists.length (var "strings")) (int32 3)) (Logic.not (var "showAll")))
      (Lists.concat2 (Lists.take (int32 3) (var "strings")) (list [string "..."]))
      (var "strings")) $
  "errMsg" <~ (
    (string "no such element: ") ++ (Core.unName (var "name")) ++
    (string ". Available elements: {") ++
    (Strings.intercalate (string ", ") (var "ellipsis" @@ (Lists.map (unaryFunction Core.unName) (Maps.keys (Graph.graphBoundTerms (var "graph")))))) ++
    (string "}")) $
  Maybes.maybe
    (Ctx.failInContext (Error.errorOther $ Error.otherError (var "errMsg")) (var "cx"))
    (unaryFunction right)
    (lookupBinding @@ var "graph" @@ var "name")

requirePrimitive :: TTermDefinition (Context -> Graph -> Name -> Either (InContext Error) Primitive)
requirePrimitive = define "requirePrimitive" $
  "cx" ~> "graph" ~> "name" ~>
  Maybes.maybe
    (Ctx.failInContext (Error.errorOther $ Error.otherError ((string "no such primitive function: ") ++ (Core.unName (var "name")))) (var "cx"))
    (unaryFunction right)
    (lookupPrimitive @@ var "graph" @@ var "name")

requirePrimitiveType :: TTermDefinition (Context -> Graph -> Name -> Either (InContext Error) TypeScheme)
requirePrimitiveType = define "requirePrimitiveType" $
  "cx" ~> "tx" ~> "name" ~>
  -- Look up the primitive directly and extract its type, avoiding O(p) map reconstruction.
  "mts" <~ Maybes.map ("_p" ~> Graph.primitiveType (var "_p"))
    (Maps.lookup (var "name") (Graph.graphPrimitives $ var "tx")) $
  optCases (var "mts")
    (Ctx.failInContext (Error.errorOther $ Error.otherError ((string "no such primitive function: ") ++ (Core.unName (var "name")))) (var "cx"))
    ("ts" ~> right $ var "ts")

requireTerm :: TTermDefinition (Context -> Graph -> Name -> Either (InContext Error) Term)
requireTerm = define "requireTerm" $
  "cx" ~> "graph" ~> "name" ~>
  Maybes.maybe
    (Ctx.failInContext (Error.errorOther $ Error.otherError ((string "no such element: ") ++ (Core.unName (var "name")))) (var "cx"))
    (unaryFunction right)
    (resolveTerm @@ var "graph" @@ var "name")

resolveTerm :: TTermDefinition (Graph -> Name -> Maybe Term)
resolveTerm = define "resolveTerm" $
  doc "TODO: distinguish between lambda-bound and let-bound variables" $
  "graph" ~> "name" ~>
  "recurse" <~ ("term" ~>
    "stripped" <~ Strip.deannotateTerm @@ var "term" $
    cases _Term (var "stripped")
      (Just (just (var "term"))) [
      _Term_variable>>: "name'" ~> resolveTerm @@ var "graph" @@ var "name'"]) $
  Maybes.maybe
    nothing
    (var "recurse")
    (lookupTerm @@ var "graph" @@ var "name")

stripAndDereferenceTerm :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
stripAndDereferenceTerm = define "stripAndDereferenceTerm" $
  "cx" ~> "graph" ~> "term" ~>
  "stripped" <~ Strip.deannotateAndDetypeTerm @@ var "term" $
  cases _Term (var "stripped")
    (Just (right (var "stripped"))) [
    _Term_variable>>: "v" ~>
      Eithers.bind (requireTerm @@ var "cx" @@ var "graph" @@ var "v") (
        "t" ~> stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "t")]

stripAndDereferenceTermEither :: TTermDefinition (Graph -> Term -> Either String Term)
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


