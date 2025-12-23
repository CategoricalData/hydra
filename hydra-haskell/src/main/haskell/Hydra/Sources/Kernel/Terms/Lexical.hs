
module Hydra.Sources.Kernel.Terms.Lexical where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (chooseUniqueName, dereferenceElement, dereferenceSchemaType, dereferenceVariable, elementsToGraph, emptyGraph, extendGraphWithBindings, fieldsOf, getField, lookupElement, lookupPrimitive, matchEnum, matchRecord, matchUnion, matchUnitField, requireElement, requirePrimitive, requirePrimitiveType, requireTerm, resolveTerm, schemaContext, stripAndDereferenceTerm, stripAndDereferenceTermEither, withEmptyGraph, withSchemaContext)
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
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.lexical"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
   [Monads.ns, Rewriting.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("A module for lexical operations over graphs.")
  where
    elements = [
      toBinding chooseUniqueName,
      toBinding dereferenceElement,
      toBinding dereferenceSchemaType,
      toBinding dereferenceVariable,
      toBinding elementsToGraph,
      toBinding emptyGraph,
      toBinding extendGraphWithBindings,
      toBinding fieldsOf,
      toBinding getField,
      toBinding lookupElement,
      toBinding lookupPrimitive,
      toBinding matchEnum,
      toBinding matchRecord,
      toBinding matchUnion,
      toBinding matchUnitField,
      toBinding requireElement,
      toBinding requirePrimitive,
      toBinding requirePrimitiveType,
      toBinding requireTerm,
      toBinding resolveTerm,
      toBinding schemaContext,
      toBinding stripAndDereferenceTerm,
      toBinding stripAndDereferenceTermEither,
      toBinding withEmptyGraph,
      toBinding withSchemaContext]

chooseUniqueName :: TBinding (S.Set Name -> Name -> Name)
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

dereferenceElement :: TBinding (Name -> Flow Graph (Maybe Binding))
dereferenceElement = define "dereferenceElement" $
  doc "Look up an element in the current graph context" $
  "name" ~> Flows.map
    ("g" ~> lookupElement @@ var "g" @@ var "name")
    (Monads.getState)

dereferenceSchemaType :: TBinding (Name -> M.Map Name TypeScheme -> Maybe TypeScheme)
dereferenceSchemaType = define "dereferenceSchemaType" $
  doc "Resolve a schema type through a chain of zero or more typedefs" $
  "name" ~> "types" ~>
  "forType" <~ ("t" ~> cases _Type (var "t")
    (Just (just (Core.typeScheme (list ([] :: [TTerm Name])) (var "t")))) [
    _Type_annotated>>: "at" ~> var "forType" @@ (Core.annotatedTypeBody (var "at")),
    _Type_forall>>: "ft" ~> Maybes.map
      ("ts" ~> Core.typeScheme
        -- Note: no alpha-renaming of type variables
        (Lists.cons (Core.forallTypeParameter (var "ft")) (Core.typeSchemeVariables (var "ts")))
        (Core.typeSchemeType (var "ts")))
      (var "forType" @@ (Core.forallTypeBody (var "ft"))),
    _Type_variable>>: "v" ~> dereferenceSchemaType @@ var "v" @@ var "types"]) $
  Maybes.bind
    (Maps.lookup (var "name") (var "types"))
    ("ts" ~> Maybes.map
      ("ts2" ~> Core.typeScheme
        -- Note: no alpha-renaming of type variables
        (Lists.concat2 (Core.typeSchemeVariables (var "ts")) (Core.typeSchemeVariables (var "ts2")))
        (Core.typeSchemeType (var "ts2")))
      (var "forType" @@ (Core.typeSchemeType (var "ts"))))

-- | Dereference a variable name in a graph, returning Either an error message or the binding
dereferenceVariable :: TBinding (Graph -> Name -> Either String Binding)
dereferenceVariable = define "dereferenceVariable" $
  doc "Look up an element by name in a graph, returning Either an error or the binding" $
  "g" ~> "name" ~>
  Maybes.maybe
    (left ((string "no such element: ") ++ (Core.unName (var "name"))))
    right_
    (lookupElement @@ var "g" @@ var "name")

elementsToGraph :: TBinding (Graph -> Maybe Graph -> [Binding] -> Graph)
elementsToGraph = define "elementsToGraph" $
  doc "Create a graph from a parent graph, optional schema, and list of element bindings" $
  "parent" ~> "schema" ~> "elements" ~>
  "toPair" <~ ("el" ~> pair (Core.bindingName (var "el")) (var "el")) $
  Graph.graph
    (Maps.fromList (Lists.map (var "toPair") (var "elements")))
    (Graph.graphEnvironment (var "parent"))
    (Graph.graphTypes (var "parent"))
    (Graph.graphBody (var "parent"))
    (Graph.graphPrimitives (var "parent"))
    (var "schema")

emptyGraph :: TBinding Graph
emptyGraph = define "emptyGraph" $
  doc "An empty graph; no elements, no primitives, no schema, and an arbitrary body." $
  Graph.graph
    Maps.empty
    Maps.empty
    Maps.empty
    (Core.termLiteral (Core.literalString (string "empty graph")))
    Maps.empty
    nothing

extendGraphWithBindings :: TBinding ([Binding] -> Graph -> Graph)
extendGraphWithBindings = define "extendGraphWithBindings" $
  doc "Add bindings to an existing graph" $
  "bindings" ~> "g" ~>
  "toEl" <~ ("binding" ~>
    "name" <~ Core.bindingName (var "binding") $
    "term" <~ Core.bindingTerm (var "binding") $
    "mts" <~ Core.bindingType (var "binding") $
    pair (var "name") (Core.binding (var "name") (var "term") (var "mts"))) $
  "newEls" <~ Maps.fromList (Lists.map (var "toEl") (var "bindings")) $
  Graph.graphWithElements (var "g") (Maps.union (var "newEls") (Graph.graphElements (var "g")))

fieldsOf :: TBinding (Type -> [FieldType])
fieldsOf = define "fieldsOf" $
  doc "Extract the fields of a record or union type" $
  "t" ~>
  "stripped" <~ Rewriting.deannotateType @@ var "t" $
  cases _Type (var "stripped")
    (Just (list ([] :: [TTerm FieldType]))) [
    _Type_forall>>: "forallType" ~> fieldsOf @@ (Core.forallTypeBody (var "forallType")),
    _Type_record>>: "rt" ~> Core.rowTypeFields (var "rt"),
    _Type_union>>: "rt" ~> Core.rowTypeFields (var "rt")]

getField :: TBinding (M.Map Name Term -> Name -> (Term -> Flow Graph b) -> Flow Graph b)
getField = define "getField" $
  "m" ~> "fname" ~> "decode" ~>
  Maybes.maybe
    (Flows.fail ((string "expected field ") ++ (Core.unName (var "fname")) ++ (string " not found")))
    (var "decode")
    (Maps.lookup (var "fname") (var "m"))

lookupElement :: TBinding (Graph -> Name -> Maybe Binding)
lookupElement = define "lookupElement" $
  "g" ~> "name" ~> Maps.lookup (var "name") (Graph.graphElements (var "g"))

lookupPrimitive :: TBinding (Graph -> Name -> Maybe Primitive)
lookupPrimitive = define "lookupPrimitive" $
  "g" ~> "name" ~>
  Maps.lookup (var "name") (Graph.graphPrimitives (var "g"))

matchEnum :: TBinding (Name -> [(Name, b)] -> Term -> Flow Graph b)
matchEnum = define "matchEnum" $
  "tname" ~> "pairs" ~>
  matchUnion @@ var "tname" @@ (Lists.map ("pair" ~>
    matchUnitField @@ (Pairs.first (var "pair")) @@ (Pairs.second (var "pair"))) (var "pairs"))

matchRecord :: TBinding ((M.Map Name Term -> Flow Graph b) -> Term -> Flow Graph b)
matchRecord = define "matchRecord" $
  "decode" ~> "term" ~>
  "stripped" <~ Rewriting.deannotateAndDetypeTerm @@ var "term" $
  cases _Term (var "stripped")
    (Just (Monads.unexpected @@ (string "record") @@ (ShowCore.term @@ var "term"))) [
    _Term_record>>: "record" ~> var "decode" @@
      (Maps.fromList (Lists.map
        ("field" ~> pair (Core.fieldName (var "field")) (Core.fieldTerm (var "field")))
        (Core.recordFields (var "record"))))]

matchUnion :: TBinding (Name -> [(Name, Term -> Flow Graph b)] -> Term -> Flow Graph b)
matchUnion = define "matchUnion" $
  "tname" ~> "pairs" ~> "term" ~>
  "stripped" <~ Rewriting.deannotateAndDetypeTerm @@ var "term" $
  "mapping" <~ Maps.fromList (var "pairs") $
--  trace "match union" $
  cases _Term (var "stripped")
    (Just (Monads.unexpected @@
      (Strings.cat $ list [
        string "inject(", Core.unName (var "tname"),
        string ") with one of {",
        (Strings.intercalate (string ", ") (Lists.map ("pair" ~> Core.unName (Pairs.first (var "pair"))) (var "pairs"))),
        string "}"]) @@
      (ShowCore.term @@ var "stripped"))) [
    _Term_variable>>: "name" ~>
      "el" <<~ requireElement @@ var "name" $
      matchUnion @@ var "tname" @@ var "pairs" @@ (Core.bindingTerm (var "el")),
    _Term_union>>: "injection" ~>
      "exp" <~ (
        "fname" <~ Core.fieldName (Core.injectionField (var "injection")) $
        "val" <~ Core.fieldTerm (Core.injectionField (var "injection")) $
        Maybes.maybe
          (Flows.fail ((string "no matching case for field \"") ++ (Core.unName (var "fname"))
            ++ (string "\" in union type ") ++ (Core.unName (var "tname"))))
          ("f" ~> var "f" @@ var "val")
          (Maps.lookup (var "fname") (var "mapping"))) $
      Logic.ifElse (Core.equalName_ (Core.injectionTypeName (var "injection")) (var "tname"))
        (var "exp")
        (Monads.unexpected @@ ((string "injection for type ") ++ (Core.unName (var "tname"))) @@ (ShowCore.term @@ var "term"))]

matchUnitField :: TBinding (Name -> y -> (Name, x -> Flow Graph y))
matchUnitField = define "matchUnitField" $
  "fname" ~> "x" ~> pair (var "fname") ("ignored" ~> produce (var "x"))

requireElement :: TBinding (Name -> Flow Graph Binding)
requireElement = define "requireElement" $
  "name" ~>
  "showAll" <~ false $
  "ellipsis" <~ ("strings" ~>
    Logic.ifElse (Logic.and (Equality.gt (Lists.length (var "strings")) (int32 3)) (Logic.not (var "showAll")))
      (Lists.concat2 (Lists.take (int32 3) (var "strings")) (list [string "..."]))
      (var "strings")) $
  "err" <~ ("g" ~> Flows.fail (
    (string "no such element: ") ++ (Core.unName (var "name")) ++
    (string ". Available elements: {") ++
    (Strings.intercalate (string ", ") (var "ellipsis" @@ (Lists.map ("el" ~> Core.unName (Core.bindingName (var "el"))) (Maps.elems (Graph.graphElements (var "g")))))) ++
    (string "}"))) $
  "mel" <<~ dereferenceElement @@ var "name" $
  Maybes.maybe
    ("g" <<~ Monads.getState $ var "err" @@ var "g")
    (unaryFunction Flows.pure)
    (var "mel")

requirePrimitive :: TBinding (Name -> Flow Graph Primitive)
requirePrimitive = define "requirePrimitive" $
  "name" ~>
  "g" <<~ Monads.getState $
  Maybes.maybe
    (Flows.fail ((string "no such primitive function: ") ++ (Core.unName (var "name"))))
    (unaryFunction Flows.pure)
    (lookupPrimitive @@ var "g" @@ var "name")

requirePrimitiveType :: TBinding (TypeContext -> Name -> Flow s TypeScheme)
requirePrimitiveType = define "requirePrimitiveType" $
  "tx" ~> "name" ~>
  "mts" <~ Maps.lookup
    (var "name" )
    (Typing.inferenceContextPrimitiveTypes $ Typing.typeContextInferenceContext $ var "tx") $
  optCases (var "mts")
    (Flows.fail ((string "no such primitive function: ") ++ (Core.unName (var "name"))))
    ("ts" ~> produce $ var "ts")

requireTerm :: TBinding (Name -> Flow Graph Term)
requireTerm = define "requireTerm" $
  "name" ~>
  "mt" <<~ resolveTerm @@ var "name" $
  Maybes.maybe
    (Flows.fail ((string "no such element: ") ++ (Core.unName (var "name"))))
    (unaryFunction Flows.pure)
    (var "mt")

resolveTerm :: TBinding (Name -> Flow Graph (Maybe Term))
resolveTerm = define "resolveTerm" $
  doc "TODO: distinguish between lambda-bound and let-bound variables" $
  "name" ~>
  "recurse" <~ ("el" ~>
    "stripped" <~ Rewriting.deannotateTerm @@ (Core.bindingTerm (var "el")) $
    cases _Term (var "stripped")
      (Just (produce (just (Core.bindingTerm (var "el"))))) [
      _Term_variable>>: "name'" ~> resolveTerm @@ var "name'"]) $
  "g" <<~ Monads.getState $
  Maybes.maybe
    (produce nothing)
    (var "recurse")
    (Maps.lookup (var "name") (Graph.graphElements (var "g")))

schemaContext :: TBinding (Graph -> Graph)
schemaContext = define "schemaContext" $
  doc "Note: assuming for now that primitive functions are the same in the schema graph" $
  "g" ~> Maybes.fromMaybe (var "g") (Graph.graphSchema (var "g"))

stripAndDereferenceTerm :: TBinding (Term -> Flow Graph Term)
stripAndDereferenceTerm = define "stripAndDereferenceTerm" $
  "term" ~>
  "stripped" <~ Rewriting.deannotateAndDetypeTerm @@ var "term" $
  cases _Term (var "stripped")
    (Just (produce (var "stripped"))) [
    _Term_variable>>: "v" ~>
      "t" <<~ requireTerm @@ var "v" $
      stripAndDereferenceTerm @@ var "t"]

-- | Strip annotations and dereference variables, returning Either String Term
-- This is the pure (non-Flow) version for use in generated decoders
stripAndDereferenceTermEither :: TBinding (Graph -> Term -> Either String Term)
stripAndDereferenceTermEither = define "stripAndDereferenceTermEither" $
  doc "Strip annotations and dereference variables, returning Either an error or the resolved term" $
  "g" ~> "term" ~>
  "stripped" <~ Rewriting.deannotateAndDetypeTerm @@ var "term" $
  cases _Term (var "stripped")
    (Just (right (var "stripped"))) [
    _Term_variable>>: "v" ~>
      Eithers.either_
        left_  -- propagate error
        ("binding" ~> stripAndDereferenceTermEither @@ var "g" @@ (Core.bindingTerm (var "binding")))
        (dereferenceVariable @@ var "g" @@ var "v")]

-- TODO: move into hydra.lexical
withEmptyGraph :: TBinding (Flow Graph a -> Flow s a)
withEmptyGraph = define "withEmptyGraph" $
  doc "Execute flow with empty graph" $
  Monads.withState @@ emptyGraph

withSchemaContext :: TBinding (Flow Graph x -> Flow Graph x)
withSchemaContext = define "withSchemaContext" $
  "f" ~>
  "g" <<~ Monads.getState $
  Monads.withState @@ (schemaContext @@ var "g") @@ var "f"
