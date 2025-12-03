{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Lexical where

-- Standard imports for kernel terms modules
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


module_ :: Module
module_ = Module (Namespace "hydra.lexical") elements
   [Monads.module_, Rewriting.module_, ShowCore.module_]
    kernelTypesModules $
    Just ("A module for lexical operations over graphs.")
  where
    elements = [
      el chooseUniqueNameDef,
      el dereferenceElementDef,
      el dereferenceSchemaTypeDef,
      el elementsToGraphDef,
      el emptyGraphDef,
      el extendGraphWithBindingsDef,
      el fieldsOfDef,
      el getFieldDef,
      el lookupElementDef,
      el lookupPrimitiveDef,
      el matchEnumDef,
      el matchRecordDef,
      el matchUnionDef,
      el matchUnitFieldDef,
      el requireElementDef,
      el requirePrimitiveDef,
      el requirePrimitiveTypeDef,
      el requireTermDef,
      el resolveTermDef,
      el schemaContextDef,
      el stripAndDereferenceTermDef,
      el withEmptyGraphDef,
      el withSchemaContextDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

chooseUniqueNameDef :: TBinding (S.Set Name -> Name -> Name)
chooseUniqueNameDef = define "chooseUniqueName" $
  "reserved" ~> "name" ~>
  "tryName" <~ ("index" ~>
    "candidate" <~ Logic.ifElse (Equality.equal (var "index") (int32 1))
      (var "name")
      (Core.name $ (Core.unName (var "name") ++ Literals.showInt32 (var "index"))) $
    Logic.ifElse (Sets.member (var "candidate") (var "reserved"))
      (var "tryName" @@ (Math.add (var "index") (int32 1)))
      (var "candidate")) $
  var "tryName" @@ (int32 1)

dereferenceElementDef :: TBinding (Name -> Flow Graph (Maybe Binding))
dereferenceElementDef = define "dereferenceElement" $
  doc "Look up an element in the current graph context" $
  "name" ~> Flows.map
    ("g" ~> ref lookupElementDef @@ var "g" @@ var "name")
    (ref Monads.getStateDef)

dereferenceSchemaTypeDef :: TBinding (Name -> M.Map Name TypeScheme -> Maybe TypeScheme)
dereferenceSchemaTypeDef = define "dereferenceSchemaType" $
  doc "Resolve a schema type through a chain of zero or more typedefs" $
  "name" ~> "types" ~>
  "forType" <~ ("t" ~> cases _Type (var "t")
    (Just (just (Core.typeScheme (list []) (var "t")))) [
    _Type_annotated>>: "at" ~> var "forType" @@ (Core.annotatedTypeBody (var "at")),
    _Type_forall>>: "ft" ~> Maybes.map
      ("ts" ~> Core.typeScheme
        -- Note: no alpha-renaming of type variables
        (Lists.cons (Core.forallTypeParameter (var "ft")) (Core.typeSchemeVariables (var "ts")))
        (Core.typeSchemeType (var "ts")))
      (var "forType" @@ (Core.forallTypeBody (var "ft"))),
    _Type_variable>>: "v" ~> ref dereferenceSchemaTypeDef @@ var "v" @@ var "types"]) $
  Maybes.bind
    (Maps.lookup (var "name") (var "types"))
    ("ts" ~> Maybes.map
      ("ts2" ~> Core.typeScheme
        -- Note: no alpha-renaming of type variables
        (Lists.concat2 (Core.typeSchemeVariables (var "ts")) (Core.typeSchemeVariables (var "ts2")))
        (Core.typeSchemeType (var "ts2")))
      (var "forType" @@ (Core.typeSchemeType (var "ts"))))

elementsToGraphDef :: TBinding (Graph -> Maybe Graph -> [Binding] -> Graph)
elementsToGraphDef = define "elementsToGraph" $
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

emptyGraphDef :: TBinding Graph
emptyGraphDef = define "emptyGraph" $
  doc "An empty graph; no elements, no primitives, no schema, and an arbitrary body." $
  Graph.graph
    Maps.empty
    Maps.empty
    Maps.empty
    (Core.termLiteral (Core.literalString "empty graph"))
    Maps.empty
    nothing

extendGraphWithBindingsDef :: TBinding ([Binding] -> Graph -> Graph)
extendGraphWithBindingsDef = define "extendGraphWithBindings" $
  doc "Add bindings to an existing graph" $
  "bindings" ~> "g" ~>
  "toEl" <~ ("binding" ~>
    "name" <~ Core.bindingName (var "binding") $
    "term" <~ Core.bindingTerm (var "binding") $
    "mts" <~ Core.bindingType (var "binding") $
    pair (var "name") (Core.binding (var "name") (var "term") (var "mts"))) $
  "newEls" <~ Maps.fromList (Lists.map (var "toEl") (var "bindings")) $
  Graph.graphWithElements (var "g") (Maps.union (var "newEls") (Graph.graphElements (var "g")))

fieldsOfDef :: TBinding (Type -> [FieldType])
fieldsOfDef = define "fieldsOf" $
  doc "Extract the fields of a record or union type" $
  "t" ~>
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "t" $
  cases _Type (var "stripped")
    (Just (list [])) [
    _Type_forall>>: "forallType" ~> ref fieldsOfDef @@ (Core.forallTypeBody (var "forallType")),
    _Type_record>>: "rt" ~> Core.rowTypeFields (var "rt"),
    _Type_union>>: "rt" ~> Core.rowTypeFields (var "rt")]

getFieldDef :: TBinding (M.Map Name Term -> Name -> (Term -> Flow Graph b) -> Flow Graph b)
getFieldDef = define "getField" $
  "m" ~> "fname" ~> "decode" ~>
  Maybes.maybe
    (Flows.fail ("expected field " ++ (Core.unName (var "fname")) ++ " not found"))
    (var "decode")
    (Maps.lookup (var "fname") (var "m"))

lookupElementDef :: TBinding (Graph -> Name -> Maybe Binding)
lookupElementDef = define "lookupElement" $
  "g" ~> "name" ~> Maps.lookup (var "name") (Graph.graphElements (var "g"))

lookupPrimitiveDef :: TBinding (Graph -> Name -> Maybe Primitive)
lookupPrimitiveDef = define "lookupPrimitive" $
  "g" ~> "name" ~>
  Maps.lookup (var "name") (Graph.graphPrimitives (var "g"))

matchEnumDef :: TBinding (Name -> [(Name, b)] -> Term -> Flow Graph b)
matchEnumDef = define "matchEnum" $
  "tname" ~> "pairs" ~>
  ref matchUnionDef @@ var "tname" @@ (Lists.map ("pair" ~>
    ref matchUnitFieldDef @@ (Pairs.first (var "pair")) @@ (Pairs.second (var "pair"))) (var "pairs"))

matchRecordDef :: TBinding ((M.Map Name Term -> Flow Graph b) -> Term -> Flow Graph b)
matchRecordDef = define "matchRecord" $
  "decode" ~> "term" ~>
  "stripped" <~ ref Rewriting.deannotateAndDetypeTermDef @@ var "term" $
  cases _Term (var "stripped")
    (Just (ref Monads.unexpectedDef @@ "record" @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_record>>: "record" ~> var "decode" @@
      (Maps.fromList (Lists.map
        ("field" ~> pair (Core.fieldName (var "field")) (Core.fieldTerm (var "field")))
        (Core.recordFields (var "record"))))]

matchUnionDef :: TBinding (Name -> [(Name, Term -> Flow Graph b)] -> Term -> Flow Graph b)
matchUnionDef = define "matchUnion" $
  "tname" ~> "pairs" ~> "term" ~>
  "stripped" <~ ref Rewriting.deannotateAndDetypeTermDef @@ var "term" $
  "mapping" <~ Maps.fromList (var "pairs") $
--  trace "match union" $
  cases _Term (var "stripped")
    (Just (ref Monads.unexpectedDef @@
      (Strings.cat $ list [
        "inject(", Core.unName (var "tname"),
        ") with one of {",
        (Strings.intercalate ", " (Lists.map ("pair" ~> Core.unName (Pairs.first (var "pair"))) (var "pairs"))),
        "}"]) @@
      (ref ShowCore.termDef @@ var "stripped"))) [
    _Term_variable>>: "name" ~>
      "el" <<~ ref requireElementDef @@ var "name" $
      ref matchUnionDef @@ var "tname" @@ var "pairs" @@ (Core.bindingTerm (var "el")),
    _Term_union>>: "injection" ~>
      "exp" <~ (
        "fname" <~ Core.fieldName (Core.injectionField (var "injection")) $
        "val" <~ Core.fieldTerm (Core.injectionField (var "injection")) $
        Maybes.maybe
          (Flows.fail ("no matching case for field " ++ (Core.unName (var "fname"))
            ++ " in union type " ++ (Core.unName (var "tname"))))
          ("f" ~> var "f" @@ var "val")
          (Maps.lookup (var "fname") (var "mapping"))) $
      Logic.ifElse (Core.equalName_ (Core.injectionTypeName (var "injection")) (var "tname"))
        (var "exp")
        (ref Monads.unexpectedDef @@ ("injection for type " ++ (Core.unName (var "tname"))) @@ (ref ShowCore.termDef @@ var "term"))]

matchUnitFieldDef :: TBinding (Name -> y -> (Name, x -> Flow Graph y))
matchUnitFieldDef = define "matchUnitField" $
  "fname" ~> "x" ~> pair (var "fname") ("ignored" ~> produce (var "x"))

requireElementDef :: TBinding (Name -> Flow Graph Binding)
requireElementDef = define "requireElement" $
  "name" ~>
  "showAll" <~ false $
  "ellipsis" <~ ("strings" ~>
    Logic.ifElse (Logic.and (Equality.gt (Lists.length (var "strings")) (int32 3)) (Logic.not (var "showAll")))
      (Lists.concat2 (Lists.take (int32 3) (var "strings")) (list ["..."]))
      (var "strings")) $
  "err" <~ ("g" ~> Flows.fail (
    "no such element: " ++ (Core.unName (var "name")) ++
    ". Available elements: {" ++
    (Strings.intercalate ", " (var "ellipsis" @@ (Lists.map ("el" ~> Core.unName (Core.bindingName (var "el"))) (Maps.elems (Graph.graphElements (var "g")))))) ++
    "}")) $
  "mel" <<~ ref dereferenceElementDef @@ var "name" $
  Maybes.maybe
    ("g" <<~ ref Monads.getStateDef $ var "err" @@ var "g")
    (unaryFunction Flows.pure)
    (var "mel")

requirePrimitiveDef :: TBinding (Name -> Flow Graph Primitive)
requirePrimitiveDef = define "requirePrimitive" $
  "name" ~>
  "g" <<~ ref Monads.getStateDef $
  Maybes.maybe
    (Flows.fail ("no such primitive function: " ++ (Core.unName (var "name"))))
    (unaryFunction Flows.pure)
    (ref lookupPrimitiveDef @@ var "g" @@ var "name")

requirePrimitiveTypeDef :: TBinding (TypeContext -> Name -> Flow s TypeScheme)
requirePrimitiveTypeDef = define "requirePrimitiveType" $
  "tx" ~> "name" ~>
  "mts" <~ Maps.lookup
    (var "name" )
    (Typing.inferenceContextPrimitiveTypes $ Typing.typeContextInferenceContext $ var "tx") $
  optCases (var "mts")
    (Flows.fail ("no such primitive function: " ++ (Core.unName (var "name"))))
    ("ts" ~> produce $ var "ts")

requireTermDef :: TBinding (Name -> Flow Graph Term)
requireTermDef = define "requireTerm" $
  "name" ~>
  "mt" <<~ ref resolveTermDef @@ var "name" $
  Maybes.maybe
    (Flows.fail ("no such element: " ++ (Core.unName (var "name"))))
    (unaryFunction Flows.pure)
    (var "mt")

resolveTermDef :: TBinding (Name -> Flow Graph (Maybe Term))
resolveTermDef = define "resolveTerm" $
  doc "TODO: distinguish between lambda-bound and let-bound variables" $
  "name" ~>
  "recurse" <~ ("el" ~>
    "stripped" <~ ref Rewriting.deannotateTermDef @@ (Core.bindingTerm (var "el")) $
    cases _Term (var "stripped")
      (Just (produce (just (Core.bindingTerm (var "el"))))) [
      _Term_variable>>: "name'" ~> ref resolveTermDef @@ var "name'"]) $
  "g" <<~ ref Monads.getStateDef $
  Maybes.maybe
    (produce nothing)
    (var "recurse")
    (Maps.lookup (var "name") (Graph.graphElements (var "g")))

schemaContextDef :: TBinding (Graph -> Graph)
schemaContextDef = define "schemaContext" $
  doc "Note: assuming for now that primitive functions are the same in the schema graph" $
  "g" ~> Maybes.fromMaybe (var "g") (Graph.graphSchema (var "g"))

stripAndDereferenceTermDef :: TBinding (Term -> Flow Graph Term)
stripAndDereferenceTermDef = define "stripAndDereferenceTerm" $
  "term" ~>
  "stripped" <~ ref Rewriting.deannotateAndDetypeTermDef @@ var "term" $
  cases _Term (var "stripped")
    (Just (produce (var "stripped"))) [
    _Term_variable>>: "v" ~>
      "t" <<~ ref requireTermDef @@ var "v" $
      ref stripAndDereferenceTermDef @@ var "t"]

-- TODO: move into hydra.lexical
withEmptyGraphDef :: TBinding (Flow Graph a -> Flow s a)
withEmptyGraphDef = define "withEmptyGraph" $
  doc "Execute flow with empty graph" $
  ref Monads.withStateDef @@ ref emptyGraphDef

withSchemaContextDef :: TBinding (Flow Graph x -> Flow Graph x)
withSchemaContextDef = define "withSchemaContext" $
  "f" ~>
  "g" <<~ ref Monads.getStateDef $
  ref Monads.withStateDef @@ (ref schemaContextDef @@ var "g") @@ var "f"
