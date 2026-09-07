{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Subterms where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Overlay.Haskell.Libraries
import qualified Hydra.Dsl.Paths        as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Checking as Checking
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Reflect as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution
import qualified Hydra.Dsl.Errors as Error


ns :: ModuleName
ns = ModuleName "hydra.subterms"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$>
              ([Checking.ns, ExtractCore.ns, Lexical.ns, Reflect.ns, Rewriting.ns, Substitution.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just
              ("The term-graph view of a typed graph: nodes are subterms, sharing is explicit, children are"
               <> " inline (a tree with symbolic cross-links). graphToTermGraph builds it; schemaToTypeGraph"
               <> " is the type-side twin. Term graphs are Ariola-Klop's nested systems of recursion equations."))}
  where
   definitions = [
     toDefinition expectedForStep,
     toDefinition fieldTypeFromSchema,
     toDefinition findFieldTypeIn,
     toDefinition graphToTermGraph,
     toDefinition rootExpectedType,
     toDefinition schemaToTypeGraph,
     toDefinition synthesizeType,
     toDefinition termAttributesOf,
     toDefinition termGraphNode,
     toDefinition termReferenceFor,
     toDefinition typeAttributesOf,
     toDefinition typeGraphNode,
     toDefinition typeLinks,
     toDefinition typeReferenceFor]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

expectedForStep :: TypedTermDefinition (M.Map Name Type -> Type -> Term -> SubtermStep -> Maybe Type)
expectedForStep = define "expectedForStep" $
  doc ("The expected type to thread into an immediate child, given this node's (synthesized) type, its"
    <> " term, and the step reaching the child. This is the term->type position map underlying the"
    <> " TermGraph:TypeGraph erasure: element positions get the container element type, mapEntry gets"
    <> " pair<K,V>, lambdaBody the codomain, and so on. `none` where the child's type is not fixed"
    <> " top-down (e.g. a function-position subterm), which is fine for non-empty children.") $
  "schema" ~> "typ" ~> "term" ~> "step" ~>
  "elementOfList" <~ ("t" ~> match _Type (var "t") (Just nothing) [_Type_list>>: "e" ~> just (var "e")]) $
  "elementOfSet" <~ ("t" ~> match _Type (var "t") (Just nothing) [_Type_set>>: "e" ~> just (var "e")]) $
  "elementOfOptional" <~ ("t" ~> match _Type (var "t") (Just nothing) [_Type_optional>>: "e" ~> just (var "e")]) $
  "codomainOf" <~ ("t" ~> match _Type (var "t") (Just nothing) [_Type_function>>: "ft" ~> just (Core.functionTypeCodomain $ var "ft")]) $
  "mapPairType" <~ ("t" ~> match _Type (var "t") (Just nothing) [
    _Type_map>>: "mt" ~> just (Core.typePair $ Core.pairType (Core.mapTypeKeys $ var "mt") (Core.mapTypeValues $ var "mt"))]) $
  match _SubtermStep (var "step") (Just nothing) [
    _SubtermStep_listElement>>: constant (var "elementOfList" @@ var "typ"),
    _SubtermStep_setElement>>: constant (var "elementOfSet" @@ var "typ"),
    _SubtermStep_optionalGiven>>: constant (var "elementOfOptional" @@ var "typ"),
    _SubtermStep_lambdaBody>>: constant (var "codomainOf" @@ var "typ"),
    _SubtermStep_mapEntry>>: constant (var "mapPairType" @@ var "typ"),
    -- pair components: this node's type is pair<A,B>
    _SubtermStep_pairFirst>>: constant (match _Type (var "typ") (Just nothing) [_Type_pair>>: "pt" ~> just (Core.pairTypeFirst $ var "pt")]),
    _SubtermStep_pairSecond>>: constant (match _Type (var "typ") (Just nothing) [_Type_pair>>: "pt" ~> just (Core.pairTypeSecond $ var "pt")]),
    -- recordField / injectField: field type from the schema type of the record/union
    _SubtermStep_recordField>>: "fname" ~> fieldTypeFromSchema @@ var "schema" @@ var "term" @@ var "fname",
    _SubtermStep_injectField>>: "fname" ~> fieldTypeFromSchema @@ var "schema" @@ var "term" @@ var "fname"]

-- The expected type of a record field / injected union field, from the named type in the schema. Used to
-- thread `expected` into an empty collection nested directly in a record field or union injection.
fieldTypeFromSchema :: TypedTermDefinition (M.Map Name Type -> Term -> Name -> Maybe Type)
fieldTypeFromSchema = define "fieldTypeFromSchema" $
  doc "The expected type of a record field / injected union field, from the named type in the schema" $
  "schema" ~> "term" ~> "fname" ~>
  -- Resolve (typeName, fields-extractor) for a record or inject parent; other parents contribute nothing.
  match _Term (var "term") (Just (nothing :: TypedTerm (Maybe Type))) [
    _Term_record>>: "r" ~>
      Optionals.bind (Maps.lookup (Core.recordTypeName $ var "r") (var "schema"))
        ("sty" ~> Eithers.either (constant nothing)
          ("fields" ~> findFieldTypeIn @@ var "fname" @@ var "fields")
          (ExtractCore.recordType @@ (Core.recordTypeName $ var "r") @@ var "sty")),
    _Term_inject>>: "inj" ~>
      Optionals.bind (Maps.lookup (Core.injectionTypeName $ var "inj") (var "schema"))
        ("sty" ~> Eithers.either (constant nothing)
          ("fields" ~> findFieldTypeIn @@ var "fname" @@ var "fields")
          (ExtractCore.unionType @@ (Core.injectionTypeName $ var "inj") @@ var "sty"))]

findFieldTypeIn :: TypedTermDefinition (Name -> [FieldType] -> Maybe Type)
findFieldTypeIn = define "findFieldTypeIn" $
  doc "The type of the named field within a list of field types, if present" $
  "fname" ~> "fields" ~>
  Optionals.map (reify Core.fieldTypeType)
    (Lists.head (Lists.filter ("f" ~> Equality.equal (Core.fieldTypeName $ var "f") (var "fname")) (var "fields")))

-- Term side (graphToTermGraph). TermNode.type is synthesized by a single O(n) bottom-up, syntax-directed
-- fold over the ELABORATED (post-inference) term — no inference, no unification. Input is a precondition:
-- a missing annotation is reported with the path, never repaired by inference. typeOfTerm (hydra.checking)
-- is a TEST ORACLE only (agreement spot-check), never on this production path.

-- The typing of a node rides the same post-order fold as node construction: children are built (and
-- typed) first, then this node's type is synthesized syntax-directedly from the elaborated term's
-- annotations plus the children's synthesized types. No inference, no unification: the input is a
-- POST-INFERENCE graph (precondition). A missing annotation is a precondition violation reported with
-- the binding name / path, never a fallback to inference.

graphToTermGraph :: TypedTermDefinition (Graph -> Prelude.Either Error TermGraph)
graphToTermGraph = define "graphToTermGraph" $
  doc ("The term-graph view of a typed graph: one root node per binding. Sharing is explicit; children"
    <> " are inline (a tree); variable occurrences become symbolic references carrying their binding"
    <> " node's id. Input must be a POST-INFERENCE (elaborated) graph: binding schemes present, lambda"
    <> " domains present, typeLambda/typeApplication explicit. Node types are synthesized in one O(n)"
    <> " bidirectional pass; a missing annotation (or an untyped empty leaf) is a precondition failure.") $
  "graph" ~>
  "boundTerms" <~ Graph.graphBoundTerms (var "graph") $
  -- Schema: the constructor/nominal types available by name. graphBoundTypes gives type SCHEMES; the
  -- synthesis uses monomorphic types, so take each scheme's body (typeLambda/typeApplication in the
  -- elaborated term supply any needed instantiation).
  "schema" <~ (Maps.map ("ts" ~> Core.typeSchemeBody (var "ts")) (Graph.graphBoundTypes (var "graph")) :: TypedTerm (M.Map Name Type)) $
  Eithers.map
    ("roots" ~> Paths.termGraph (Maps.fromList (var "roots")))
    (Eithers.mapList
      ("nt" ~>
        "name" <~ Pairs.first (var "nt") $
        "term" <~ Pairs.second (var "nt") $
        -- Root expected type: the binding's declared type (scheme body under its typeLambdas).
        "expected" <~ (rootExpectedType @@ var "schema" @@ var "name") $
        Eithers.map
          ("node" ~> pair (var "name") (var "node"))
          (termGraphNode @@ var "graph" @@ var "schema" @@ var "name"
            @@ (Maps.empty :: TypedTerm (M.Map Name (TermNodeId, Type)))
            @@ (Sets.empty :: TypedTerm (S.Set Name))
            @@ var "expected"
            @@ (Paths.subtermPath $ list ([] :: [TypedTerm SubtermStep]))
            @@ var "term"))
      (Maps.toList (var "boundTerms" :: TypedTerm (M.Map Name Term))))

rootExpectedType :: TypedTermDefinition (M.Map Name Type -> Name -> Maybe Type)
rootExpectedType = define "rootExpectedType" $
  doc "The expected type for a root binding node: its declared type in the schema, if present" $
  "schema" ~> "name" ~> Maps.lookup (var "name" :: TypedTerm Name) (var "schema")

-- Type side (schemaToTypeGraph): needs no term typing, so it is complete and independent of the
-- open TermNode.type design question. One root node per named type; forall-bound variables tracked
-- in scope (like lambda-bound on the term side); nominal references to schema names; leaf types and
-- the two/three attributes as content.

schemaToTypeGraph :: TypedTermDefinition (M.Map Name Type -> Prelude.Either Error TypeGraph)
schemaToTypeGraph = define "schemaToTypeGraph" $
  doc ("The type-graph view of a schema: one root node per named type. A Type.variable referencing a"
    <> " schema-bound name is a nominal reference; a forall-bound variable in scope is a variable"
    <> " reference to its binding node; leaf types and non-type constituents are carried as links.") $
  "schema" ~>
  -- The set of schema-bound names, for nominal-vs-variable classification.
  "schemaNames" <~ (Maps.keys (var "schema" :: TypedTerm (M.Map Name Type))) $
  Eithers.map
    ("roots" ~> Paths.typeGraph (Maps.fromList (var "roots")))
    (Eithers.mapList
      ("nt" ~>
        "name" <~ Pairs.first (var "nt") $
        "typ" <~ Pairs.second (var "nt") $
        Eithers.map
          ("node" ~> pair (var "name") (var "node"))
          (typeGraphNode @@ (Sets.fromList (var "schemaNames"))
            @@ var "name" @@ (Maps.empty :: TypedTerm (M.Map Name TypeNodeId))
            @@ (Paths.subtypePath $ list ([] :: [TypedTerm SubtypeStep]))
            @@ var "typ"))
      (Maps.toList (var "schema" :: TypedTerm (M.Map Name Type))))

synthesizeType :: TypedTermDefinition (Graph -> M.Map Name Type -> M.Map Name Type -> Name -> Maybe Type -> SubtermPath -> Term -> Prelude.Either Error Type)
synthesizeType = define "synthesizeType" $
  doc ("Synthesize the type of an elaborated subterm, syntax-directed and without unification, threading"
    <> " `expected` top-down. Reads annotations (lambda domain, typeApplication type) and combines child"
    <> " types bottom-up; synthesis-incomplete leaves (empty list/set/map, optional none, either's absent"
    <> " branch) CONSUME `expected`. `expected = none` at such a leaf is a precondition failure with path.") $
  "graph" ~> "schema" ~> "env" ~> "root" ~> "expected" ~> "path" ~> "term" ~>
  "recurseInto" <~ ("mexp" ~> "child" ~> synthesizeType @@ var "graph" @@ var "schema" @@ var "env" @@ var "root" @@ var "mexp" @@ var "path" @@ var "child") $
  "fail" <~ ("msg" ~> (left (Error.errorOther $ Error.otherError $ Strings.concat2
    (Strings.concat2 (string "synthesizeType precondition: ") (var "msg"))
    (Strings.concat2 (string " at ") (Core.unName $ var "root")))
    :: TypedTerm (Prelude.Either Error Type))) $
  -- consumeExpected: for synthesis-incomplete leaves; expected=none is THE failure case.
  "consumeExpected" <~ ("what" ~> Optionals.match (var "expected")
    (var "fail" @@ Strings.concat2 (string "untyped ") (var "what"))
    ("t" ~> right (var "t"))) $
  -- expected-projection helpers: refine `expected` for a present element/body child, so a nested empty
  -- collection under a present parent still receives its element type.
  "elementOfExpected" <~ Optionals.bind (var "expected")
    ("t" ~> match _Type (var "t") (Just nothing) [
      _Type_list>>: "e" ~> just (var "e"),
      _Type_set>>: "e" ~> just (var "e"),
      _Type_optional>>: "e" ~> just (var "e")]) $
  "codomainOfExpected" <~ Optionals.bind (var "expected")
    ("t" ~> match _Type (var "t") (Just nothing) [
      _Type_function>>: "ft" ~> just (Core.functionTypeCodomain $ var "ft")]) $
  match _Term (var "term")
    (Just $ var "fail" @@ string "unsupported term constructor") [
    -- application: the function child's codomain (function position: expected unknown)
    _Term_application>>: "app" ~>
      Eithers.bind (var "recurseInto" @@ nothing @@ (Core.applicationFunction $ var "app"))
        ("ftype" ~> match _Type (var "ftype")
          (Just $ var "fail" @@ string "application of a non-function") [
          _Type_function>>: "ft" ~> right (Core.functionTypeCodomain $ var "ft")]),
    -- lambda: domain -> bodyType (domain from the annotation; required on elaborated input)
    _Term_lambda>>: "l" ~> Optionals.match (Core.lambdaDomain $ var "l")
      (var "fail" @@ string "lambda without a domain annotation")
      ("dom" ~>
        Eithers.map
          ("bt" ~> Core.typeFunction $ Core.functionType (var "dom") (var "bt"))
          (var "recurseInto" @@ var "codomainOfExpected" @@ (Core.lambdaBody $ var "l"))),
    -- typeLambda: forall v. bodyType
    _Term_typeLambda>>: "tl" ~>
      Eithers.map
        ("bt" ~> Core.typeForall $ Core.forallType (Core.typeLambdaParameter $ var "tl") (var "bt"))
        (var "recurseInto" @@ nothing @@ (Core.typeLambdaBody $ var "tl")),
    -- typeApplication: instantiate the body's forall with the explicit type argument (direct subst)
    _Term_typeApplication>>: "ta" ~>
      Eithers.bind (var "recurseInto" @@ nothing @@ (Core.typeApplicationTermBody $ var "ta"))
        ("bt" ~> match _Type (var "bt")
          (Just $ var "fail" @@ string "type application of a non-forall") [
          _Type_forall>>: "ft" ~> right (Substitution.substInType
            @@ (Substitution.singletonTypeSubst @@ (Core.forallTypeParameter $ var "ft") @@ (Core.typeApplicationTermType $ var "ta"))
            @@ (Core.forallTypeBody $ var "ft"))]),
    -- variable: from the term environment (lambda/let-bound); else a graph binding's declared type
    _Term_variable>>: "name" ~> Optionals.match (Maps.lookup (var "name" :: TypedTerm Name) (var "env"))
      (Optionals.match (Maps.lookup (var "name" :: TypedTerm Name) (var "schema"))
        (var "fail" @@ Strings.concat2 (string "unbound variable ") (Core.unName $ var "name"))
        ("t" ~> right (var "t")))
      ("t" ~> right (var "t")),
    -- unit
    _Term_unit>>: constant (right Core.typeUnit),
    -- literal: the literal's type
    _Term_literal>>: "lit" ~> right (Core.typeLiteral $ Reflect.literalType @@ var "lit"),
    -- optional: elementType from the present value; empty (none) consumes expected
    _Term_optional>>: "mt" ~> Optionals.match (var "mt")
      (var "consumeExpected" @@ string "optional none")
      ("t" ~> Eithers.map (reify Core.typeOptional) (var "recurseInto" @@ (var "elementOfExpected") @@ var "t")),
    -- list: elementType from the first element; empty consumes expected
    _Term_list>>: "els" ~> Optionals.match (Lists.head $ var "els")
      (var "consumeExpected" @@ string "empty list")
      ("t0" ~> Eithers.map (reify Core.typeList) (var "recurseInto" @@ (var "elementOfExpected") @@ var "t0")),
    -- set: elementType from the first element; empty consumes expected
    _Term_set>>: "s" ~> Optionals.match (Lists.head $ Sets.toList (var "s" :: TypedTerm (S.Set Term)))
      (var "consumeExpected" @@ string "empty set")
      ("t0" ~> Eithers.map (reify Core.typeSet) (var "recurseInto" @@ (var "elementOfExpected") @@ var "t0")),
    -- map: K/V from the first entry; empty consumes expected
    _Term_map>>: "m" ~> Optionals.match (Lists.head $ Maps.toList (var "m" :: TypedTerm (M.Map Term Term)))
      (var "consumeExpected" @@ string "empty map")
      ("kv" ~>
        Eithers.bind (var "recurseInto" @@ nothing @@ (Pairs.first $ var "kv"))
          ("kt" ~> Eithers.map
            ("vt" ~> Core.typeMap $ Core.mapType (var "kt") (var "vt"))
            (var "recurseInto" @@ nothing @@ (Pairs.second $ var "kv")))),
    -- pair: pair<firstType, secondType>
    _Term_pair>>: "p" ~>
      Eithers.bind (var "recurseInto" @@ nothing @@ (Pairs.first $ var "p"))
        ("ft" ~> Eithers.map
          ("st" ~> Core.typePair $ Core.pairType (var "ft") (var "st"))
          (var "recurseInto" @@ nothing @@ (Pairs.second $ var "p"))),
    -- either: one side present; the WHOLE value's type must come from expected (absent branch unknown)
    _Term_either>>: "e" ~> var "consumeExpected" @@ string "either value",
    -- record: the declared type of its named record type in the schema
    _Term_record>>: "r" ~> Optionals.match (Maps.lookup (Core.recordTypeName $ var "r") (var "schema"))
      (var "fail" @@ Strings.concat2 (string "record type not in schema: ") (Core.unName $ Core.recordTypeName $ var "r"))
      ("t" ~> right (var "t")),
    -- inject: the declared type of its named union type in the schema
    _Term_inject>>: "inj" ~> Optionals.match (Maps.lookup (Core.injectionTypeName $ var "inj") (var "schema"))
      (var "fail" @@ Strings.concat2 (string "union type not in schema: ") (Core.unName $ Core.injectionTypeName $ var "inj"))
      ("t" ~> right (var "t")),
    -- wrap: the declared type of its named wrapper type in the schema
    _Term_wrap>>: "w" ~> Optionals.match (Maps.lookup (Core.wrappedTermTypeName $ var "w") (var "schema"))
      (var "fail" @@ Strings.concat2 (string "wrapper type not in schema: ") (Core.unName $ Core.wrappedTermTypeName $ var "w"))
      ("t" ~> right (var "t")),
    -- annotated: the type of the underlying body
    _Term_annotated>>: "at" ~> var "recurseInto" @@ var "expected" @@ (Core.annotatedTermBody $ var "at"),
    -- let: the type of the body (expected flows through unchanged)
    _Term_let>>: "lt" ~> var "recurseInto" @@ var "expected" @@ (Core.letBody $ var "lt"),
    -- project: nominalType -> fieldType (typeArgs empty at the node → bare nominal domain)
    _Term_project>>: "p" ~>
      "tname" <~ Core.projectionTypeName (var "p") $
      "fname" <~ Core.projectionFieldName (var "p") $
      Optionals.match (Maps.lookup (var "tname" :: TypedTerm Name) (var "schema"))
        (var "fail" @@ Strings.concat2 (string "projection type not in schema: ") (Core.unName $ var "tname"))
        ("sty" ~> Eithers.bind (ExtractCore.recordType @@ var "tname" @@ var "sty")
          ("fields" ~> Optionals.match (findFieldTypeIn @@ var "fname" @@ var "fields")
            (var "fail" @@ Strings.concat2 (string "projected field not in record type: ") (Core.unName $ var "fname"))
            ("ftyp" ~> right (Core.typeFunction $ Core.functionType (Core.typeVariable $ var "tname") (var "ftyp"))))),
    -- unwrap: wrapperType -> wrappedBodyType
    _Term_unwrap>>: "tname" ~>
      Optionals.match (Maps.lookup (var "tname" :: TypedTerm Name) (var "schema"))
        (var "fail" @@ Strings.concat2 (string "unwrap type not in schema: ") (Core.unName $ var "tname"))
        ("sty" ~> Eithers.map
          ("inner" ~> Core.typeFunction $ Core.functionType (Core.typeVariable $ var "tname") (var "inner"))
          (ExtractCore.wrappedType @@ var "tname" @@ var "sty")),
    -- cases: unionType -> branchCodomain (the common result type of the handlers)
    _Term_cases>>: "cs" ~>
      "tname" <~ Core.caseStatementTypeName (var "cs") $
      -- Pick a representative handler: the default if present, else the first case; take its codomain.
      "rep" <~ Optionals.match (Core.caseStatementDefault $ var "cs")
        (Optionals.map (reify Core.caseAlternativeHandler) (Lists.head $ Core.caseStatementCases $ var "cs"))
        ("d" ~> just (var "d")) $
      Optionals.match (var "rep")
        (var "fail" @@ string "case statement with no branches")
        ("handler" ~> Eithers.bind (var "recurseInto" @@ nothing @@ var "handler")
          ("htype" ~> Eithers.map
            ("ft" ~> Core.typeFunction $ Core.functionType (Core.typeVariable $ var "tname") (Core.functionTypeCodomain $ var "ft"))
            (ExtractCore.functionType @@ var "htype")))]

termAttributesOf :: TypedTermDefinition (Term -> [TermAttributeLink])
termAttributesOf = define "termAttributesOf" $
  doc "The content attribute links contributed by a term constructor (a non-term constituent, with its value)" $
  "term" ~>
  match _Term (var "term")
    (Just $ list ([] :: [TypedTerm TermAttributeLink])) [
    _Term_cases>>: "cs" ~> list [Paths.termAttributeLinkCasesTypeName $ Core.caseStatementTypeName $ var "cs"],
    _Term_inject>>: "inj" ~> list [Paths.termAttributeLinkInjectTypeName $ Core.injectionTypeName $ var "inj"],
    _Term_lambda>>: "l" ~> Lists.concat2
      (list [Paths.termAttributeLinkLambdaParameter $ Core.lambdaParameter $ var "l"])
      (Optionals.match (Core.lambdaDomain $ var "l")
        (list ([] :: [TypedTerm TermAttributeLink]))
        ("d" ~> list [Paths.termAttributeLinkLambdaDomainGiven $ var "d"])),
    _Term_literal>>: "lit" ~> list [Paths.termAttributeLinkLiteral $ var "lit"],
    _Term_project>>: "p" ~> list [
      Paths.termAttributeLinkProjectTypeName $ Core.projectionTypeName $ var "p",
      Paths.termAttributeLinkProjectFieldName $ Core.projectionFieldName $ var "p"],
    _Term_record>>: "r" ~> list [Paths.termAttributeLinkRecordTypeName $ Core.recordTypeName $ var "r"],
    _Term_typeApplication>>: "ta" ~> list [Paths.termAttributeLinkTypeApplicationType $ Core.typeApplicationTermType $ var "ta"],
    _Term_typeLambda>>: "tl" ~> list [Paths.termAttributeLinkTypeLambdaParameter $ Core.typeLambdaParameter $ var "tl"],
    _Term_unwrap>>: "u" ~> list [Paths.termAttributeLinkUnwrapTypeName $ var "u"],
    _Term_wrap>>: "w" ~> list [Paths.termAttributeLinkWrapTypeName $ Core.wrappedTermTypeName $ var "w"]]

termGraphNode :: TypedTermDefinition (Graph -> M.Map Name Type -> Name -> M.Map Name (TermNodeId, Type) -> S.Set Name -> Maybe Type -> SubtermPath -> Term -> Prelude.Either Error TermNode)
termGraphNode = define "termGraphNode" $
  doc ("Build a term-graph node (inline) for the subterm at the given path within a root binding. `env`"
    <> " maps each in-scope local variable to its (binder node id, type); `lambdaScope` marks the"
    <> " lambda-bound subset. `expected` is the type flowing top-down, consumed by synthesis-incomplete"
    <> " leaves. A variable child is not a node: the parent emits a reference link for it.") $
  "graph" ~> "schema" ~> "root" ~> "env" ~> "lambdaScope" ~> "expected" ~> "path" ~> "term" ~>
  "steps" <~ unwrap _SubtermPath @@ var "path" $
  "thisId" <~ Paths.termNodeId (var "root") (var "path") $
  "envTypes" <~ (Maps.map ("p" ~> Pairs.second (var "p")) (var "env") :: TypedTerm (M.Map Name Type)) $
  -- childEnv / childLambdaScope: extend by this term's binders. lambda param -> (this node id, domain);
  -- each let binding -> (its bound-term node id = path++[letBinding name], scheme body).
  "childEnv" <~ (match _Term (var "term") (Just $ var "env") [
    _Term_lambda>>: "l" ~> Maps.insert
      (Core.lambdaParameter $ var "l")
      (pair (var "thisId") (Optionals.withDefault (Core.typeVariable $ Core.name $ string "?") (Core.lambdaDomain $ var "l")))
      (var "env"),
    _Term_let>>: "lt" ~> Lists.foldl
      ("acc" ~> "b" ~>
        "bname" <~ Core.bindingName (var "b") $
        "bid" <~ Paths.termNodeId (var "root") (Paths.subtermPath $ Lists.concat2 (var "steps") (list [Paths.subtermStepLetBinding $ var "bname"])) $
        "btype" <~ Optionals.withDefault (Core.typeVariable $ Core.name $ string "?") (Optionals.map (reify Core.typeSchemeBody) (Core.bindingTypeScheme $ var "b")) $
        Maps.insert (var "bname") (pair (var "bid") (var "btype")) (var "acc"))
      (var "env")
      (Core.letBindings $ var "lt")]) $
  "childLambdaScope" <~ (match _Term (var "term") (Just $ var "lambdaScope") [
    _Term_lambda>>: "l" ~> Sets.insert (Core.lambdaParameter $ var "l") (var "lambdaScope")]) $
  -- This node's type: synthesize bottom-up; empties/either-missing consume `expected`.
  Eithers.bind (synthesizeType @@ var "graph" @@ var "schema" @@ var "envTypes" @@ var "root" @@ var "expected" @@ var "path" @@ var "term")
    ("typ" ~>
      -- Attribute links contributed by this constructor.
      "attrLinks" <~ Lists.map (reify Paths.termLinkAttribute) (termAttributesOf @@ var "term") $
      -- One link per immediate subterm: a variable child becomes a reference link (occurrences are not
      -- nodes); any other child becomes an inline subterm link with a recursively-built child node.
      Eithers.bind (Eithers.mapList
        ("st" ~>
          "step" <~ Pairs.first (var "st") $
          "child" <~ Pairs.second (var "st") $
          "childPath" <~ Paths.subtermPath (Lists.concat2 (var "steps") (list [var "step"])) $
          "childExpected" <~ (expectedForStep @@ var "schema" @@ var "typ" @@ var "term" @@ var "step") $
          match _Term (var "child")
            (Just $ Eithers.map
              ("cnode" ~> Paths.termLinkSubterm $ Paths.subtermLink (var "step") (var "cnode"))
              (termGraphNode @@ var "graph" @@ var "schema" @@ var "root" @@ var "childEnv"
                @@ var "childLambdaScope" @@ var "childExpected" @@ var "childPath" @@ var "child")) [
            _Term_variable>>: "name" ~>
              Eithers.map
                ("ref" ~> Paths.termLinkReference $ Paths.termReferenceLink (var "step") (var "ref"))
                (termReferenceFor @@ var "graph" @@ var "childEnv" @@ var "childLambdaScope" @@ var "name")])
        (Rewriting.subtermsWithSteps @@ var "term"))
        ("childLinks" ~> right (Paths.termNode (var "term") (var "typ")
          (Lists.concat2 (var "attrLinks") (var "childLinks")))))

termReferenceFor :: TypedTermDefinition (Graph -> M.Map Name (TermNodeId, Type) -> S.Set Name -> Name -> Prelude.Either Error TermReference)
termReferenceFor = define "termReferenceFor" $
  doc ("Classify a variable occurrence, carrying its binder's node id: lambda-bound (node = the binding"
    <> " lambda), let-bound — locally or as a graph binding — (node = the bound term), or a primitive; a"
    <> " free variable is a precondition failure. `lambdaScope` marks which local names are lambda-bound.") $
  "graph" ~> "env" ~> "lambdaScope" ~> "name" ~>
  Optionals.match (Maps.lookup (var "name" :: TypedTerm Name) (var "env"))
    -- Not locally bound: a graph binding (let-like; node = that binding's root) or a primitive or free.
    (Logic.ifElse (Maps.member (var "name" :: TypedTerm Name) (Graph.graphBoundTerms $ var "graph"))
      (right $ Paths.termReferenceLet $ Paths.letVariableReference (var "name")
        (Paths.termNodeId (var "name") (Paths.subtermPath $ list ([] :: [TypedTerm SubtermStep])))
        (Optionals.withDefault (Core.typeVariable $ Core.name $ string "?")
          (Optionals.map (reify Core.typeSchemeBody) (Maps.lookup (var "name" :: TypedTerm Name) (Graph.graphBoundTypes $ var "graph")))))
      (Logic.ifElse (Maps.member (var "name" :: TypedTerm Name) (Graph.graphPrimitives $ var "graph"))
        (right $ Paths.termReferencePrimitive $ Paths.primitiveReference (var "name")
          (Core.typeVariable $ Core.name $ string "?"))
        (left (Error.errorOther $ Error.otherError $ Strings.concat2
          (string "free variable in term graph: ") (Core.unName $ var "name")))))
    -- Locally bound: the env tuple carries (binderNodeId, type); lambdaScope says lambda- vs let-bound.
    ("idType" ~>
      "binderId" <~ Pairs.first (var "idType") $
      "occType" <~ Pairs.second (var "idType") $
      Logic.ifElse (Sets.member (var "name" :: TypedTerm Name) (var "lambdaScope"))
        (right $ Paths.termReferenceLambda $ Paths.lambdaVariableReference (var "name") (var "binderId") (var "occType"))
        (right $ Paths.termReferenceLet $ Paths.letVariableReference (var "name") (var "binderId") (var "occType")))

typeAttributesOf :: TypedTermDefinition (Type -> [TypeAttributeLink])
typeAttributesOf = define "typeAttributesOf" $
  doc "The content attribute links contributed by a type constructor: annotation, forall parameter, or literal type" $
  "typ" ~>
  match _Type (var "typ")
    (Just $ list ([] :: [TypedTerm TypeAttributeLink])) [
    _Type_annotated>>: "at" ~> list [Paths.typeAttributeLinkAnnotatedAnnotation $ Core.annotatedTypeAnnotation $ var "at"],
    _Type_forall>>: "ft" ~> list [Paths.typeAttributeLinkForallParameter $ Core.forallTypeParameter $ var "ft"],
    _Type_literal>>: "lt" ~> list [Paths.typeAttributeLinkLiteral $ var "lt"]]

typeGraphNode :: TypedTermDefinition (S.Set Name -> Name -> M.Map Name TypeNodeId -> SubtypePath -> Type -> Prelude.Either Error TypeNode)
typeGraphNode = define "typeGraphNode" $
  doc "Build a type-graph node (inline) for the subtype at the given path within a named root type" $
  "schemaNames" ~> "root" ~> "forallScope" ~> "path" ~> "typ" ~>
  Eithers.map
    ("links" ~> Paths.typeNode (var "typ") (var "links"))
    (typeLinks @@ var "schemaNames" @@ var "root" @@ var "forallScope" @@ var "path" @@ var "typ")

typeLinks :: TypedTermDefinition (S.Set Name -> Name -> M.Map Name TypeNodeId -> SubtypePath -> Type -> Prelude.Either Error [TypeLink])
typeLinks = define "typeLinks" $
  doc ("The outgoing links of a type node: its content attributes, and one link per immediate subtype —"
    <> " a variable occurrence becomes a reference link on the parent (occurrences are not nodes), any"
    <> " other subtype an inline subtype link. `forallScope` maps each in-scope forall variable to its"
    <> " binding node id.") $
  "schemaNames" ~> "root" ~> "forallScope" ~> "path" ~> "typ" ~>
  "steps" <~ unwrap _SubtypePath @@ var "path" $
  "thisId" <~ Paths.typeNodeId (var "root") (var "path") $
  -- Content attributes contributed by this constructor (annotatedAnnotation / forallParameter / literal).
  "attrLinks" <~ Lists.map (reify Paths.typeLinkAttribute) (typeAttributesOf @@ var "typ") $
  -- The scope extended by descending: a forall parameter is bound at THIS node.
  "childScope" <~ (match _Type (var "typ") (Just $ var "forallScope") [
    _Type_forall>>: "ft" ~> Maps.insert (Core.forallTypeParameter $ var "ft") (var "thisId") (var "forallScope")]) $
  -- One link per immediate subtype: a variable child is a reference link; others are inline subtype links.
  Eithers.bind (Eithers.mapList
    ("st" ~>
      "step" <~ Pairs.first (var "st") $
      "child" <~ Pairs.second (var "st") $
      "childPath" <~ Paths.subtypePath (Lists.concat2 (var "steps") (list [var "step"])) $
      match _Type (var "child")
        (Just $ Eithers.map
          ("cnode" ~> Paths.typeLinkSubtype $ Paths.subtypeLink (var "step") (var "cnode"))
          (typeGraphNode @@ var "schemaNames" @@ var "root" @@ var "childScope" @@ var "childPath" @@ var "child")) [
        _Type_variable>>: "name" ~>
          Eithers.map
            ("ref" ~> Paths.typeLinkReference $ Paths.typeReferenceLink (var "step") (var "ref"))
            (typeReferenceFor @@ var "schemaNames" @@ var "childScope" @@ var "name")])
    (Rewriting.subtypesWithSteps @@ var "typ"))
    ("childLinks" ~> right (Lists.concat2 (var "attrLinks") (var "childLinks")))

typeReferenceFor :: TypedTermDefinition (S.Set Name -> M.Map Name TypeNodeId -> Name -> Prelude.Either Error TypeReference)
typeReferenceFor = define "typeReferenceFor" $
  doc ("Classify a type variable occurrence: a schema-bound name is a nominal reference; a forall-bound"
    <> " variable in scope is a variable reference carrying its binding forall's node id; anything else"
    <> " is a free-variable precondition failure.") $
  "schemaNames" ~> "forallScope" ~> "name" ~>
  Logic.ifElse (Sets.member (var "name" :: TypedTerm Name) (var "schemaNames"))
    (right (Paths.typeReferenceNominal $ Paths.nominalTypeReference (var "name")))
    (Optionals.match (Maps.lookup (var "name" :: TypedTerm Name) (var "forallScope"))
      (left (Error.errorOther $ Error.otherError $ Strings.concat2
        (string "free type variable in type graph: ") (Core.unName $ var "name")))
      ("binderId" ~> right (Paths.typeReferenceVariable $ Paths.typeVariableReference (var "name") (var "binderId"))))

-- ==================================================================================================
-- Term side (graphToTermGraph). TermNode.type is synthesized by a single O(n) bottom-up, syntax-directed
-- fold over the ELABORATED (post-inference) term — no inference, no unification. Input is a precondition:
-- a missing annotation is reported with the path, never repaired by inference. typeOfTerm (hydra.checking)
-- is a TEST ORACLE only (agreement spot-check), never on this production path.
-- ==================================================================================================

-- The typing of a node rides the same post-order fold as node construction: children are built (and
-- typed) first, then this node's type is synthesized syntax-directedly from the elaborated term's
-- annotations plus the children's synthesized types. No inference, no unification: the input is a
-- POST-INFERENCE graph (precondition). A missing annotation is a precondition violation reported with
-- the binding name / path, never a fallback to inference.
