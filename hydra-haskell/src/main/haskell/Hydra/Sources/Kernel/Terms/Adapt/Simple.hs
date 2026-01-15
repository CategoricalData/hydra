
module Hydra.Sources.Kernel.Terms.Adapt.Simple where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  adaptFloatType, adaptDataGraph, adaptGraphSchema, adaptIntegerType, adaptLiteral, adaptLiteralType,
  adaptLiteralTypesMap, adaptLiteralValue, adaptPrimitive, adaptTerm, adaptType, adaptTypeScheme,
  dataGraphToDefinitions, literalTypeSupported, schemaGraphToDefinitions, termAlternatives, typeAlternatives)
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

import qualified Hydra.Sources.Kernel.Terms.Hoisting    as Hoisting
import qualified Hydra.Sources.Kernel.Terms.Inference   as Inference
import qualified Hydra.Sources.Kernel.Terms.Literals    as Literals
import qualified Hydra.Sources.Kernel.Terms.Reduction   as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect     as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting   as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas     as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core   as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph  as ShowGraph


ns :: Namespace
ns = Namespace "hydra.adapt.simple"

module_ :: Module
module_ = Module ns elements
    [Hoisting.ns, Inference.ns, Literals.ns, Reduction.ns, Reflect.ns, Rewriting.ns, Schemas.ns,
      ShowCore.ns, ShowGraph.ns]
    kernelTypesNamespaces $
    Just "Simple, one-way adapters for types and terms"
  where
    elements = [
      toBinding adaptFloatType,
      toBinding adaptDataGraph,
      toBinding adaptGraphSchema,
      toBinding adaptIntegerType,
      toBinding adaptLiteral,
      toBinding adaptLiteralType,
      toBinding adaptLiteralTypesMap,
      toBinding adaptLiteralValue,
      toBinding adaptPrimitive,
      toBinding adaptTerm,
      toBinding adaptType,
      toBinding adaptTypeScheme,
      toBinding dataGraphToDefinitions,
      toBinding literalTypeSupported,
      toBinding schemaGraphToDefinitions,
      toBinding termAlternatives,
      toBinding typeAlternatives]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

adaptFloatType :: TBinding (LanguageConstraints -> FloatType -> Maybe FloatType)
adaptFloatType = define "adaptFloatType" $
  doc "Attempt to adapt a floating-point type using the given language constraints" $
  "constraints" ~> "ft" ~>
  "supported" <~ Sets.member (var "ft") (Coders.languageConstraintsFloatTypes $ var "constraints") $
  "alt" <~ (adaptFloatType @@ var "constraints") $
  "forUnsupported" <~ ("ft" ~> cases _FloatType (var "ft")
    Nothing [
--    _FloatType_bigfloat>>: constant nothing,
    _FloatType_bigfloat>>: constant $ var "alt" @@ Core.floatTypeFloat64, -- TODO: temporary; the only non-lossy alternative for bigfloat is string, but some migration is needed
    _FloatType_float32>>: constant $ var "alt" @@ Core.floatTypeFloat64,
    _FloatType_float64>>: constant $ var "alt" @@ Core.floatTypeBigfloat]) $
  Logic.ifElse (var "supported")
    (just $ var "ft")
    (var "forUnsupported" @@ var "ft")

adaptDataGraph :: TBinding (LanguageConstraints -> Bool -> Graph -> Flow s Graph)
adaptDataGraph = define "adaptDataGraph" $
  doc ("Adapt a graph and its schema to the given language constraints."
    <> " The doExpand flag controls eta expansion of partial applications."
    <> " Note: case statement hoisting is done separately, prior to inference.") $
  "constraints" ~> "doExpand" ~> "graph0" ~>
  "transform" <~ ("graph" ~> "gterm" ~>
    "tx" <<~ Schemas.graphToTypeContext @@ var "graph" $
    -- Order of operations:
    -- 1. Unshadow variables first (prevents capture issues in eta expansion)
    -- 2. Eta expand (needs type annotations; creates fully-applied functions)
    -- 3. Remove types (cleanup after eta expansion)
    -- 4. Lift lambdas above lets (structural cleanup)
    "gterm1" <~ Rewriting.unshadowVariables @@ var "gterm" $
    "gterm2" <<~ Logic.ifElse (var "doExpand")
      (Reduction.etaExpandTypedTerm @@ var "tx" @@ var "gterm1")
      (produce $ var "gterm1") $
    "gterm3" <~ Rewriting.removeTypesFromTerm @@ var "gterm2" $
    produce $ Rewriting.liftLambdaAboveLet @@ var "gterm3") $
  "litmap" <~ adaptLiteralTypesMap @@ var "constraints" $
  "els0" <~ Graph.graphElements (var "graph0") $
  "env0" <~ Graph.graphEnvironment (var "graph0") $
  "body0" <~ Graph.graphBody (var "graph0") $
  "prims0" <~ Graph.graphPrimitives (var "graph0") $
  "schema0" <~ Graph.graphSchema (var "graph0") $
  "schema1" <<~ optCases (var "schema0")
    (produce nothing)
--    ("sg" ~> Flows.fail $ "schema graph: " ++ (ShowGraph.graph @@ var "sg")) $
    ( "sg" ~>
      "tmap0" <<~ Schemas.graphAsTypes @@ var "sg" $
      "tmap1" <<~ adaptGraphSchema @@ var "constraints" @@ var "litmap" @@ var "tmap0" $
      "emap" <~ Schemas.typesToElements @@ var "tmap1" $
      produce $ just $ Graph.graphWithElements (var "sg") (var "emap")) $
  "gterm0" <~ Schemas.graphAsTerm @@ var "graph0" $
  "gterm1" <<~ Logic.ifElse (var "doExpand")
    (var "transform" @@ var "graph0" @@ var "gterm0")
    (produce $ var "gterm0") $
  "gterm2" <<~ adaptTerm @@ var "constraints" @@ var "litmap" @@ var "gterm1" $
  "els1Raw" <~ Schemas.termAsGraph @@ var "gterm2" $
  "prims1" <<~ Flows.mapElems (adaptPrimitive @@ var "constraints" @@ var "litmap") (var "prims0") $

  -- Collect existing constraints from original bindings (keyed by binding name)
  -- These need to be preserved through adaptation since termAsGraph doesn't preserve TypeScheme constraints
  "originalConstraints" <~ Maps.fromList (Maybes.cat $ Lists.map
    ("el" ~> Maybes.bind
      (Core.bindingType $ var "el")
      ("ts" ~> Maybes.map
        ("c" ~> pair (Core.bindingName $ var "el") (var "c"))
        (Core.typeSchemeConstraints $ var "ts")))
    (Maps.elems $ var "els0")) $

  -- Merge original constraints back into adapted bindings
  "mergeConstraints" <~ ("el" ~>
    "bname" <~ Core.bindingName (var "el") $
    "origConstraints" <~ Maps.lookup (var "bname") (var "originalConstraints") $
    Maybes.maybe
      (var "el")  -- No original constraints, keep binding as-is
      ("origC" ~> Maybes.maybe
        -- No inferred type scheme, create one with just the constraints
        (Core.binding
          (var "bname")
          (Core.bindingTerm $ var "el")
          (just $ Core.typeScheme (list ([] :: [TTerm Name])) (Core.typeVariable $ Core.name $ string "a") (just $ var "origC")))
        ("ts" ~>
          "inferredC" <~ Core.typeSchemeConstraints (var "ts") $
          "mergedC" <~ Maybes.maybe
            (just $ var "origC")  -- No inferred constraints, use original
            ("infC" ~> just $ Maps.union (var "origC") (var "infC"))  -- Merge both
            (var "inferredC") $
          Core.binding
            (var "bname")
            (Core.bindingTerm $ var "el")
            (just $ Core.typeScheme
              (Core.typeSchemeVariables $ var "ts")
              (Core.typeSchemeType $ var "ts")
              (var "mergedC")))
        (Core.bindingType $ var "el"))
      (var "origConstraints")) $

  "els1" <~ Maps.fromList (Lists.map
    ("el" ~> pair (Core.bindingName $ var "el") (var "mergeConstraints" @@ var "el"))
    (Maps.elems $ var "els1Raw")) $

  produce $ Graph.graph
    (var "els1")
    (var "env0")
    Maps.empty
    Core.termUnit
    (var "prims1")
    (var "schema1")

adaptGraphSchema :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType -> M.Map Name Type -> Flow s (M.Map Name Type))
adaptGraphSchema = define "adaptGraphSchema" $
  doc "Adapt a schema graph to the given language constraints" $
  "constraints" ~> "litmap" ~> "types0" ~>
  "mapPair" <~ ("pair" ~>
    "name" <~ Pairs.first (var "pair") $
    "typ" <~ Pairs.second (var "pair") $
    "typ1" <<~ adaptType @@ var "constraints" @@ var "litmap" @@ var "typ" $
    produce $ pair (var "name") (var "typ1")) $
  "pairs" <<~ Flows.mapList (var "mapPair") (Maps.toList $ var "types0") $
  produce $ Maps.fromList (var "pairs")

adaptIntegerType :: TBinding (LanguageConstraints -> IntegerType -> Maybe IntegerType)
adaptIntegerType = define "adaptIntegerType" $
  doc "Attempt to adapt an integer type using the given language constraints" $
  "constraints" ~> "it" ~>
  "supported" <~ Sets.member (var "it") (Coders.languageConstraintsIntegerTypes $ var "constraints") $
  "alt" <~ (adaptIntegerType @@ var "constraints") $
  "forUnsupported" <~ ("it" ~> cases _IntegerType (var "it")
    Nothing [
    _IntegerType_bigint>>: constant nothing,
    _IntegerType_int8>>: constant $ var "alt" @@ Core.integerTypeUint16,
    _IntegerType_int16>>: constant $ var "alt" @@ Core.integerTypeUint32,
    _IntegerType_int32>>: constant $ var "alt" @@ Core.integerTypeUint64,
    _IntegerType_int64>>: constant $ var "alt" @@ Core.integerTypeBigint,
    _IntegerType_uint8>>: constant $ var "alt" @@ Core.integerTypeInt16,
    _IntegerType_uint16>>: constant $ var "alt" @@ Core.integerTypeInt32,
    _IntegerType_uint32>>: constant $ var "alt" @@ Core.integerTypeInt64,
    _IntegerType_uint64>>: constant $ var "alt" @@ Core.integerTypeBigint]) $
  Logic.ifElse (var "supported")
    (just $ var "it")
    (var "forUnsupported" @@ var "it")

adaptLiteral :: TBinding (LiteralType -> Literal -> Literal)
adaptLiteral = define "adaptLiteral" $
  doc "Convert a literal to a different type" $
  "lt" ~> "l" ~>
  cases _Literal (var "l")
    Nothing [
    _Literal_binary>>: "b" ~> cases _LiteralType (var "lt")
      Nothing [
      _LiteralType_string>>: constant $ Core.literalString $ Literals.binaryToString $ var "b"],
    _Literal_boolean>>: "b" ~> cases _LiteralType (var "lt")
      Nothing [
      _LiteralType_integer>>: "it" ~> Core.literalInteger $
        Literals.bigintToIntegerValue @@ var "it" @@ Logic.ifElse (var "b") (bigint 1) (bigint 0)],
    _Literal_float>>: "f" ~> cases _LiteralType (var "lt")
      Nothing [
      _LiteralType_float>>: "ft" ~> Core.literalFloat $
        Literals.bigfloatToFloatValue @@ var "ft" @@ (Literals.floatValueToBigfloat @@ var "f")],
    _Literal_integer>>: "i" ~> cases _LiteralType (var "lt")
      Nothing [
      _LiteralType_integer>>: "it" ~> Core.literalInteger $
        Literals.bigintToIntegerValue @@ var "it" @@ (Literals.integerValueToBigint @@ var "i")]]

adaptLiteralType :: TBinding (LanguageConstraints -> LiteralType -> Maybe LiteralType)
adaptLiteralType = define "adaptLiteralType" $
  doc "Attempt to adapt a literal type using the given language constraints" $
  "constraints" ~> "lt" ~>
  "forUnsupported" <~ ("lt" ~> cases _LiteralType (var "lt")
    (Just nothing) [
    _LiteralType_binary>>: constant $ just Core.literalTypeString,
    _LiteralType_boolean>>: constant $ Maybes.map (unaryFunction Core.literalTypeInteger) $
      adaptIntegerType @@ var "constraints" @@ Core.integerTypeInt8,
    _LiteralType_float>>: "ft" ~> Maybes.map (unaryFunction Core.literalTypeFloat) $
      adaptFloatType @@ var "constraints" @@ var "ft",
    _LiteralType_integer>>: "it" ~> Maybes.map (unaryFunction Core.literalTypeInteger) $
      adaptIntegerType @@ var "constraints" @@ var "it"]) $
  Logic.ifElse (literalTypeSupported @@ var "constraints" @@ var "lt")
    nothing
    (var "forUnsupported" @@ var "lt")

adaptLiteralTypesMap :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType)
adaptLiteralTypesMap = define "adaptLiteralTypesMap" $
  doc "Derive a map of adapted literal types for the given language constraints" $
  "constraints" ~>
  "tryType" <~ ("lt" ~> optCases (adaptLiteralType @@ var "constraints" @@ var "lt")
    nothing
    ("lt2" ~> just $ pair (var "lt") (var "lt2"))) $
  Maps.fromList $ Maybes.cat $ Lists.map (var "tryType") (Reflect.literalTypes)

adaptLiteralValue :: TBinding (M.Map LiteralType LiteralType -> LiteralType -> Literal -> Literal)
adaptLiteralValue = define "adaptLiteralValue" $
  doc "Adapt a literal value using the given language constraints" $
  "litmap" ~> "lt" ~> "l" ~> optCases (Maps.lookup (var "lt") (var "litmap"))
    (Core.literalString $ ShowCore.literal @@ var "l")
    ("lt2" ~> adaptLiteral @@ var "lt2" @@ var "l")

adaptPrimitive :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType -> Primitive -> Flow s Primitive)
adaptPrimitive = define "adaptPrimitive" $
  doc "Adapt a primitive to the given language constraints, prior to inference" $
  "constraints" ~> "litmap" ~> "prim0" ~>
  "ts0" <~ Graph.primitiveType (var "prim0") $
  "ts1" <<~ adaptTypeScheme @@ var "constraints" @@ var "litmap" @@ var "ts0" $
  produce $ Graph.primitiveWithType (var "prim0") (var "ts1")

-- Note: this function could be made more efficient through precomputation of alternatives,
--       similar to what is done for literals.
adaptTerm :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType -> Term -> Flow Graph Term)
adaptTerm = define "adaptTerm" $
  doc "Adapt a term using the given language constraints" $
  "constraints" ~> "litmap" ~> "term0" ~>
  "rewrite" <~ ("recurse" ~> "term0" ~> lets [
    "forSupported">: ("term" ~> cases _Term (var "term")
      (Just $ produce $ just $ var "term") [
      _Term_literal>>: "l" ~>
        "lt" <~ Reflect.literalType @@ var "l" $
        produce $ just $ Logic.ifElse (literalTypeSupported @@ var "constraints" @@ var "lt")
          (var "term")
          (Core.termLiteral $ adaptLiteralValue @@ var "litmap" @@ var "lt" @@ var "l")]),
    "forUnsupported">: ("term" ~> lets [
      "forNonNull">: ("alts" ~>
        "mterm" <<~ var "tryTerm" @@ Lists.head (var "alts") $
        optCases (var "mterm")
          (var "tryAlts" @@ Lists.tail (var "alts"))
          ("t" ~> produce $ just $ var "t")),
      "tryAlts">: ("alts" ~> Logic.ifElse (Lists.null $ var "alts")
        (produce nothing)
        (var "forNonNull" @@ var "alts"))] $
      "alts" <<~ termAlternatives @@ var "term" $
      var "tryAlts" @@ var "alts"),
    "tryTerm">: ("term" ~>
      "supportedVariant" <~ Sets.member
        (Reflect.termVariant @@ var "term")
        (Coders.languageConstraintsTermVariants $ var "constraints") $
      Logic.ifElse (var "supportedVariant")
        (var "forSupported" @@ var "term")
        (var "forUnsupported" @@ var "term"))] $
    "term1" <<~ var "recurse" @@ var "term0" $
    "mterm" <<~ var "tryTerm" @@ var "term1" $
    optCases (var "mterm")
      (Flows.fail $ (string "no alternatives for term: ") ++ (ShowCore.term @@ var "term1"))
      ("term2" ~> produce $ var "term2")) $
  Rewriting.rewriteTermM @@ var "rewrite" @@ var "term0"

adaptType :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType -> Type -> Flow s Type)
adaptType = define "adaptType" $
  doc "Adapt a type using the given language constraints" $
  "constraints" ~> "litmap" ~> "type0" ~>
  lets [
  "forSupported">: ("typ" ~> cases _Type (var "typ")
    (Just $ just $ var "typ") [
    _Type_literal>>: "lt" ~> Logic.ifElse (literalTypeSupported @@ var "constraints" @@ var "lt")
      (just $ var "typ")
      (optCases (Maps.lookup (var "lt") (var "litmap"))
        (just $ Core.typeLiteral Core.literalTypeString)
        ("lt2" ~> just $ Core.typeLiteral $ var "lt2"))]),
  "forUnsupported">: ("typ" ~>
    "tryAlts" <~ ("alts" ~> Logic.ifElse (Lists.null $ var "alts")
      nothing
      (optCases (var "tryType" @@ Lists.head (var "alts"))
        (var "tryAlts" @@ Lists.tail (var "alts"))
        ("t" ~> just $ var "t"))) $
    "alts" <~ typeAlternatives @@ var "typ" $
    var "tryAlts" @@ var "alts"),
  "tryType">: ("typ" ~>
    "supportedVariant" <~ Sets.member
      (Reflect.typeVariant @@ var "typ")
      (Coders.languageConstraintsTypeVariants $ var "constraints") $
    Logic.ifElse (var "supportedVariant")
      (var "forSupported" @@ var "typ")
      (var "forUnsupported" @@ var "typ"))] $
  "rewrite" <~ ("recurse" ~> "typ" ~>
    "type1" <<~ var "recurse" @@ var "typ" $
    optCases (var "tryType" @@ var "type1")
      (Flows.fail $ (string "no alternatives for type: ") ++ (ShowCore.type_ @@ var "typ"))
      ("type2" ~> produce $ var "type2")) $
  Rewriting.rewriteTypeM @@ var "rewrite" @@ var "type0"

adaptTypeScheme :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType -> TypeScheme -> Flow s TypeScheme)
adaptTypeScheme = define "adaptTypeScheme" $
  doc "Adapt a type scheme to the given language constraints, prior to inference" $
  "constraints" ~> "litmap" ~> "ts0" ~>
  "vars0" <~ Core.typeSchemeVariables (var "ts0") $
  "t0" <~ Core.typeSchemeType (var "ts0") $
  "t1" <<~ adaptType @@ var "constraints" @@ var "litmap" @@ var "t0" $
  produce $ Core.typeScheme (var "vars0") (var "t1") (Core.typeSchemeConstraints (var "ts0"))

dataGraphToDefinitions :: TBinding (LanguageConstraints -> Bool -> Bool -> Bool -> Graph -> [[Name]] -> Flow s (Graph, [[TermDefinition]]))
dataGraphToDefinitions = define "dataGraphToDefinitions" $
  doc ("Given a data graph along with language constraints and a designated list of element names,"
    <> " adapt the graph to the language constraints, perform inference,"
    <> " then return a corresponding term definition for each element name."
    <> " The doExpand flag controls eta expansion."
    <> " The doHoistCaseStatements flag controls case statement hoisting (needed for Python)."
    <> " The doHoistPolymorphicLetBindings flag controls polymorphic let binding hoisting (needed for Java).") $
  "constraints" ~> "doExpand" ~> "doHoistCaseStatements" ~> "doHoistPolymorphicLetBindings" ~> "graph" ~> "nameLists" ~>

  -- Step 0: Unshadow variables BEFORE case statement hoisting
  -- This prevents capture issues where hoisted code references the wrong variable
  -- after code generators rename shadowed parameters
  "graphu0" <<~ Logic.ifElse (var "doHoistCaseStatements")
    ("gterm0" <~ Schemas.graphAsTerm @@ var "graph" $
     "gterm1" <~ Rewriting.unshadowVariables @@ var "gterm0" $
     "newElements" <~ Schemas.termAsGraph @@ var "gterm1" $
     produce $ Graph.graphWithElements (var "graph") (var "newElements"))
    (produce $ var "graph") $

  -- Step 1: Hoist case statements BEFORE inference (case hoisting doesn't need types)
  -- This ensures match expressions are applied to arguments before eta expansion
  "graphh1" <<~ Logic.ifElse (var "doHoistCaseStatements")
    (Hoisting.hoistCaseStatementsInGraph @@ var "graphu0")
    (produce $ var "graphu0") $

  -- Step 2: Unshadow variables AGAIN after case statement hoisting
  -- Hoisting creates new lambda wrappers for captured variables, which can reintroduce shadowing
  "graphu1" <<~ Logic.ifElse (var "doHoistCaseStatements")
    ("gterm2" <~ Schemas.graphAsTerm @@ var "graphh1" $
     "gterm3" <~ Rewriting.unshadowVariables @@ var "gterm2" $
     "newElements2" <~ Schemas.termAsGraph @@ var "gterm3" $
     produce $ Graph.graphWithElements (var "graphh1") (var "newElements2"))
    (produce $ var "graphh1") $

  -- Step 3: If eta expansion OR polymorphic let hoisting is needed, run inference first
  -- (eta expansion needs types; polymorphic let hoisting needs to know which bindings are polymorphic)
  "needFirstInference" <~ Logic.or (var "doExpand") (var "doHoistPolymorphicLetBindings") $
  "graphi1" <<~ Logic.ifElse (var "needFirstInference")
    (Inference.inferGraphTypes @@ var "graphu1")
    (produce $ var "graphu1") $

  -- Step 4: Hoist polymorphic let bindings AFTER first inference
  -- If doHoistPolymorphicLetBindings is True, polymorphic bindings are hoisted to top level (for Java)
  -- Non-polymorphic bindings are kept as inner let terms (local variables)
  -- Note: We wrap non-let terms in a dummy let, process them, then unwrap
  "graphh2" <<~ Logic.ifElse (var "doHoistPolymorphicLetBindings")
    (-- Process each binding's term through hoistPolymorphicLetBindings
     "processBinding" <~ ("binding" ~>
       "term" <~ Core.bindingTerm (var "binding") $
       -- Wrap term in a let if it's not already a let
       "wrappedLet" <~ cases _Term (var "term")
         -- If not a let, wrap it in a dummy let with no bindings
         (Just $ Core.let_ (list ([] :: [TTerm Binding])) (var "term")) [
         -- If already a let, use it directly
         _Term_let>>: "lt" ~> var "lt"] $
       -- Apply hoisting to only hoist polymorphic bindings
       "hoistedLet" <~ Hoisting.hoistPolymorphicLetBindings @@ var "wrappedLet" $
       -- Unwrap: if there are no bindings, just use the body; otherwise keep the let
       "resultTerm" <~ Logic.ifElse (Lists.null $ Core.letBindings $ var "hoistedLet")
         (Core.letBody $ var "hoistedLet")
         (Core.termLet $ var "hoistedLet") $
       Core.binding
         (Core.bindingName $ var "binding")
         (var "resultTerm")
         (Core.bindingType $ var "binding")) $
     "newBindings" <~ Lists.map (var "processBinding") (Maps.elems $ Graph.graphElements $ var "graphi1") $
     "newElements3" <~ Maps.fromList (Lists.map ("b" ~> pair (Core.bindingName $ var "b") (var "b")) (var "newBindings")) $
     produce $ Graph.graphWithElements (var "graphi1") (var "newElements3"))
    (produce $ var "graphi1") $

  -- Step 5: Unshadow variables after polymorphic let hoisting (if it was done)
  "graphu2" <<~ Logic.ifElse (var "doHoistPolymorphicLetBindings")
    ("gterm4" <~ Schemas.graphAsTerm @@ var "graphh2" $
     "gterm5" <~ Rewriting.unshadowVariables @@ var "gterm4" $
     "newElements4" <~ Schemas.termAsGraph @@ var "gterm5" $
     produce $ Graph.graphWithElements (var "graphh2") (var "newElements4"))
    (produce $ var "graphh2") $

  -- Step 6: Adapt the graph (includes eta expansion if enabled)
  "graph1" <<~ adaptDataGraph @@ var "constraints" @@ var "doExpand" @@ var "graphu2" $

--  Flows.fail ("adapted graph: " ++ (ShowGraph.graph @@ var "graph1"))

  -- Step 7: Perform final inference on the adapted graph
  "graph2" <<~ Inference.inferGraphTypes @@ var "graph1" $

  -- Construct term definitions
  "toDef" <~ ("el" ~>
    "ts" <~ Maybes.fromJust (Core.bindingType $ var "el") $
    Module.termDefinition
      (Core.bindingName $ var "el")
      (Core.bindingTerm $ var "el")
      (var "ts")) $

  produce $ pair
    (var "graph2")
    (Lists.map
      ("names" ~> Lists.map (var "toDef") $
        Lists.map ("n" ~> Maybes.fromJust $ Maps.lookup (var"n") (Graph.graphElements $ var "graph2")) (var "names"))
      (var "nameLists"))

literalTypeSupported :: TBinding (LanguageConstraints -> LiteralType -> Bool)
literalTypeSupported = define "literalTypeSupported" $
  doc "Check if a literal type is supported by the given language constraints" $
  "constraints" ~> "lt" ~>
  "forType" <~ ("lt" ~> cases _LiteralType (var "lt")
    (Just true) [
      _LiteralType_float>>: "ft" ~> Sets.member (var "ft") (Coders.languageConstraintsFloatTypes $ var "constraints"),
      _LiteralType_integer>>: "it" ~> Sets.member (var "it") (Coders.languageConstraintsIntegerTypes $ var "constraints")]) $
  Logic.ifElse
    (Sets.member
      (Reflect.literalTypeVariant @@ var "lt")
      (Coders.languageConstraintsLiteralVariants $ var "constraints"))
    (var "forType" @@ var "lt")
    false

schemaGraphToDefinitions :: TBinding (LanguageConstraints -> Graph -> [[Name]] -> Flow s (M.Map Name Type, [[TypeDefinition]]))
schemaGraphToDefinitions = define "schemaGraphToDefinitions" $
  doc ("Given a schema graph along with language constraints and a designated list of element names,"
    <> " adapt the graph to the language constraints,"
    <> " then return a corresponding type definition for each element name.") $
  "constraints" ~> "graph" ~> "nameLists" ~>
  "litmap" <~ adaptLiteralTypesMap @@ var "constraints" $
  "tmap0" <<~ Schemas.graphAsTypes @@ var "graph" $
  "tmap1" <<~ adaptGraphSchema @@ var "constraints" @@ var "litmap" @@ var "tmap0" $
  "toDef" <~ ("pair" ~> Module.typeDefinition (Pairs.first $ var "pair") (Pairs.second $ var "pair")) $
  produce $ pair
    (var "tmap1")
    (Lists.map
      ("names" ~> Lists.map (var "toDef") $
        Lists.map ("n" ~> pair (var "n") (Maybes.fromJust $ Maps.lookup (var "n") (var "tmap1"))) (var "names"))
      (var "nameLists"))
--  Flows.fail $ "schema graph for definitions: " ++ (ShowGraph.graph @@ var "graph")

termAlternatives :: TBinding (Term -> Flow Graph [Term])
termAlternatives = define "termAlternatives" $
  doc "Find a list of alternatives for a given term, if any" $
  "term" ~> cases _Term (var "term")
    (Just $ produce $ list ([] :: [TTerm Term])) [
    _Term_annotated>>: "at" ~>
      "term2" <~ Core.annotatedTermBody (var "at") $
      produce $ list [
        var "term2"], -- TODO: lossy
    _Term_maybe>>: "ot" ~> produce $ list [
      Core.termList $ optCases (var "ot")
        (list ([] :: [TTerm Term]))
        ("term2" ~> list [var "term2"])],
    -- Note: no type abstractions or type applications, as we are not expecting System F terms here
    _Term_union>>: "inj" ~>
      "tname" <~ Core.injectionTypeName (var "inj") $
      "field" <~ Core.injectionField (var "inj") $
      "fname" <~ Core.fieldName (var "field") $
      "fterm" <~ Core.fieldTerm (var "field") $
      "forFieldType" <~ ("ft" ~>
        "ftname" <~ Core.fieldTypeName (var "ft") $
        Core.field (var "fname") $ Core.termMaybe $ Logic.ifElse (Equality.equal (var "ftname") (var "fname"))
          (just $ var "fterm")
          (nothing)) $
      "rt" <<~ Schemas.requireUnionType @@ var "tname" $
      produce $ list [
        Core.termRecord $ Core.record (var "tname") (Lists.map (var "forFieldType") (Core.rowTypeFields $ var "rt"))],
    _Term_unit>>: constant $ produce $ list [
      Core.termLiteral $ Core.literalBoolean true],
    _Term_wrap>>: "wt" ~>
      "term2" <~ Core.wrappedTermBody (var "wt") $
      produce $ list [
         var "term2"]]

typeAlternatives :: TBinding (Type -> [Type])
typeAlternatives = define "typeAlternatives" $
  doc "Find a list of alternatives for a given type, if any" $
  "type" ~> cases _Type (var "type")
    (Just $ list ([] :: [TTerm Type])) [
    _Type_annotated>>: "at" ~>
      "type2" <~ Core.annotatedTypeBody (var "at") $
       list [var "type2"], -- TODO: lossy
    _Type_maybe>>: "ot" ~> list [
      Core.typeList $ var "ot"],
    _Type_union>>: "rt" ~>
      "tname" <~ Core.rowTypeTypeName (var "rt") $
      "fields" <~ Core.rowTypeFields (var "rt") $
      "toOptField" <~ ("f" ~> Core.fieldType (Core.fieldTypeName $ var "f") (MetaTypes.optional $ Core.fieldTypeType $ var "f")) $
      "optFields" <~ Lists.map (var "toOptField") (var "fields") $
      list [
        Core.typeRecord $ Core.rowType (var "tname") (var "optFields")],
    _Type_unit>>: constant $ list [
      Core.typeLiteral $ Core.literalTypeBoolean]]
