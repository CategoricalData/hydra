-- | Scala code generator in Hydra DSL.
-- This module provides DSL versions of Scala code generation functions.

module Hydra.Sources.Scala.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Variables      as Variables
import qualified Hydra.Sources.Kernel.Terms.Scoping        as Scoping
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Resolution    as Resolution
import qualified Hydra.Sources.Kernel.Terms.Analysis      as Analysis
import qualified Hydra.Sources.Kernel.Terms.Environment   as Environment
import qualified Hydra.Sources.Kernel.Terms.Predicates    as Predicates
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import           Prelude hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Coerce (coerce)

-- Additional imports
import qualified Hydra.Scala.Syntax as Scala
import qualified Hydra.Typing as HydraTyping
import qualified Hydra.Sources.Scala.Syntax as ScalaSyntax
import qualified Hydra.Sources.Scala.Language as ScalaLanguageSource
import qualified Hydra.Sources.Scala.Utils as ScalaUtilsSource
import qualified Hydra.Sources.Scala.Serde as ScalaSerdeSource


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_

-- | An empty list term, avoiding ambiguous type variable issues with 'emptyList'
emptyList :: TTerm [a]
emptyList = TTerm $ TermList []


ns :: Namespace
ns = Namespace "hydra.scala.coder"

module_ :: Module
module_ = Module ns definitions
    [ScalaUtilsSource.ns, ScalaSerdeSource.ns, Formatting.ns, Names.ns, Scoping.ns, Strip.ns, Variables.ns, Analysis.ns, Environment.ns, Predicates.ns, Resolution.ns, ShowCore.ns, Annotations.ns, Constants.ns,
      Inference.ns, Sorting.ns, Arity.ns, SerializationSource.ns, Reduction.ns]
    (ScalaSyntax.ns:moduleNamespace ScalaLanguageSource.module_:KernelTypes.kernelTypesNamespaces) $
    Just "Scala code generator: converts Hydra modules to Scala source code"
  where
    definitions = [
      toDefinition applyVar,
      toDefinition constructModule,
      toDefinition dropDomains,
      toDefinition encodeCase,
      toDefinition encodeComplexTermDef,
      toDefinition encodeFunction,
      toDefinition encodeLetBinding,
      toDefinition encodeLiteral,
      toDefinition encodeLocalDef,
      toDefinition encodeTerm,
      toDefinition encodeTermDefinition,
      toDefinition encodeType,
      toDefinition encodeTypeDefinition,
      toDefinition encodeTypedParam,
      toDefinition encodeUntypeApplicationTerm,
      toDefinition extractBody,
      toDefinition extractCodomain,
      toDefinition extractDomains,
      toDefinition extractLetBindings,
      toDefinition extractParams,
      toDefinition fieldToEnumCase,
      toDefinition fieldToParam,
      toDefinition findDomain,
      toDefinition findImports,
      toDefinition findSdom,
      toDefinition moduleToScala,
      toDefinition stripWrapEliminations,
      toDefinition toElImport,
      toDefinition toPrimImport,
      toDefinition typeParamToTypeVar]


applyVar :: TTermDefinition (Term -> Name -> Term)
applyVar = def "applyVar" $
  doc "Apply a variable to a term, performing substitution for lambdas" $
  lambda "fterm" $ lambda "avar" $ lets [
    "v">: Core.unName (var "avar")] $
    cases _Term (Strip.deannotateAndDetypeTerm @@ var "fterm") (Just $ Core.termApplication (record _Application [_Application_function>>: var "fterm", _Application_argument>>: Core.termVariable (var "avar")])) [
      _Term_lambda>>: ("lam" ~> lets [
        "lamParam">: project _Lambda _Lambda_parameter @@ var "lam",
        "lamBody">: project _Lambda _Lambda_body @@ var "lam"] $
        Logic.ifElse (Variables.isFreeVariableInTerm @@ var "lamParam" @@ var "lamBody")
          (var "lamBody")
          (Variables.substituteVariable @@ var "lamParam" @@ var "avar" @@ var "lamBody"))]

constructModule :: TTermDefinition (Context -> Graph -> Module -> [Definition] -> Either Error Scala.Pkg)
constructModule = def "constructModule" $
  doc "Construct a Scala package from a Hydra module and its definitions" $
  lambda "cx" $ lambda "g" $ lambda "mod" $ lambda "defs" $ lets [
    "partitioned">: Environment.partitionDefinitions @@ var "defs",
    "typeDefs">: Pairs.first (var "partitioned"),
    "termDefs">: Pairs.second (var "partitioned"),
    "nsName">: Packaging.unNamespace (Packaging.moduleNamespace (var "mod")),
    "pname">: toScalaName (var "nsName"),
    "pref">: inject _Data_Ref _Data_Ref_name (var "pname")] $
    Eithers.bind
      (Eithers.mapList ("td" ~> asTerm encodeTypeDefinition @@ var "cx" @@ var "g" @@ var "td") (var "typeDefs"))
      ("typeDeclStats" ~>
        Eithers.bind
          (Eithers.mapList ("td" ~> asTerm encodeTermDefinition @@ var "cx" @@ var "g" @@ var "td") (var "termDefs"))
          ("termDeclStats" ~>
            Eithers.bind
              (asTerm findImports @@ var "cx" @@ var "g" @@ var "mod")
              ("imports" ~>
                right (record _Pkg [
                  _Pkg_name>>: var "pname",
                  _Pkg_ref>>: var "pref",
                  _Pkg_stats>>: Lists.concat (list [var "imports", var "typeDeclStats", var "termDeclStats"])]))))
  where
    toScalaName n = record _Data_Name [
      _Data_Name_value>>: wrap _PredefString (Strings.intercalate (string ".") (Strings.splitOn (string ".") n))]

-- | Drop N domain types from a function type, returning the remaining type.
--   dropDomains 0 (A -> B -> C) = A -> B -> C
--   dropDomains 1 (A -> B -> C) = B -> C
--   dropDomains 2 (A -> B -> C) = C
dropDomains :: TTermDefinition (Int -> Type -> Type)
dropDomains = def "dropDomains" $
  doc "Drop N domain types from a function type, returning the remaining type" $
  lambda "n" $ lambda "t" $
    Logic.ifElse (Equality.lte (var "n") (int32 0))
      (var "t")
      (cases _Type (Strip.deannotateType @@ var "t")
        (Just $ var "t") [
        _Type_function>>: ("ft" ~> dropDomains @@ (Math.sub (var "n") (int32 1)) @@ (Core.functionTypeCodomain $ var "ft")),
        _Type_forall>>: ("fa" ~> dropDomains @@ var "n" @@ (Core.forallTypeBody $ var "fa"))])

encodeCase :: TTermDefinition (Context -> Graph -> M.Map Name Type -> Maybe Name -> Field -> Either Error Scala.Case)
encodeCase = def "encodeCase" $
  doc "Encode a case branch" $
  lambda "cx" $ lambda "g" $ lambda "ftypes" $ lambda "sn" $ lambda "f" $ lets [
    "fname">: project _Field _Field_name @@ var "f",
    "fterm">: project _Field _Field_term @@ var "f",
    -- Determine if the field has unit type: check ftypes if available, otherwise check if term is a lambda
    "isUnit">: Maybes.maybe
      -- If ftypes doesn't have this field, check the term structure
      (cases _Term (Strip.deannotateAndDetypeTerm @@ var "fterm") (Just false) [
        -- Check lambda: unit if domain=Unit, or if lambda body doesn't use the parameter
        _Term_lambda>>: ("lam" ~> lets [
          "lamParam">: Core.lambdaParameter $ var "lam",
          "lamBody">: Core.lambdaBody $ var "lam",
          "domIsUnit">: Maybes.maybe false ("dom" ~> Equality.equal (var "dom") (Core.typeUnit)) (Core.lambdaDomain $ var "lam"),
          -- isFreeVariableInTerm returns True when the variable is NOT present
          "bodyIgnoresParam">: Variables.isFreeVariableInTerm @@ var "lamParam" @@ var "lamBody"] $
          Logic.or (var "domIsUnit") (var "bodyIgnoresParam")),
        _Term_record>>: ("r" ~> Equality.equal (Lists.length (Core.recordFields (var "r"))) (int32 0)),
        _Term_unit>>: (constant true)])
      ("dom" ~> cases _Type (Strip.deannotateType @@ var "dom")
        (Just false)
        [_Type_unit>>: constant true,
         _Type_record>>: ("rt" ~> Equality.equal (Lists.length (var "rt")) (int32 0))])
      (Maps.lookup (var "fname") (var "ftypes")),
    -- Use type name + field name + lambda param name for unique variable names.
    -- The lambda param suffix prevents shadowing in nested matches on the same union type
    -- (e.g., outer "sf" -> "v_ParseResult_success_sf", inner "sa" -> "v_ParseResult_success_sa").
    "shortTypeName">: Lists.last (Strings.splitOn (string ".") (Maybes.maybe (string "x") ("n" ~> Core.unName (var "n")) (var "sn"))),
    -- Sanitize lambda param name for use as suffix: replace apostrophes with underscores
    "lamParamSuffix">: cases _Term (Strip.deannotateAndDetypeTerm @@ var "fterm") (Just $ string "") [
      _Term_lambda>>: ("lam" ~> lets [
        "rawName">: Core.unName (Core.lambdaParameter $ var "lam"),
        "safeName">: Strings.fromList (Lists.map ("c" ~> Logic.ifElse (Equality.equal (var "c") (int32 39)) (int32 95) (var "c")) (Strings.toList (var "rawName")))] $
        Strings.cat2 (string "_") (var "safeName"))],
    "v">: Core.name (Strings.cat (list [string "v_", var "shortTypeName", string "_", Core.unName (var "fname"), var "lamParamSuffix"])),
    -- Check if variant is truly parameterless (domain is Unit) vs parameterized but unused
    "domainIsUnit">: cases _Term (Strip.deannotateAndDetypeTerm @@ var "fterm") (Just true) [
      _Term_lambda>>: ("lam" ~>
        Maybes.maybe true ("dom" ~> Equality.equal (var "dom") (Core.typeUnit)) (Core.lambdaDomain $ var "lam"))],
    "patArgs">: Logic.ifElse (var "isUnit")
      (Logic.ifElse (var "domainIsUnit")
        (emptyList)
        (list [inject _Pat _Pat_wildcard unit]))
      (list [ScalaUtilsSource.svar @@ var "v"]),
    "pat">: inject _Pat _Pat_extract (record _Pat_Extract [
      _Pat_Extract_fun>>: ScalaUtilsSource.sname @@ (ScalaUtilsSource.qualifyUnionFieldName @@ string "MATCHED." @@ var "sn" @@ var "fname"),
      _Pat_Extract_args>>: var "patArgs"]),
    "applied">: asTerm applyVar @@ var "fterm" @@ var "v"] $
    Eithers.bind
      (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "applied")
      ("body" ~>
        right (record _Case [
          _Case_pat>>: var "pat",
          _Case_cond>>: nothing,
          _Case_body>>: var "body"]))

encodeComplexTermDef :: TTermDefinition (Context -> Graph -> String -> Term -> Type -> Either Error Scala.Stat)
encodeComplexTermDef = def "encodeComplexTermDef" $
  doc "Encode a complex term definition with proper parameter types from the type signature" $
  lambda "cx" $ lambda "g" $ lambda "lname" $ lambda "term" $ lambda "typ" $ lets [
    "doms">: extractDomains @@ var "typ",
    "paramNames">: extractParams @@ var "term",
    -- Only zip as many params as we have domain types (in case of mismatch)
    "paramCount">: Math.min (Lists.length (var "paramNames")) (Lists.length (var "doms")),
    -- Use effective codomain: only strip as many arrows as we have actual parameters
    "cod">: dropDomains @@ var "paramCount" @@ var "typ",
    "zippedParams">: Lists.zip (Lists.take (var "paramCount") (var "paramNames")) (Lists.take (var "paramCount") (var "doms")),
    "freeTypeVars">: Lists.filter
      ("v" ~> Logic.not (Lists.elem (int32 46) (Strings.toList (Core.unName (var "v")))))
      (Sets.toList (Variables.freeVariablesInType @@ var "typ")),
    "tparams">: Lists.map (lambda "tv" $ ScalaUtilsSource.stparam @@ var "tv") (var "freeTypeVars"),
    "letBindings">: extractLetBindings @@ var "term",
    -- Extend graph with the def's type variables so inner lets can see them
    "gWithTypeVars">: Graph.graph
      (Graph.graphBoundTerms $ var "g")
      (Graph.graphBoundTypes $ var "g")
      (Graph.graphClassConstraints $ var "g")
      (Graph.graphLambdaVariables $ var "g")
      (Graph.graphMetadata $ var "g")
      (Graph.graphPrimitives $ var "g")
      (Graph.graphSchemaTypes $ var "g")
      (Sets.union (Sets.fromList (var "freeTypeVars")) (Graph.graphTypeVariables $ var "g"))] $
    -- Create typed parameters by zipping param names with domain types
    Eithers.bind (Eithers.mapList (asTerm encodeTypedParam @@ var "cx" @@ var "gWithTypeVars") (var "zippedParams"))
      ("sparams" ~>
        -- Encode the body (with all lambdas/lets stripped)
        Eithers.bind (asTerm encodeTerm @@ var "cx" @@ var "gWithTypeVars" @@ (extractBody @@ var "term"))
          ("sbody" ~>
            -- Encode return type
            Eithers.bind (asTerm encodeType @@ var "cx" @@ var "g" @@ var "cod")
              ("scod" ~>
                -- Extend graph with let bindings for type lookups
                lets [
                "gForLets">: Logic.ifElse (Lists.null (var "letBindings"))
                  (var "gWithTypeVars")
                  (Scoping.extendGraphForLet @@ ("g" ~> "b" ~> Logic.ifElse (Predicates.isComplexBinding @@ var "g" @@ var "b") (just MetaTerms.true) nothing) @@ var "gWithTypeVars"
                    @@ Core.let_ (var "letBindings") (Core.termVariable (wrap _Name (string "dummy"))))] $
                -- Encode let bindings, passing current type params as outer scope
                Eithers.bind (Eithers.mapList (asTerm encodeLetBinding @@ var "cx" @@ var "gForLets" @@ (Sets.fromList (var "freeTypeVars"))) (var "letBindings"))
                  ("sbindings" ~> lets [
                    "defBody">: Logic.ifElse (Lists.null (var "sbindings"))
                      (var "sbody")
                      (inject _Data _Data_block (record _Data_Block [
                        _Data_Block_stats>>: Lists.concat2 (var "sbindings") (list [inject _Stat _Stat_term (var "sbody")])]))] $
                    right (inject _Stat _Stat_defn (inject _Defn _Defn_def (record _Defn_Def [
                      _Defn_Def_mods>>: emptyList,
                      _Defn_Def_name>>: record _Data_Name [_Data_Name_value>>: wrap _PredefString (var "lname")],
                      _Defn_Def_tparams>>: var "tparams",
                      _Defn_Def_paramss>>: Lists.map ("p" ~> list [var "p"]) (var "sparams"),
                      _Defn_Def_decltpe>>: just (var "scod"),
                      _Defn_Def_body>>: var "defBody"])))))))

encodeFunction :: TTermDefinition (Context -> Graph -> M.Map Name Term -> Term -> Maybe Term -> Either Error Scala.Data)
encodeFunction = def "encodeFunction" $
  doc "Encode a Hydra function-valued term (lambda, project, cases, or unwrap) as a Scala expression" $
  lambda "cx" $ lambda "g" $ lambda "meta" $ lambda "funTerm" $ lambda "arg" $
    (cases _Term (Strip.deannotateAndDetypeTerm @@ var "funTerm") (Just $ left (Error.errorOther $ Error.otherError (string "unsupported function"))) [
      _Term_lambda>>: ("lam" ~> lets [
        "param">: project _Lambda _Lambda_parameter @@ var "lam",
        "v">: ScalaUtilsSource.scalaEscapeName @@ (Core.unName (var "param")),
        "body">: project _Lambda _Lambda_body @@ var "lam",
        "rawMdom">: Core.lambdaDomain $ var "lam",
        -- Discard lambda domain if it contains unresolved type variables.
        -- These come from: (1) forall-residual single-char vars like 'a', 'b', 'x',
        -- or (2) inference-generated fresh variables like 't0', 't1'.
        -- Check: any free unqualified type variable NOT in graphTypeVariables is suspect.
        "mdom">: Maybes.bind (var "rawMdom")
          ("dom" ~> lets [
            "freeVars">: Variables.freeVariablesInType @@ var "dom",
            -- Filter to unqualified vars (no dots in name)
            "unqualifiedFreeVars">: Sets.fromList (Lists.filter
              ("n" ~> Logic.not (Lists.elem (int32 46) (Strings.toList (Core.unName (var "n")))))
              (Sets.toList (var "freeVars"))),
            -- Check if any unqualified free vars are not in the graph's type variables
            "unresolvedVars">: Sets.difference (var "unqualifiedFreeVars") (Graph.graphTypeVariables $ var "g")] $
            Logic.ifElse (Sets.null (var "unresolvedVars"))
              (just (var "dom"))
              nothing)] $
        -- Encode lambda body and wrap in lambda with typed parameter
        Eithers.bind
          (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "body")
          ("sbody" ~>
            Eithers.bind
              (Maybes.maybe
                (asTerm findSdom @@ var "cx" @@ var "g" @@ var "meta")
                ("dom" ~> Eithers.bind (asTerm encodeType @@ var "cx" @@ var "g" @@ var "dom")
                  ("sdom" ~> right (just (var "sdom"))))
                (var "mdom"))
              ("sdom" ~>
                right (ScalaUtilsSource.slambda @@ var "v" @@ var "sbody" @@ var "sdom")))),
      _Term_unwrap>>: ("name" ~>
        -- Wrap elimination is identity in Scala (newtypes are erased)
        Maybes.maybe
          (Eithers.bind (asTerm findSdom @@ var "cx" @@ var "g" @@ var "meta")
            ("sdom" ~> right (ScalaUtilsSource.slambda @@ string "x" @@ (ScalaUtilsSource.sname @@ string "x") @@ var "sdom")))
          ("a" ~> asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "a")
          (var "arg")),
      _Term_project>>: ("proj" ~> lets [
        "fname">: ScalaUtilsSource.scalaEscapeName @@ (Core.unName (project _Projection _Projection_field @@ var "proj")),
        "typeName">: project _Projection _Projection_typeName @@ var "proj",
        "pv">: string "x"] $
        Maybes.maybe
          -- Unapplied projection: generate lambda x => x.fieldName
          -- Try findSdom first (full type with type params), fall back to Projection.typeName
          (Eithers.bind
            (Eithers.either_
              -- findSdom failed: fall back to Projection.typeName
              (constant $ Eithers.bind
                (asTerm encodeType @@ var "cx" @@ var "g" @@ (Core.typeVariable (var "typeName")))
                ("st" ~> right (just (var "st"))))
              -- findSdom succeeded: check if result is Nothing, fall back to typeName
              ("msdom" ~> Maybes.maybe
                -- findSdom returned Nothing: fall back to Projection.typeName
                (Eithers.bind
                  (asTerm encodeType @@ var "cx" @@ var "g" @@ (Core.typeVariable (var "typeName")))
                  ("st" ~> right (just (var "st"))))
                -- findSdom returned Just: use it
                ("sdom" ~> right (just (var "sdom")))
                (var "msdom"))
              (asTerm findSdom @@ var "cx" @@ var "g" @@ var "meta"))
            ("msdom" ~>
              right (ScalaUtilsSource.slambda @@ var "pv" @@
                (inject _Data _Data_ref (inject _Data_Ref _Data_Ref_select (
                  record _Data_Select [
                    _Data_Select_qual>>: ScalaUtilsSource.sname @@ var "pv",
                    _Data_Select_name>>: record _Data_Name [
                      _Data_Name_value>>: wrap _PredefString (var "fname")]])))
                @@ var "msdom")))

          -- Applied projection: encode in the application handler (shouldn't reach here)
          ("a" ~>
            Eithers.bind
              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "a")
              ("sa" ~>
                right (inject _Data _Data_ref (inject _Data_Ref _Data_Ref_select (
                  record _Data_Select [
                    _Data_Select_qual>>: var "sa",
                    _Data_Select_name>>: record _Data_Name [
                      _Data_Name_value>>: wrap _PredefString (var "fname")]])))))
          (var "arg")),
      _Term_cases>>: ("cs" ~> lets [
        "v">: string "v",
        "tname">: project _CaseStatement _CaseStatement_typeName @@ var "cs",
        "dom">: Core.typeVariable (var "tname"),
        "sn">: ScalaUtilsSource.nameOfType @@ var "g" @@ var "dom",
        "cases">: project _CaseStatement _CaseStatement_cases @@ var "cs",
        "dflt">: project _CaseStatement _CaseStatement_default @@ var "cs",
        -- Try to get field types from the graph; fall back to empty map if unavailable
        "ftypes">: Eithers.either_
          (constant Maps.empty)
          identity
          (Resolution.fieldTypes @@ var "cx" @@ var "g" @@ var "dom")] $
            Eithers.bind
              (Eithers.mapList ("f" ~> asTerm encodeCase @@ var "cx" @@ var "g" @@ var "ftypes" @@ var "sn" @@ var "f") (var "cases"))
              ("fieldCases" ~>
                -- Add default case if present
                Eithers.bind
                  (Maybes.maybe
                    (right (var "fieldCases"))
                    ("dfltTerm" ~>
                      Eithers.bind
                        (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "dfltTerm")
                        ("sdflt" ~>
                          right (Lists.concat2 (var "fieldCases") (list [
                            record _Case [
                              _Case_pat>>: inject _Pat _Pat_wildcard unit,
                              _Case_cond>>: nothing,
                              _Case_body>>: var "sdflt"]]))))
                    (var "dflt"))
                  ("scases" ~>
                Maybes.maybe
                  (Eithers.bind
                    (asTerm findSdom @@ var "cx" @@ var "g" @@ var "meta")
                    ("sdom" ~>
                      right (ScalaUtilsSource.slambda @@ var "v" @@ (inject _Data _Data_match (record _Data_Match [
                        _Data_Match_expr>>: ScalaUtilsSource.sname @@ var "v",
                        _Data_Match_cases>>: var "scases"])) @@ var "sdom")))
                  ("a" ~>
                    Eithers.bind
                      (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "a")
                      ("sa" ~>
                        right (inject _Data _Data_match (record _Data_Match [
                          _Data_Match_expr>>: var "sa",
                          _Data_Match_cases>>: var "scases"]))))
                  (var "arg"))))])

-- | Helper to construct a lazy val statement (for top-level definitions to avoid forward reference errors)
mkLazyVal :: TTerm String -> TTerm (Maybe Scala.Type) -> TTerm Scala.Data -> TTerm Scala.Stat
mkLazyVal vname mdecltpe rhs =
  inject _Stat _Stat_defn (inject _Defn _Defn_val (record _Defn_Val [
    _Defn_Val_mods>>: list [inject Scala._Mod Scala._Mod_lazy unit],
    _Defn_Val_pats>>: list [inject _Pat _Pat_var (record _Pat_Var [
      _Pat_Var_name>>: record _Data_Name [_Data_Name_value>>: wrap _PredefString vname]])],
    _Defn_Val_decltpe>>: mdecltpe,
    _Defn_Val_rhs>>: rhs]))

encodeLetBinding :: TTermDefinition (Context -> Graph -> S.Set Name -> Binding -> Either Error Scala.Stat)
encodeLetBinding = def "encodeLetBinding" $
  doc "Encode a let binding as a val or def declaration. outerTypeVars are type params from the enclosing scope." $
  lambda "cx" $ lambda "g" $ lambda "outerTypeVars" $ lambda "b" $ lets [
    "bname">: ScalaUtilsSource.scalaEscapeName @@ (Core.unName (Core.bindingName $ var "b")),
    "bterm">: Core.bindingTerm $ var "b",
    -- Effective type scheme: binding's own type, or fall back to graph's bound types
    "mts">: optCases (Core.bindingType $ var "b")
      (Maps.lookup (Core.bindingName $ var "b") (Graph.graphBoundTypes $ var "g"))
      ("ts" ~> just (var "ts")),
    -- Check if the binding has a function type
    "isFn">: Maybes.maybe false
      ("ts" ~> cases _Type (Strip.deannotateType @@ (Core.typeSchemeType $ var "ts"))
        (Just false)
        [_Type_function>>: constant true,
         _Type_forall>>: ("fa" ~> cases _Type (Strip.deannotateType @@ Core.forallTypeBody (var "fa"))
           (Just false) [_Type_function>>: constant true])])
      (var "mts")] $
    -- Route to encodeLocalDef when we have a type scheme AND either it's a function OR it has local type vars.
    -- This mirrors the Java coder which always uses typed declarations and hoists polymorphic bindings to methods.
    Maybes.maybe
      -- No type scheme at all: simple val
      (Eithers.bind (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "bterm")
        ("srhs" ~> right (mkLazyVal (var "bname") nothing (var "srhs"))))
      ("ts" ~> lets [
        "newVars">: Lists.filter ("v" ~> Logic.not (Sets.member (var "v") (var "outerTypeVars"))) (Core.typeSchemeVariables $ var "ts"),
        -- Use def when: function type, or has locally-quantified type vars (like Java's static <T0> method)
        "useDef">: Logic.or (var "isFn") (Logic.not (Lists.null (var "newVars")))] $
        Logic.ifElse (var "useDef")
          -- Generate local def with type params and/or typed params
          (asTerm encodeLocalDef @@ var "cx" @@ var "g" @@ var "outerTypeVars" @@ var "bname" @@ var "bterm" @@ (Core.typeSchemeType $ var "ts"))
          -- Monomorphic non-function: val with type annotation
          (Eithers.bind (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "bterm")
            ("srhs" ~> Eithers.bind (asTerm encodeType @@ var "cx" @@ var "g" @@ (Core.typeSchemeType $ var "ts"))
              ("styp" ~> right (mkLazyVal (var "bname") (just (var "styp")) (var "srhs"))))))
      (var "mts")

encodeLiteral :: TTermDefinition (Context -> Graph -> Literal -> Either Error Scala.Lit)
encodeLiteral = def "encodeLiteral" $
  doc "Encode a literal value as a Scala literal" $
  lambda "cx" $ lambda "g" $ lambda "av" $
    (cases _Literal (var "av") (Just $ left (Error.errorOther $ Error.otherError (string "unexpected literal"))) [
      _Literal_binary>>: ("b" ~> right (inject _Lit (Name "bytes") (Literals.binaryToBytes (var "b")))),
      _Literal_boolean>>: ("b" ~> right (inject _Lit _Lit_boolean (var "b"))),
      _Literal_decimal>>: ("d" ~> right (inject _Lit _Lit_string (Literals.showDecimal (var "d")))),
      _Literal_float>>: ("fv" ~> cases _FloatValue (var "fv") (Just $ left (Error.errorOther $ Error.otherError (string "unexpected float value"))) [
        _FloatValue_bigfloat>>: ("bf" ~> right (inject _Lit _Lit_double (Literals.bigfloatToFloat64 (var "bf")))),
        _FloatValue_float32>>: ("f" ~> right (inject _Lit _Lit_float (var "f"))),
        _FloatValue_float64>>: ("f" ~> right (inject _Lit _Lit_double (var "f")))]),
      _Literal_integer>>: ("iv" ~> cases _IntegerValue (var "iv") (Just $ left (Error.errorOther $ Error.otherError (string "unexpected integer value"))) [
        _IntegerValue_bigint>>: ("i" ~> right (inject _Lit _Lit_long (Literals.bigintToInt64 (var "i")))),
        _IntegerValue_int8>>: ("i" ~> right (inject _Lit _Lit_byte (var "i"))),
        _IntegerValue_int16>>: ("i" ~> right (inject _Lit _Lit_short (var "i"))),
        _IntegerValue_int32>>: ("i" ~> right (inject _Lit _Lit_int (var "i"))),
        _IntegerValue_int64>>: ("i" ~> right (inject _Lit _Lit_long (var "i"))),
        _IntegerValue_uint8>>: ("i" ~> right (inject _Lit _Lit_byte (Literals.bigintToInt8 (Literals.uint8ToBigint (var "i"))))),
        _IntegerValue_uint16>>: ("i" ~> right (inject _Lit _Lit_int (Literals.bigintToInt32 (Literals.uint16ToBigint (var "i"))))),
        _IntegerValue_uint32>>: ("i" ~> right (inject _Lit _Lit_long (Literals.bigintToInt64 (Literals.uint32ToBigint (var "i"))))),
        _IntegerValue_uint64>>: ("i" ~> right (inject _Lit _Lit_long (Literals.bigintToInt64 (Literals.uint64ToBigint (var "i")))))]),
      _Literal_string>>: ("s" ~> right (inject _Lit _Lit_string (var "s")))])

encodeLocalDef :: TTermDefinition (Context -> Graph -> S.Set Name -> String -> Term -> Type -> Either Error Scala.Stat)
encodeLocalDef = def "encodeLocalDef" $
  doc "Encode a local def. outerTypeVars are type params already in scope (don't redeclare them)." $
  lambda "cx" $ lambda "g" $ lambda "outerTypeVars" $ lambda "lname" $ lambda "term" $ lambda "typ" $ lets [
    -- Only declare type params that are NOT in the outer scope
    "freeTypeVars">: Lists.filter
      ("v" ~> Logic.and
        (Logic.not (Lists.elem (int32 46) (Strings.toList (Core.unName (var "v")))))
        (Logic.not (Sets.member (var "v") (var "outerTypeVars"))))
      (Sets.toList (Variables.freeVariablesInType @@ var "typ")),
    "doms">: extractDomains @@ var "typ",
    "paramNames">: extractParams @@ var "term",
    "paramCount">: Math.min (Lists.length (var "paramNames")) (Lists.length (var "doms")),
    -- Use effective codomain: only strip as many arrows as we have actual parameters
    "cod">: dropDomains @@ var "paramCount" @@ var "typ",
    "zippedParams">: Lists.zip (Lists.take (var "paramCount") (var "paramNames")) (Lists.take (var "paramCount") (var "doms")),
    "letBindings">: extractLetBindings @@ var "term",
    "tparams">: Lists.map (lambda "tv" $ ScalaUtilsSource.stparam @@ var "tv") (var "freeTypeVars"),
    -- All type vars in scope: outer + this def's own
    "allTypeVars">: Sets.union (var "outerTypeVars") (Sets.fromList (var "freeTypeVars")),
    -- Extend graph with accumulated type variables
    "gWithTypeVars">: Graph.graph
      (Graph.graphBoundTerms $ var "g")
      (Graph.graphBoundTypes $ var "g")
      (Graph.graphClassConstraints $ var "g")
      (Graph.graphLambdaVariables $ var "g")
      (Graph.graphMetadata $ var "g")
      (Graph.graphPrimitives $ var "g")
      (Graph.graphSchemaTypes $ var "g")
      (Sets.union (var "allTypeVars") (Graph.graphTypeVariables $ var "g"))] $
    Eithers.bind (Eithers.mapList (asTerm encodeTypedParam @@ var "cx" @@ var "gWithTypeVars") (var "zippedParams"))
      ("sparams" ~>
        Eithers.bind (asTerm encodeTerm @@ var "cx" @@ var "gWithTypeVars" @@ (extractBody @@ var "term"))
          ("sbody" ~>
            Eithers.bind (asTerm encodeType @@ var "cx" @@ var "gWithTypeVars" @@ var "cod")
              ("scod" ~>
                -- Extend graph with let bindings for type lookups
                lets [
                "gForLets">: Logic.ifElse (Lists.null (var "letBindings"))
                  (var "gWithTypeVars")
                  (Scoping.extendGraphForLet @@ ("g" ~> "b" ~> Logic.ifElse (Predicates.isComplexBinding @@ var "g" @@ var "b") (just MetaTerms.true) nothing) @@ var "gWithTypeVars"
                    @@ Core.let_ (var "letBindings") (Core.termVariable (wrap _Name (string "dummy"))))] $
                Eithers.bind (Eithers.mapList (asTerm encodeLetBinding @@ var "cx" @@ var "gForLets" @@ var "allTypeVars") (var "letBindings"))
                  ("sbindings" ~> lets [
                    "defBody">: Logic.ifElse (Lists.null (var "sbindings"))
                      (var "sbody")
                      (inject _Data _Data_block (record _Data_Block [
                        _Data_Block_stats>>: Lists.concat2 (var "sbindings") (list [inject _Stat _Stat_term (var "sbody")])]))] $
                    right (inject _Stat _Stat_defn (inject _Defn _Defn_def (record _Defn_Def [
                      _Defn_Def_mods>>: emptyList,
                      _Defn_Def_name>>: record _Data_Name [_Data_Name_value>>: wrap _PredefString (var "lname")],
                      _Defn_Def_tparams>>: var "tparams",
                      _Defn_Def_paramss>>: Lists.map ("p" ~> list [var "p"]) (var "sparams"),
                      _Defn_Def_decltpe>>: just (var "scod"),
                      _Defn_Def_body>>: var "defBody"])))))))

encodeTerm :: TTermDefinition (Context -> Graph -> Term -> Either Error Scala.Data)
encodeTerm = def "encodeTerm" $
  doc "Encode a Hydra term as a Scala expression" $
  lambda "cx" $ lambda "g" $ lambda "term0" $ lets [
    "term">: stripWrapEliminations @@ var "term0"] $
    (cases _Term (Strip.deannotateTerm @@ var "term") (Just $ left (Error.errorOther $ Error.otherError (string "unexpected term"))) [
      _Term_typeApplication>>: ("ta" ~> lets [
        -- Collect all nested type applications into a list of type args
        "collectTypeArgs">: ("t" ~> "acc" ~>
          cases _Term (Strip.deannotateTerm @@ var "t")
            (Just $ pair (var "acc") (var "t"))
            [_Term_typeApplication>>: ("ta2" ~>
              var "collectTypeArgs"
                @@ (Core.typeApplicationTermBody $ var "ta2")
                @@ (Lists.cons (Core.typeApplicationTermType $ var "ta2") (var "acc")))]),
        "collected">: var "collectTypeArgs"
          @@ (Core.typeApplicationTermBody $ var "ta")
          @@ (list [Core.typeApplicationTermType $ var "ta"]),
        "typeArgs">: Pairs.first (var "collected"),
        "innerTerm">: Pairs.second (var "collected"),
        -- Collect type lambda parameters from the inner term to build a substitution map
        "collectTypeLambdas">: ("t" ~> "acc" ~>
          cases _Term (Strip.deannotateTerm @@ var "t")
            (Just $ pair (var "acc") (var "t"))
            [_Term_typeLambda>>: ("tl" ~>
              var "collectTypeLambdas"
                @@ (Core.typeLambdaBody $ var "tl")
                @@ (Lists.cons (Core.typeLambdaParameter $ var "tl") (var "acc")))]),
        "tlCollected">: var "collectTypeLambdas" @@ var "innerTerm" @@ (list ([] :: [TTerm Name])),
        "typeParams">: Pairs.first (var "tlCollected"),
        "bodyAfterTypeLambdas">: Pairs.second (var "tlCollected"),
        "substitutedBody">: var "bodyAfterTypeLambdas"] $
        -- Check if the inner term is a function (primitive or variable) — if so, add type args
        cases _Term (Strip.deannotateTerm @@ var "substitutedBody")
          -- Not a function: encode the substituted body
          (Just $ asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "substitutedBody")
          [
          -- For eliminations (record/union projections/unwrap): encode the substituted body
          _Term_project>>: (constant $ asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "substitutedBody"),
          _Term_cases>>: (constant $ asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "substitutedBody"),
          _Term_unwrap>>: (constant $ asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "substitutedBody"),
          -- TermVariable referencing a primitive: same treatment as Function_primitive
          _Term_variable>>: ("pname" ~>
            Maybes.cases (Maps.lookup (var "pname") (Graph.graphPrimitives (var "g")))
              -- Not a primitive: encode the substituted body
              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "substitutedBody")
              (lambda "_prim" $
                Eithers.bind
                  (Eithers.mapList ("targ" ~> asTerm encodeType @@ var "cx" @@ var "g" @@ var "targ") (var "typeArgs"))
                  ("stypeArgs" ~> lets [
                      "inScopeTypeVarNames">: Sets.fromList (Lists.map
                        ("n" ~> Formatting.capitalize @@ (Core.unName (var "n")))
                        (Sets.toList (Graph.graphTypeVariables $ var "g"))),
                      "hasForallResidual">: Logic.not (Lists.null (Lists.filter ("st" ~>
                        cases Scala._Type (var "st") (Just false) [
                          Scala._Type_var>>: ("tv" ~> lets [
                            "tvName">: project Scala._Type_Name Scala._Type_Name_value @@ (project Scala._Type_Var Scala._Type_Var_name @@ var "tv")] $
                            Logic.and
                              (Logic.not (Lists.elem (int32 46) (Strings.toList (var "tvName"))))
                              (Logic.not (Sets.member (var "tvName") (var "inScopeTypeVarNames"))))])
                        (var "stypeArgs")))] $
                      Logic.ifElse (var "hasForallResidual")
                        (right (ScalaUtilsSource.sprim @@ var "pname"))
                        (right (ScalaUtilsSource.sapplyTypes @@ (ScalaUtilsSource.sprim @@ var "pname") @@ var "stypeArgs")))))]),
      _Term_typeLambda>>: ("tl" ~>
        asTerm encodeTerm @@ var "cx" @@ (Scoping.extendGraphForTypeLambda @@ var "g" @@ var "tl") @@ (Core.typeLambdaBody $ var "tl")),
      _Term_application>>: ("app" ~> lets [
        "fun">: project _Application _Application_function @@ var "app",
        "arg">: project _Application _Application_argument @@ var "app"] $
        cases _Term (Strip.deannotateAndDetypeTerm @@ var "fun")
          (Just $ Eithers.bind
            (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "fun")
            ("sfun" ~>
              Eithers.bind
                (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "arg")
                ("sarg" ~>
                  right (ScalaUtilsSource.sapply @@ var "sfun" @@ list [var "sarg"])))) [
          -- Beta-reduce: Application(Lambda(v, body), arg) → encode body with arg substituted
          -- Common case: Lambda(v, Cases(cs)(v)) applied to subject → subject match { cs }
          _Term_lambda>>: ("lam" ~> lets [
            "lamBody">: Core.lambdaBody $ var "lam"] $
            -- Check if the body is a cases elimination applied to the lambda param
            -- i.e. Application(Cases(cs), Variable(v))
            cases _Term (Strip.deannotateAndDetypeTerm @@ var "lamBody")
              -- Default: just encode as function application
              (Just $ Eithers.bind
                (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "fun")
                ("sfun" ~>
                  Eithers.bind
                    (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "arg")
                    ("sarg" ~>
                      right (ScalaUtilsSource.sapply @@ var "sfun" @@ list [var "sarg"])))) [
              -- Application(Cases(cs), lamParam) → encode as arg match { cs }
              _Term_application>>: ("innerApp" ~> lets [
                "innerFun">: Core.applicationFunction $ var "innerApp"] $
                cases _Term (Strip.deannotateAndDetypeTerm @@ var "innerFun")
                  (Just $ Eithers.bind
                    (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "fun")
                    ("sfun" ~>
                      Eithers.bind
                        (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "arg")
                        ("sarg" ~>
                          right (ScalaUtilsSource.sapply @@ var "sfun" @@ list [var "sarg"])))) [
                  -- Found: Application(Cases(cs), v) — beta-reduce to arg match { cs }
                  _Term_cases>>: (constant $
                    -- Pass arg as the applied argument to encodeFunction's cases handler
                    asTerm encodeFunction @@ var "cx" @@ var "g" @@ (Annotations.termAnnotationInternal @@ var "innerFun") @@ var "innerFun" @@ just (var "arg"))])]),
          _Term_project>>: ("proj" ~> lets [
            "fname">: ScalaUtilsSource.scalaEscapeName @@ (Core.unName (project _Projection _Projection_field @@ var "proj"))] $
            Eithers.bind
              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "arg")
              ("sarg" ~>
                right (inject _Data _Data_ref (inject _Data_Ref _Data_Ref_select (
                  record _Data_Select [
                    _Data_Select_qual>>: var "sarg",
                    _Data_Select_name>>: record _Data_Name [
                      _Data_Name_value>>: wrap _PredefString (var "fname")]]))))),
          _Term_cases>>: (constant $
            asTerm encodeFunction @@ var "cx" @@ var "g" @@ (Annotations.termAnnotationInternal @@ var "fun") @@ var "fun" @@ just (var "arg"))]),
      _Term_lambda>>: (constant $
        asTerm encodeFunction @@ var "cx" @@ var "g" @@ (Annotations.termAnnotationInternal @@ var "term") @@ var "term" @@ nothing),
      _Term_project>>: (constant $
        asTerm encodeFunction @@ var "cx" @@ var "g" @@ (Annotations.termAnnotationInternal @@ var "term") @@ var "term" @@ nothing),
      _Term_cases>>: (constant $
        asTerm encodeFunction @@ var "cx" @@ var "g" @@ (Annotations.termAnnotationInternal @@ var "term") @@ var "term" @@ nothing),
      _Term_unwrap>>: (constant $
        asTerm encodeFunction @@ var "cx" @@ var "g" @@ (Annotations.termAnnotationInternal @@ var "term") @@ var "term" @@ nothing),
      _Term_list>>: ("els" ~>
        Eithers.bind
          (Eithers.mapList ("e" ~> asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "e") (var "els"))
          ("sels" ~>
            right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "Seq") @@ var "sels"))),
      _Term_literal>>: ("v" ~>
        Eithers.bind
          (asTerm encodeLiteral @@ var "cx" @@ var "g" @@ var "v")
          ("slit" ~> lets [
            "litData">: inject _Data _Data_lit (var "slit")] $
            -- Wrap BigInt and BigDecimal literals in constructor calls
            cases _Literal (var "v") (Just $ right (var "litData")) [
              _Literal_decimal>>: (constant $ right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "BigDecimal") @@ list [var "litData"])),
              _Literal_integer>>: ("iv" ~> cases _IntegerValue (var "iv") (Just $ right (var "litData")) [
                _IntegerValue_bigint>>: ("bi" ~> right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "BigInt") @@ list [inject _Data _Data_lit (inject _Lit _Lit_string (Literals.showBigint (var "bi")))])),
                _IntegerValue_uint64>>: ("ui" ~> right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "BigInt") @@ list [inject _Data _Data_lit (inject _Lit _Lit_string (Literals.showBigint (Literals.uint64ToBigint (var "ui"))))]))]),
              _Literal_float>>: ("fv" ~> cases _FloatValue (var "fv") (Just $ right (var "litData")) [
                _FloatValue_bigfloat>>: (constant $ right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "BigDecimal") @@ list [var "litData"]))])])),
      _Term_map>>: ("m" ~>
        Eithers.bind
          (Eithers.mapList ("kv" ~>
            Eithers.bind
              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ (Pairs.first (var "kv")))
              ("sk" ~>
                Eithers.bind
                  (asTerm encodeTerm @@ var "cx" @@ var "g" @@ (Pairs.second (var "kv")))
                  ("sv" ~>
                    right (ScalaUtilsSource.sassign @@ var "sk" @@ var "sv"))))
            (Maps.toList (var "m")))
          ("spairs" ~>
            right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "Map") @@ var "spairs"))),
      _Term_wrap>>: ("wt" ~>
        asTerm encodeTerm @@ var "cx" @@ var "g" @@ (project _WrappedTerm _WrappedTerm_body @@ var "wt")),
      _Term_maybe>>: ("m" ~>
        Maybes.maybe
          (right (ScalaUtilsSource.sname @@ string "None"))
          ("t" ~>
            Eithers.bind
              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "t")
              ("s" ~> right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "Some") @@ list [var "s"])))
          (var "m")),
      _Term_record>>: ("rec" ~> lets [
        "rname">: project _Record _Record_typeName @@ var "rec",
        "fields">: project _Record _Record_fields @@ var "rec",
        "n">: ScalaUtilsSource.scalaTypeName @@ true @@ var "rname"] $
        Eithers.bind
          (Eithers.mapList ("f" ~> asTerm encodeTerm @@ var "cx" @@ var "g" @@ (project _Field _Field_term @@ var "f")) (var "fields"))
          ("args" ~>
            right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ var "n") @@ var "args"))),
      _Term_set>>: ("s" ~>
        Eithers.bind
          (Eithers.mapList ("e" ~> asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "e") (Sets.toList (var "s")))
          ("sels" ~>
            right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "scala.collection.immutable.Set") @@ var "sels"))),
      _Term_inject>>: ("inj" ~> lets [
        "sn">: project _Injection _Injection_typeName @@ var "inj",
        "fn">: project _Field _Field_name @@ (project _Injection _Injection_field @@ var "inj"),
        "ft">: project _Field _Field_term @@ (project _Injection _Injection_field @@ var "inj"),
        "lhs">: ScalaUtilsSource.sname @@ (ScalaUtilsSource.qualifyUnionFieldName @@ string "UNION." @@ just (var "sn") @@ var "fn"),
        -- Get field types from the union type definition if available
        "unionFtypes">: Eithers.either_
          (constant Maps.empty)
          identity
          (Resolution.fieldTypes @@ var "cx" @@ var "g" @@ Core.typeVariable (var "sn"))] $
        -- Check if the field is unit-typed using ftypes, then term structure
        Logic.ifElse (Maybes.maybe
          -- No ftypes: check term structure
          (cases _Term (Strip.deannotateAndDetypeTerm @@ var "ft") (Just false) [
            _Term_unit>>: constant true,
            _Term_record>>: ("rec" ~> Equality.equal (Lists.length (Core.recordFields (var "rec"))) (int32 0))])
          -- Has ftypes: check if field type is unit or empty record
          ("dom" ~> cases _Type (Strip.deannotateType @@ var "dom") (Just false) [
            _Type_unit>>: constant true,
            _Type_record>>: ("rt" ~> Equality.equal (Lists.length (var "rt")) (int32 0))])
          (Maps.lookup (var "fn") (var "unionFtypes")))
          -- Unit-typed: bare constructor name (simple enum case in Scala 3)
          (right (var "lhs"))
          -- Non-unit: apply argument
          (Eithers.bind
            (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "ft")
            ("sarg" ~> right (ScalaUtilsSource.sapply @@ var "lhs" @@ list [var "sarg"])))),
      _Term_variable>>: ("v" ~> lets [
        "fullName">: Core.unName (var "v"),
        "localName">: Names.localNameOf @@ var "v",
        "parts">: Strings.splitOn (string ".") (var "fullName"),
        "numParts">: Lists.length (var "parts"),
        -- Use local name for local variables and hoisted params, qualified for cross-module refs
        "escaped">: Logic.ifElse (Equality.lte (var "numParts") (int32 1))
          (ScalaUtilsSource.scalaEscapeName @@ var "fullName")
          (Logic.ifElse (Equality.equal (var "numParts") (int32 2))
            -- 2 parts: always qualify (e.g. lists.map)
            (Strings.cat2 (Lists.head (var "parts")) (Strings.cat2 (string ".") (ScalaUtilsSource.scalaEscapeName @@ var "localName")))
            -- 3+ parts: fully qualify (module-level functions)
            (Strings.intercalate (string ".") (Lists.concat2
              (Lists.take (Math.sub (var "numParts") (int32 1)) (var "parts"))
              (list [ScalaUtilsSource.scalaEscapeName @@ var "localName"]))))] $
        right (ScalaUtilsSource.sname @@ var "escaped")),
      _Term_annotated>>: ("at" ~>
        asTerm encodeTerm @@ var "cx" @@ var "g" @@ (Core.annotatedTermBody $ var "at")),
      _Term_either>>: ("e" ~>
        Eithers.either_
          ("l" ~>
            Eithers.bind
              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "l")
              ("sl" ~> right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "Left") @@ list [var "sl"])))
          ("r" ~>
            Eithers.bind
              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "r")
              ("sr" ~> right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "Right") @@ list [var "sr"])))
          (var "e")),
      _Term_pair>>: ("p" ~>
        Eithers.bind
          (asTerm encodeTerm @@ var "cx" @@ var "g" @@ (Pairs.first (var "p")))
          ("sf" ~>
            Eithers.bind
              (asTerm encodeTerm @@ var "cx" @@ var "g" @@ (Pairs.second (var "p")))
              ("ss" ~>
                right (ScalaUtilsSource.sapply @@ (ScalaUtilsSource.sname @@ string "Tuple2") @@ list [var "sf", var "ss"])))),
      _Term_unit>>: (constant $ right (inject _Data _Data_lit (inject _Lit _Lit_unit unit))),
      _Term_let>>: ("lt" ~> lets [
        "bindings">: Core.letBindings $ var "lt",
        "body">: Core.letBody $ var "lt",
        -- Extend the graph with the let bindings so type lookups work for recursive refs
        "gLet">: Scoping.extendGraphForLet @@ ("g" ~> "b" ~> Logic.ifElse (Predicates.isComplexBinding @@ var "g" @@ var "b") (just MetaTerms.true) nothing) @@ var "g" @@ var "lt"] $
        Eithers.bind
          (Eithers.mapList (asTerm encodeLetBinding @@ var "cx" @@ var "gLet" @@ (Graph.graphTypeVariables $ var "gLet"))
            (var "bindings"))
          ("sbindings" ~>
            Eithers.bind
              (asTerm encodeTerm @@ var "cx" @@ var "gLet" @@ var "body")
              ("sbody" ~>
                right (inject _Data _Data_block (record _Data_Block [
                  _Data_Block_stats>>: Lists.concat2 (var "sbindings") (list [inject _Stat _Stat_term (var "sbody")])])))))])

encodeTermDefinition :: TTermDefinition (Context -> Graph -> TermDefinition -> Either Error Scala.Stat)
encodeTermDefinition = def "encodeTermDefinition" $
  doc "Encode a term definition as a Scala statement" $
  lambda "cx" $ lambda "g" $ lambda "td" $ lets [
    "name">: project _TermDefinition _TermDefinition_name @@ var "td",
    "term">: project _TermDefinition _TermDefinition_term @@ var "td",
    "lname">: ScalaUtilsSource.scalaEscapeName @@ (Names.localNameOf @@ var "name"),
    "typ'">: Maybes.maybe
      (Core.typeVariable (wrap _Name (string "hydra.core.Unit")))
      (unaryFunction Core.typeSchemeType)
      (project _TermDefinition _TermDefinition_type @@ var "td"),
    -- Check if the type is a function type (needs def) by looking at the stripped type
    "isFunctionType">: cases _Type (Strip.deannotateType @@ var "typ'")
      (Just false)
      [_Type_function>>: constant true,
       _Type_forall>>: ("fa" ~> cases _Type (Strip.deannotateType @@ Core.forallTypeBody (var "fa"))
         (Just false) [_Type_function>>: constant true])]] $
    Logic.ifElse (var "isFunctionType")
      -- Complex binding: extract parameter types from the type signature
      (asTerm encodeComplexTermDef @@ var "cx" @@ var "g" @@ var "lname" @@ var "term" @@ var "typ'")
      -- Simple binding: encode as val with type annotation
      (Eithers.bind
        (asTerm encodeType @@ var "cx" @@ var "g" @@ var "typ'")
        ("stype" ~>
          Eithers.bind
            (asTerm encodeTerm @@ var "cx" @@ var "g" @@ var "term")
            ("rhs" ~>
              right (mkLazyVal (var "lname") (just (var "stype")) (var "rhs")))))

encodeType :: TTermDefinition (Context -> Graph -> Type -> Either Error Scala.Type)
encodeType = def "encodeType" $
  doc "Encode a Hydra type as a Scala type" $
  lambda "cx" $ lambda "g" $ lambda "t" $
    (cases _Type (Strip.deannotateType @@ var "t") (Just $ left (Error.errorOther $ Error.otherError (string "unsupported type"))) [
      _Type_application>>: ("at" ~> lets [
        -- Collect all curried type application args into a flat list
        "collectTypeArgs">: ("t" ~> "acc" ~>
          cases _Type (Strip.deannotateType @@ var "t")
            (Just $ pair (var "t") (var "acc"))
            [_Type_application>>: ("at2" ~> lets [
              "f2">: project _ApplicationType _ApplicationType_function @@ var "at2",
              "a2">: project _ApplicationType _ApplicationType_argument @@ var "at2"] $
              var "collectTypeArgs" @@ var "f2" @@ (Lists.cons (var "a2") (var "acc")))]),
        "collected">: var "collectTypeArgs" @@ (Core.typeApplication $ var "at") @@ (list ([] :: [TTerm Type])),
        "baseFun">: Pairs.first (var "collected"),
        "allArgs">: Pairs.second (var "collected")] $
        Eithers.bind (asTerm encodeType @@ var "cx" @@ var "g" @@ var "baseFun")
          ("sfun" ~> Eithers.bind (Eithers.mapList ("a" ~> asTerm encodeType @@ var "cx" @@ var "g" @@ var "a") (var "allArgs"))
            ("sargs" ~> right (ScalaUtilsSource.stapply @@ var "sfun" @@ var "sargs")))),
      _Type_unit>>: (constant $ right (stref (string "Unit"))),
      _Type_either>>: ("et" ~> lets [
        "lt">: project _EitherType _EitherType_left @@ var "et",
        "rt">: project _EitherType _EitherType_right @@ var "et"] $
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "lt")
          ("slt" ~>
            Eithers.bind
              (asTerm encodeType @@ var "cx" @@ var "g" @@ var "rt")
              ("srt" ~>
                right (ScalaUtilsSource.stapply2 @@ stref (string "Either") @@ var "slt" @@ var "srt")))),
      _Type_function>>: ("ft" ~> lets [
        "dom">: project _FunctionType _FunctionType_domain @@ var "ft",
        "cod">: project _FunctionType _FunctionType_codomain @@ var "ft"] $
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "dom")
          ("sdom" ~>
            Eithers.bind
              (asTerm encodeType @@ var "cx" @@ var "g" @@ var "cod")
              ("scod" ~>
                right (inject Scala._Type _Type_functionType (inject _Type_FunctionType _Type_FunctionType_function (
                  record _Type_Function [
                    _Type_Function_params>>: list [var "sdom"],
                    _Type_Function_res>>: var "scod"])))))),
      _Type_list>>: ("lt" ~>
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "lt")
          ("slt" ~>
            right (ScalaUtilsSource.stapply1 @@ stref (string "Seq") @@ var "slt"))),
      _Type_literal>>: ("lt" ~> cases _LiteralType (var "lt") (Just $ left (Error.errorOther $ Error.otherError (string "unsupported literal type"))) [
        _LiteralType_binary>>: (constant $ right (ScalaUtilsSource.stapply @@ stref (string "Array") @@ list [stref (string "Byte")])),
        _LiteralType_boolean>>: (constant $ right (stref (string "Boolean"))),
        _LiteralType_decimal>>: (constant $ right (stref (string "BigDecimal"))),
        _LiteralType_float>>: ("ft" ~> cases _FloatType (var "ft") (Just $ left (Error.errorOther $ Error.otherError (string "unsupported float type"))) [
          _FloatType_bigfloat>>: (constant $ right (stref (string "BigDecimal"))),
          _FloatType_float32>>: (constant $ right (stref (string "Float"))),
          _FloatType_float64>>: (constant $ right (stref (string "Double")))]),
        _LiteralType_integer>>: ("it" ~> cases _IntegerType (var "it") (Just $ left (Error.errorOther $ Error.otherError (string "unsupported integer type"))) [
          _IntegerType_bigint>>: (constant $ right (stref (string "BigInt"))),
          _IntegerType_int8>>: (constant $ right (stref (string "Byte"))),
          _IntegerType_int16>>: (constant $ right (stref (string "Short"))),
          _IntegerType_int32>>: (constant $ right (stref (string "Int"))),
          _IntegerType_int64>>: (constant $ right (stref (string "Long"))),
          _IntegerType_uint8>>: (constant $ right (stref (string "Byte"))),
          _IntegerType_uint16>>: (constant $ right (stref (string "Int"))),
          _IntegerType_uint32>>: (constant $ right (stref (string "Long"))),
          _IntegerType_uint64>>: (constant $ right (stref (string "BigInt")))]),
        _LiteralType_string>>: (constant $ right (stref (string "scala.Predef.String")))]),
      _Type_map>>: ("mt" ~> lets [
        "kt">: project _MapType _MapType_keys @@ var "mt",
        "vt">: project _MapType _MapType_values @@ var "mt"] $
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "kt")
          ("skt" ~>
            Eithers.bind
              (asTerm encodeType @@ var "cx" @@ var "g" @@ var "vt")
              ("svt" ~>
                right (ScalaUtilsSource.stapply2 @@ stref (string "Map") @@ var "skt" @@ var "svt")))),
      _Type_maybe>>: ("ot" ~>
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "ot")
          ("sot" ~>
            right (ScalaUtilsSource.stapply1 @@ stref (string "Option") @@ var "sot"))),
      _Type_pair>>: ("pt" ~> lets [
        "ft">: project _PairType _PairType_first @@ var "pt",
        "st">: project _PairType _PairType_second @@ var "pt"] $
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "ft")
          ("sft" ~>
            Eithers.bind
              (asTerm encodeType @@ var "cx" @@ var "g" @@ var "st")
              ("sst" ~>
                right (ScalaUtilsSource.stapply2 @@ stref (string "Tuple2") @@ var "sft" @@ var "sst")))),
      _Type_record>>: (constant $
        Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous record type")) (var "cx")),
      _Type_set>>: ("st" ~>
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "st")
          ("sst" ~>
            right (ScalaUtilsSource.stapply1 @@ stref (string "scala.collection.immutable.Set") @@ var "sst"))),
      _Type_union>>: (constant $
        Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous union type")) (var "cx")),
      _Type_wrap>>: (constant $
        Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous wrap type")) (var "cx")),
      _Type_forall>>: ("ft" ~> lets [
        "v">: project _ForallType _ForallType_parameter @@ var "ft",
        "body">: project _ForallType _ForallType_body @@ var "ft"] $
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "body")
          ("sbody" ~>
            right (inject Scala._Type _Type_lambda (record _Type_Lambda [
              _Type_Lambda_tparams>>: list [ScalaUtilsSource.stparam @@ var "v"],
              _Type_Lambda_tpe>>: var "sbody"])))),
      _Type_variable>>: ("v" ~> lets [
        "rawName">: Core.unName (var "v"),
        -- Only capitalize short type variable names (no dots), not qualified type references
        "typeName">: Logic.ifElse (Lists.elem (int32 46) (Strings.toList (var "rawName")))
          (var "rawName")
          (Formatting.capitalize @@ var "rawName")] $
        right (inject Scala._Type _Type_var (record _Type_Var [
          _Type_Var_name>>: record _Type_Name [_Type_Name_value>>: var "typeName"]])))])
  where
    stref s = inject Scala._Type _Type_ref (inject _Type_Ref _Type_Ref_name (record _Type_Name [_Type_Name_value>>: s]))

encodeTypeDefinition :: TTermDefinition (Context -> Graph -> TypeDefinition -> Either Error Scala.Stat)
encodeTypeDefinition = def "encodeTypeDefinition" $
  doc "Encode a type definition as a Scala statement" $
  lambda "cx" $ lambda "g" $ lambda "td" $ lets [
    "name">: project _TypeDefinition _TypeDefinition_name @@ var "td",
    "typ">: Core.typeSchemeType $ project _TypeDefinition _TypeDefinition_type @@ var "td",
    "lname">: Names.localNameOf @@ var "name",
    "tname">: record _Type_Name [_Type_Name_value>>: var "lname"],
    "dname">: record _Data_Name [_Data_Name_value>>: wrap _PredefString (var "lname")],
    "freeVars">: Lists.filter ("v" ~> Logic.not (Lists.elem (int32 46) (Strings.toList (Core.unName (var "v"))))) (Sets.toList (Variables.freeVariablesInType @@ var "typ")),
    "tparams">: Lists.map ("__v" ~> stparam (var "__v")) (var "freeVars")] $
    (cases _Type (Strip.deannotateType @@ var "typ") (Just $ defaultTypeCase (var "lname") (var "tparams") (var "cx") (var "g") (var "typ")) [
      _Type_forall>>: ("ft" ~> lets [
        "forallBody">: project _ForallType _ForallType_body @@ var "ft",
        "forallParam">: project _ForallType _ForallType_parameter @@ var "ft",
        -- Collect all forall-bound type params by recursively stripping foralls
        "collectForallParams">: ("t" ~> "acc" ~>
          cases _Type (Strip.deannotateType @@ var "t")
            (Just $ pair (var "acc") (var "t"))
            [_Type_forall>>: ("ft2" ~>
              var "collectForallParams"
                @@ (Core.forallTypeBody $ var "ft2")
                @@ (Lists.cons (Core.forallTypeParameter $ var "ft2") (var "acc")))]),
        "collected">: var "collectForallParams" @@ var "forallBody" @@ (list [var "forallParam"]),
        "allForallParams">: Lists.reverse (Pairs.first (var "collected")),
        "innerBody">: Pairs.second (var "collected"),
        "allTparams">: Lists.map ("__v" ~> stparam (var "__v")) (var "allForallParams")] $
        -- Unwrap all foralls and check inner body
        cases _Type (Strip.deannotateType @@ var "innerBody")
          (Just $ defaultTypeCase (var "lname") (var "allTparams") (var "cx") (var "g") (var "innerBody"))
          [ _Type_record>>: ("rt2" ~> recordTypeCase (var "tname") (var "allTparams") (var "cx") (var "g") (var "rt2"))
          , _Type_union>>: ("rt2" ~> unionTypeCase (var "tname") (var "lname") (var "allTparams") (var "cx") (var "g") (var "rt2"))
          , _Type_wrap>>: ("wt2" ~>
              Eithers.bind
                (asTerm encodeType @@ var "cx" @@ var "g" @@ var "wt2")
                ("styp" ~>
                  right (inject _Stat _Stat_defn (inject _Defn _Defn_type (
                    record _Defn_Type [
                      _Defn_Type_mods>>: emptyList,
                      _Defn_Type_name>>: var "tname",
                      _Defn_Type_tparams>>: var "allTparams",
                      _Defn_Type_body>>: var "styp"])))))]),
      _Type_record>>: ("rt" ~> recordTypeCase (var "tname") (var "tparams") (var "cx") (var "g") (var "rt")),
      _Type_union>>: ("rt" ~> unionTypeCase (var "tname") (var "lname") (var "tparams") (var "cx") (var "g") (var "rt")),
      _Type_wrap>>: ("wt" ~>
        Eithers.bind
          (asTerm encodeType @@ var "cx" @@ var "g" @@ var "wt")
          ("styp" ~>
            right (inject _Stat _Stat_defn (inject _Defn _Defn_type (
              record _Defn_Type [
                _Defn_Type_mods>>: emptyList,
                _Defn_Type_name>>: var "tname",
                _Defn_Type_tparams>>: var "tparams",
                _Defn_Type_body>>: var "styp"])))))])
  where
    defaultTypeCase :: TTerm String -> TTerm [Scala.Type_Param] -> TTerm Context -> TTerm Graph -> TTerm Type -> TTerm (Either Error Scala.Stat)
    defaultTypeCase lname tparams cx g typ =
      "mkAlias" <~ ("styp" ~>
        right (inject _Stat _Stat_defn (inject _Defn _Defn_type (
          record _Defn_Type [
            _Defn_Type_mods>>: emptyList,
            _Defn_Type_name>>: record _Type_Name [_Type_Name_value>>: lname],
            _Defn_Type_tparams>>: tparams,
            _Defn_Type_body>>: var "styp"])))) $
      -- Try encodeType; if it fails (e.g. for anonymous wrap/record/union), produce a type alias to Any
      Eithers.either_
        (constant $ var "mkAlias" @@ (ScalaUtilsSource.stref @@ string "Any"))
        (var "mkAlias")
        (asTerm encodeType @@ cx @@ g @@ typ)

    recordTypeCase :: TTerm Scala.Type_Name -> TTerm [Scala.Type_Param] -> TTerm Context -> TTerm Graph -> TTerm [FieldType] -> TTerm (Either Error Scala.Stat)
    recordTypeCase tname tparams cx g rt =
      Eithers.bind
        (Eithers.mapList ("f" ~> asTerm fieldToParam @@ cx @@ g @@ var "f") rt)
        ("params" ~>
          right (inject _Stat _Stat_defn (inject _Defn _Defn_class (
            record _Defn_Class [
              _Defn_Class_mods>>: list [inject _Mod _Mod_case unit],
              _Defn_Class_name>>: tname,
              _Defn_Class_tparams>>: tparams,
              _Defn_Class_ctor>>: record _Ctor_Primary [
                _Ctor_Primary_mods>>: emptyList,
                _Ctor_Primary_name>>: inject Scala._Name _Name_value (string ""),
                _Ctor_Primary_paramss>>: list [var "params"]],
              _Defn_Class_template>>: emptyTemplate]))))

    unionTypeCase :: TTerm Scala.Type_Name -> TTerm String -> TTerm [Scala.Type_Param] -> TTerm Context -> TTerm Graph -> TTerm [FieldType] -> TTerm (Either Error Scala.Stat)
    unionTypeCase tname lname tparams cx g rt =
      Eithers.bind
        (Eithers.mapList ("f" ~> asTerm fieldToEnumCase @@ cx @@ g @@ lname @@ tparams @@ var "f") rt)
        ("cases" ~>
          right (inject _Stat _Stat_defn (inject _Defn _Defn_enum (
            record _Defn_Enum [
              _Defn_Enum_mods>>: emptyList,
              _Defn_Enum_name>>: tname,
              _Defn_Enum_tparams>>: tparams,
              _Defn_Enum_ctor>>: record _Ctor_Primary [
                _Ctor_Primary_mods>>: emptyList,
                _Ctor_Primary_name>>: inject Scala._Name _Name_value (string ""),
                _Ctor_Primary_paramss>>: emptyList],
              _Defn_Enum_template>>: record _Template [
                _Template_early>>: emptyList,
                _Template_inits>>: emptyList,
                _Template_self>>: wrap _Self unit,
                _Template_stats>>: var "cases"]]))))

    emptyTemplate = record _Template [
      _Template_early>>: emptyList,
      _Template_inits>>: emptyList,
      _Template_self>>: wrap _Self unit,
      _Template_stats>>: emptyList]

    stparam v = lets [
      "vn">: Formatting.capitalize @@ (Core.unName v)] $
      record _Type_Param [
        _Type_Param_mods>>: emptyList,
        _Type_Param_name>>: inject Scala._Name _Name_value (var "vn"),
        _Type_Param_tparams>>: emptyList,
        _Type_Param_tbounds>>: emptyList,
        _Type_Param_vbounds>>: emptyList,
        _Type_Param_cbounds>>: emptyList]

encodeTypedParam :: TTermDefinition (Context -> Graph -> (Name, Type) -> Either Error Scala.Data_Param)
encodeTypedParam = def "encodeTypedParam" $
  doc "Encode a parameter with its type annotation" $
  lambda "cx" $ lambda "g" $ lambda "pair" $ lets [
    "pname">: ScalaUtilsSource.scalaEscapeName @@ (Names.localNameOf @@ (Pairs.first (var "pair"))),
    "pdom">: Pairs.second (var "pair")] $
    Eithers.bind (asTerm encodeType @@ var "cx" @@ var "g" @@ var "pdom")
      ("sdom" ~> right (record _Data_Param [
        _Data_Param_mods>>: emptyList,
        _Data_Param_name>>: inject Scala._Name _Name_value (var "pname"),
        _Data_Param_decltpe>>: just (var "sdom"),
        _Data_Param_default>>: nothing]))

encodeUntypeApplicationTerm :: TTermDefinition (Context -> Graph -> Term -> Either Error Scala.Data)
encodeUntypeApplicationTerm = def "encodeUntypeApplicationTerm" $
  doc "Encode an untyped application term by first inferring types" $
  lambda "cx" $ lambda "g" $ lambda "term" $
    Eithers.bind
      (Inference.inferInGraphContext @@ var "cx" @@ var "g" @@ var "term")
      ("result" ~>
        asTerm encodeTerm @@ var "cx" @@ var "g" @@ Typing.inferenceResultTerm (var "result"))

-- | Extract the body from a term by peeling off lambdas, type lambdas, lets, etc.
extractBody :: TTermDefinition (Term -> Term)
extractBody = def "extractBody" $
  doc "Extract the innermost body from a term" $
  lambda "t" $
    cases _Term (Strip.deannotateAndDetypeTerm @@ var "t")
      (Just $ var "t") [
      _Term_lambda>>: ("lam" ~> extractBody @@ (Core.lambdaBody $ var "lam")),
      _Term_typeLambda>>: ("tl" ~> extractBody @@ (Core.typeLambdaBody $ var "tl")),
      _Term_typeApplication>>: ("ta" ~> extractBody @@ (Core.typeApplicationTermBody $ var "ta")),
      _Term_let>>: ("lt" ~> extractBody @@ (Core.letBody $ var "lt"))]

-- | Extract the final codomain from a function type
extractCodomain :: TTermDefinition (Type -> Type)
extractCodomain = def "extractCodomain" $
  doc "Extract the final return type from a function type" $
  lambda "t" $
    cases _Type (Strip.deannotateType @@ var "t")
      (Just $ var "t") [
      _Type_function>>: ("ft" ~> extractCodomain @@ (Core.functionTypeCodomain $ var "ft")),
      _Type_forall>>: ("fa" ~> extractCodomain @@ (Core.forallTypeBody $ var "fa"))]

-- | Extract parameter types from a function type by peeling off function arrows
extractDomains :: TTermDefinition (Type -> [Type])
extractDomains = def "extractDomains" $
  doc "Extract domain types from a function type" $
  lambda "t" $
    cases _Type (Strip.deannotateType @@ var "t")
      (Just $ list ([] :: [TTerm Type])) [
      _Type_function>>: ("ft" ~>
        Lists.cons
          (Core.functionTypeDomain $ var "ft")
          (extractDomains @@ (Core.functionTypeCodomain $ var "ft"))),
      _Type_forall>>: ("fa" ~> extractDomains @@ (Core.forallTypeBody $ var "fa"))]

-- | Extract let bindings from a term by peeling off lambdas, type lambdas, etc.
extractLetBindings :: TTermDefinition (Term -> [Binding])
extractLetBindings = def "extractLetBindings" $
  doc "Extract let bindings from a term" $
  lambda "t" $
    cases _Term (Strip.deannotateAndDetypeTerm @@ var "t")
      (Just $ list ([] :: [TTerm Binding])) [
      _Term_lambda>>: ("lam" ~> extractLetBindings @@ (Core.lambdaBody $ var "lam")),
      _Term_typeLambda>>: ("tl" ~> extractLetBindings @@ (Core.typeLambdaBody $ var "tl")),
      _Term_typeApplication>>: ("ta" ~> extractLetBindings @@ (Core.typeApplicationTermBody $ var "ta")),
      _Term_let>>: ("lt" ~>
        Lists.concat2
          (Core.letBindings $ var "lt")
          (extractLetBindings @@ (Core.letBody $ var "lt")))]

-- | Extract parameter names from a term by peeling off lambdas
extractParams :: TTermDefinition (Term -> [Name])
extractParams = def "extractParams" $
  doc "Extract parameter names from a term" $
  lambda "t" $
    cases _Term (Strip.deannotateAndDetypeTerm @@ var "t")
      (Just $ list ([] :: [TTerm Name])) [
      _Term_lambda>>: ("lam" ~>
        Lists.cons
          (Core.lambdaParameter $ var "lam")
          (extractParams @@ (Core.lambdaBody $ var "lam"))),
      _Term_typeLambda>>: ("tl" ~> extractParams @@ (Core.typeLambdaBody $ var "tl")),
      _Term_typeApplication>>: ("ta" ~> extractParams @@ (Core.typeApplicationTermBody $ var "ta")),
      _Term_let>>: ("lt" ~> extractParams @@ (Core.letBody $ var "lt"))]

fieldToEnumCase :: TTermDefinition (Context -> Graph -> String -> [Scala.Type_Param] -> FieldType -> Either Error Scala.Stat)
fieldToEnumCase = def "fieldToEnumCase" $
  doc "Convert a field type to a Scala enum case" $
  lambda "cx" $ lambda "g" $ lambda "parentName" $ lambda "tparams" $ lambda "ft" $ lets [
    "fname">: ScalaUtilsSource.scalaEscapeName @@ (Core.unName (project _FieldType _FieldType_name @@ var "ft")),
    "ftyp">: project _FieldType _FieldType_type @@ var "ft",
    "caseName">: record _Data_Name [_Data_Name_value>>: wrap _PredefString (var "fname")],
    "isUnit">: cases _Type (Strip.deannotateType @@ var "ftyp") (Just false) [
      _Type_unit>>: (constant true),
      _Type_record>>: ("rt" ~> Equality.equal (Lists.length (var "rt")) (int32 0))],
    "parentType">: Logic.ifElse (Lists.null (var "tparams"))
      (stref (var "parentName"))
      (inject Scala._Type _Type_apply (record _Type_Apply [
        _Type_Apply_tpe>>: stref (var "parentName"),
        _Type_Apply_args>>: Lists.map (asTerm typeParamToTypeVar) (var "tparams")]))] $
    Eithers.bind
      (asTerm encodeType @@ var "cx" @@ var "g" @@ var "ftyp")
      ("sftyp" ~>
        right (inject _Stat _Stat_defn (inject _Defn _Defn_enumCase (
          record _Defn_EnumCase [
            _Defn_EnumCase_mods>>: emptyList,
            _Defn_EnumCase_name>>: var "caseName",
            _Defn_EnumCase_tparams>>: emptyList,
            _Defn_EnumCase_ctor>>: record _Ctor_Primary [
              _Ctor_Primary_mods>>: emptyList,
              _Ctor_Primary_name>>: inject Scala._Name _Name_value (string ""),
              _Ctor_Primary_paramss>>: list [
                Logic.ifElse (var "isUnit")
                  (emptyList)
                  (list [record _Data_Param [
                    _Data_Param_mods>>: emptyList,
                    _Data_Param_name>>: inject Scala._Name _Name_value (string "value"),
                    _Data_Param_decltpe>>: just (var "sftyp"),
                    _Data_Param_default>>: nothing]])]],
            _Defn_EnumCase_inits>>: list [record _Init [
              _Init_tpe>>: var "parentType",
              _Init_name>>: inject Scala._Name _Name_value (string ""),
              _Init_argss>>: emptyList]]]))))
  where
    stref s = inject Scala._Type _Type_ref (inject _Type_Ref _Type_Ref_name (record _Type_Name [_Type_Name_value>>: s]))

fieldToParam :: TTermDefinition (Context -> Graph -> FieldType -> Either Error Scala.Data_Param)
fieldToParam = def "fieldToParam" $
  doc "Convert a field type to a Scala parameter" $
  lambda "cx" $ lambda "g" $ lambda "ft" $ lets [
    "fname">: ScalaUtilsSource.scalaEscapeName @@ (Core.unName (project _FieldType _FieldType_name @@ var "ft")),
    "ftyp">: project _FieldType _FieldType_type @@ var "ft"] $
    Eithers.bind
      (asTerm encodeType @@ var "cx" @@ var "g" @@ var "ftyp")
      ("sftyp" ~>
        right (record _Data_Param [
          _Data_Param_mods>>: emptyList,
          _Data_Param_name>>: inject Scala._Name _Name_value (var "fname"),
          _Data_Param_decltpe>>: just (var "sftyp"),
          _Data_Param_default>>: nothing]))

-- | Type alias for Result
-- type Result a = Either Error a

-- | Get a type annotation, converting DecodingError to InContext Error.
getTypeE :: TTerm Context -> TTerm Graph -> TTerm (M.Map Name Term) -> TTerm (Either Error (Maybe Type))
getTypeE cx g ann = Eithers.bimap
  ("__de" ~> Error.errorOther (Error.otherError ((unwrap _DecodingError) @@ var "__de")))
  ("__a" ~> var "__a")
  (Annotations.getType @@ g @@ ann)

findDomain :: TTermDefinition (Context -> Graph -> M.Map Name Term -> Either Error Type)
findDomain = def "findDomain" $
  doc "Find the domain type from annotations" $
  lambda "cx" $ lambda "g" $ lambda "meta" $
    Eithers.bind
      (getTypeE (var "cx") (var "g") (var "meta"))
      ("r" ~> Maybes.maybe
        (left (Error.errorOther $ Error.otherError (string "expected a typed term")))
        ("t" ~> cases _Type (Strip.deannotateType @@ var "t") (Just $ left (Error.errorOther $ Error.otherError (string "expected a function type"))) [
          _Type_function>>: ("ft" ~> right (project _FunctionType _FunctionType_domain @@ var "ft"))])
        (var "r"))

findImports :: TTermDefinition (Context -> Graph -> Module -> Either Error [Scala.Stat])
findImports = def "findImports" $
  doc "Find import statements for the module" $
  lambda "cx" $ lambda "g" $ lambda "mod" $
    Eithers.bind
      (Analysis.moduleDependencyNamespaces @@ var "cx" @@ var "g" @@ false @@ false @@ true @@ false @@ var "mod")
      ("elImps" ~>
        Eithers.bind
          (Analysis.moduleDependencyNamespaces @@ var "cx" @@ var "g" @@ false @@ true @@ false @@ false @@ var "mod")
          ("primImps" ~>
            right (Lists.concat (list [
              Lists.map (asTerm toElImport) (Sets.toList (var "elImps")),
              Lists.map (asTerm toPrimImport) (Sets.toList (var "primImps"))]))))

findSdom :: TTermDefinition (Context -> Graph -> M.Map Name Term -> Either Error (Maybe Scala.Type))
findSdom = def "findSdom" $
  doc "Find the Scala domain type for a function from annotations" $
  lambda "cx" $ lambda "g" $ lambda "meta" $
    Eithers.bind
      (getTypeE (var "cx") (var "g") (var "meta"))
      ("mtyp" ~> Maybes.maybe
        -- No type annotation: return Nothing (shouldn't happen in well-typed terms)
        (right nothing)
        ("t" ~> cases _Type (Strip.deannotateType @@ var "t")
          (Just $
            -- Not a function type but has type annotation: encode the full type
            Eithers.bind
              (asTerm encodeType @@ var "cx" @@ var "g" @@ var "t")
              ("st" ~> right (just (var "st")))) [
          _Type_function>>: ("ft" ~> lets [
            "dom">: project _FunctionType _FunctionType_domain @@ var "ft"] $
            Eithers.bind
              (asTerm encodeType @@ var "cx" @@ var "g" @@ var "dom")
              ("sdom" ~> right (just (var "sdom")))),
          _Type_forall>>: ("fa" ~> cases _Type (Strip.deannotateType @@ Core.forallTypeBody (var "fa"))
            (Just $ right nothing) [
            _Type_function>>: ("ft2" ~> lets [
              "dom2">: project _FunctionType _FunctionType_domain @@ var "ft2"] $
              Eithers.bind
                (asTerm encodeType @@ var "cx" @@ var "g" @@ var "dom2")
                ("sdom2" ~> right (just (var "sdom2"))))])])
        (var "mtyp"))

moduleToScala :: TTermDefinition (Module -> [Definition] -> Context -> Graph -> Either Error (M.Map FilePath String))
moduleToScala = def "moduleToScala" $
  doc "Convert a Hydra module to Scala source code" $
  lambda "mod" $ lambda "defs" $ lambda "cx" $ lambda "g" $
    Eithers.bind
      (asTerm constructModule @@ var "cx" @@ var "g" @@ var "mod" @@ var "defs")
      ("pkg" ~> lets [
        "s">: SerializationSource.printExpr @@ (SerializationSource.parenthesize @@ (TTerm (TermVariable (Name "hydra.scala.serde.writePkg")) @@ var "pkg"))] $
        right (Maps.singleton
          (Names.namespaceToFilePath @@ Util.caseConventionCamel @@ wrap _FileExtension (string "scala") @@ Packaging.moduleNamespace (var "mod"))
          (var "s")))

-- | Strip wrap eliminations from a term (newtypes are erased in Scala)
stripWrapEliminations :: TTermDefinition (Term -> Term)
stripWrapEliminations = def "stripWrapEliminations" $
  doc "Strip wrap eliminations from terms (newtypes are erased in Scala)" $
  lambda "t" $
    cases _Term (Strip.deannotateAndDetypeTerm @@ var "t")
      (Just $ var "t") [
      _Term_application>>: ("app" ~> lets [
        "appFun">: Core.applicationFunction $ var "app",
        "appArg">: Core.applicationArgument $ var "app"] $
        cases _Term (Strip.deannotateAndDetypeTerm @@ var "appFun")
          (Just $ var "t") [
          -- unwrap(value) → value
          _Term_unwrap>>: (constant $ stripWrapEliminations @@ var "appArg"),
          -- unwrap(x)(arg) → x(arg) — strip the wrap but keep the intermediate application
          _Term_application>>: ("innerApp" ~> lets [
            "innerFun">: Core.applicationFunction $ var "innerApp",
            "innerArg">: Core.applicationArgument $ var "innerApp"] $
            cases _Term (Strip.deannotateAndDetypeTerm @@ var "innerFun")
              (Just $ var "t") [
              _Term_unwrap>>: (constant $
                -- Reconstruct as innerArg(appArg) with wrap stripped
                stripWrapEliminations @@ Core.termApplication (record _Application [
                  _Application_function>>: var "innerArg",
                  _Application_argument>>: var "appArg"]))])])]

toElImport :: TTermDefinition (Namespace -> Scala.Stat)
toElImport = def "toElImport" $
  doc "Create an element import statement" $
  lambda "ns" $
    inject _Stat _Stat_importExport (
      inject _ImportExportStat _ImportExportStat_import (
        record _Import [
          _Import_importers>>: list [
            record _Importer [
              _Importer_ref>>: inject _Data_Ref _Data_Ref_name (
                record _Data_Name [
                  _Data_Name_value>>: wrap _PredefString (
                    Strings.intercalate (string ".") (Strings.splitOn (string ".") (Packaging.unNamespace (var "ns"))))]),
              _Importer_importees>>: list [inject _Importee _Importee_wildcard unit]]]]))

toPrimImport :: TTermDefinition (Namespace -> Scala.Stat)
toPrimImport = def "toPrimImport" $
  doc "Create a primitive import statement" $
  lambda "ns" $
    inject _Stat _Stat_importExport (
      inject _ImportExportStat _ImportExportStat_import (
        record _Import [
          _Import_importers>>: list [
            record _Importer [
              _Importer_ref>>: inject _Data_Ref _Data_Ref_name (
                record _Data_Name [
                  _Data_Name_value>>: wrap _PredefString (
                    Strings.intercalate (string ".") (Strings.splitOn (string ".") (Packaging.unNamespace (var "ns"))))]),
              _Importer_importees>>: emptyList]]]))

typeParamToTypeVar :: TTermDefinition (Scala.Type_Param -> Scala.Type)
typeParamToTypeVar = def "typeParamToTypeVar" $
  doc "Convert a type parameter to a type variable reference" $
  lambda "tp" $ lets [
    "n">: project _Type_Param _Type_Param_name @@ var "tp",
    "s">: cases Scala._Name (var "n") (Just $ string "") [
      Scala._Name_value>>: ("v" ~> var "v")]] $
    inject Scala._Type _Type_var (record _Type_Var [
      _Type_Var_name>>: record _Type_Name [_Type_Name_value>>: var "s"]])

-- | Helper to construct a Scala val statement
mkVal :: TTerm String -> TTerm (Maybe Scala.Type) -> TTerm Scala.Data -> TTerm Scala.Stat
mkVal vname mdecltpe rhs =
  inject _Stat _Stat_defn (inject _Defn _Defn_val (record _Defn_Val [
    _Defn_Val_mods>>: emptyList,
    _Defn_Val_pats>>: list [inject _Pat _Pat_var (record _Pat_Var [
      _Pat_Var_name>>: record _Data_Name [_Data_Name_value>>: wrap _PredefString vname]])],
    _Defn_Val_decltpe>>: mdecltpe,
    _Defn_Val_rhs>>: rhs]))


-- Name references used by Coder

-- Scala Meta names (re-exported from Utils for convenience)
_Data = Scala._Data
_Data_Apply = Scala._Data_Apply
_Data_Assign = Scala._Data_Assign
_Data_FunctionData = Scala._Data_FunctionData
_Data_FunctionData_function = Scala._Data_FunctionData_function
_Data_Function = Scala._Data_Function
_Data_Function_params = Name "params"
_Data_Function_body = Name "body"
_Data_Match = Scala._Data_Match
_Data_Match_expr = Name "expr"
_Data_Match_cases = Name "cases"
_Data_Name = Scala._Data_Name
_Data_Name_value = Name "value"
_Data_Param = Scala._Data_Param
_Data_Param_mods = Name "mods"
_Data_Param_name = Name "name"
_Data_Param_decltpe = Name "decltpe"
_Data_Param_default = Name "default"
_Data_Ref = Scala._Data_Ref
_Data_Ref_name = Name "name"
_Data_Ref_select = Name "select"
_Data_Select = Scala._Data_Select
_Data_Select_qual = Name "qual"
_Data_Select_name = Name "name"

_Data_Block = Scala._Data_Block
_Data_Block_stats = Scala._Data_Block_stats

_Data_apply = Scala._Data_apply
_Data_assign = Scala._Data_assign
_Data_block = Scala._Data_block
_Data_functionData = Scala._Data_functionData
_Data_ref = Scala._Data_ref
_Data_lit = Scala._Data_lit
_Data_match = Scala._Data_match

_Name_value = Scala._Name_value

_Type_Apply = Scala._Type_Apply
_Type_Apply_tpe = Name "tpe"
_Type_Apply_args = Name "args"
_Type_FunctionType = Scala._Type_FunctionType
_Type_FunctionType_function = Name "function"
_Type_Function = Scala._Type_Function
_Type_Function_params = Name "params"
_Type_Function_res = Name "res"
_Type_Lambda = Scala._Type_Lambda
_Type_Lambda_tparams = Name "tparams"
_Type_Lambda_tpe = Name "tpe"
_Type_Name = Scala._Type_Name
_Type_Name_value = Name "value"
_Type_Param = Scala._Type_Param
_Type_Param_mods = Name "mods"
_Type_Param_name = Name "name"
_Type_Param_tparams = Name "tparams"
_Type_Param_tbounds = Name "tbounds"
_Type_Param_vbounds = Name "vbounds"
_Type_Param_cbounds = Name "cbounds"
_Type_Ref = Scala._Type_Ref
_Type_Ref_name = Name "name"
_Type_Var = Scala._Type_Var
_Type_Var_name = Name "name"

_Type_apply = Scala._Type_apply
_Type_functionType = Scala._Type_functionType
_Type_lambda = Scala._Type_lambda
_Type_ref = Scala._Type_ref
_Type_var = Scala._Type_var

_Pat = Scala._Pat
_Pat_Var = Scala._Pat_Var
_Pat_Var_name = Name "name"
_Pat_Extract = Scala._Pat_Extract
_Pat_Extract_fun = Name "fun"
_Pat_Extract_args = Name "args"
_Pat_var = Scala._Pat_var
_Pat_wildcard = Scala._Pat_wildcard
_Pat_extract = Name "extract"

_PredefString = Scala._PredefString

_Lit = Scala._Lit
_Lit_boolean = Scala._Lit_boolean
_Lit_float = Scala._Lit_float
_Lit_double = Scala._Lit_double
_Lit_short = Scala._Lit_short
_Lit_int = Scala._Lit_int
_Lit_long = Scala._Lit_long
_Lit_byte = Scala._Lit_byte
_Lit_string = Scala._Lit_string
_Lit_unit = Scala._Lit_unit

_Stat = Scala._Stat
_Stat_term = Scala._Stat_term
_Stat_defn = Scala._Stat_defn
_Stat_importExport = Scala._Stat_importExport

_Defn = Scala._Defn
_Defn_Class = Scala._Defn_Class
_Defn_Class_mods = Name "mods"
_Defn_Class_name = Name "name"
_Defn_Class_tparams = Name "tparams"
_Defn_Class_ctor = Name "ctor"
_Defn_Class_template = Name "template"
_Defn_Def = Scala._Defn_Def
_Defn_Def_mods = Name "mods"
_Defn_Def_name = Name "name"
_Defn_Def_tparams = Name "tparams"
_Defn_Def_paramss = Name "paramss"
_Defn_Def_decltpe = Name "decltpe"
_Defn_Def_body = Name "body"
_Defn_Enum = Scala._Defn_Enum
_Defn_Enum_mods = Name "mods"
_Defn_Enum_name = Name "name"
_Defn_Enum_tparams = Name "tparams"
_Defn_Enum_ctor = Name "ctor"
_Defn_Enum_template = Name "template"
_Defn_EnumCase = Scala._Defn_EnumCase
_Defn_EnumCase_mods = Name "mods"
_Defn_EnumCase_name = Name "name"
_Defn_EnumCase_tparams = Name "tparams"
_Defn_EnumCase_ctor = Name "ctor"
_Defn_EnumCase_inits = Name "inits"
_Defn_Type = Scala._Defn_Type
_Defn_Type_mods = Name "mods"
_Defn_Type_name = Name "name"
_Defn_Type_tparams = Name "tparams"
_Defn_Type_body = Name "body"
_Defn_Val = Scala._Defn_Val
_Defn_Val_mods = Name "mods"
_Defn_Val_pats = Name "pats"
_Defn_Val_decltpe = Name "decltpe"
_Defn_Val_rhs = Name "rhs"

_Defn_class = Name "class"
_Defn_def = Name "def"
_Defn_enum = Name "enum"
_Defn_enumCase = Name "enumCase"
_Defn_type = Name "type"
_Defn_val = Name "val"

_Ctor_Primary = Scala._Ctor_Primary
_Ctor_Primary_mods = Name "mods"
_Ctor_Primary_name = Name "name"
_Ctor_Primary_paramss = Name "paramss"
_Template = Scala._Template
_Template_early = Name "early"
_Template_inits = Name "inits"
_Template_self = Name "self"
_Template_stats = Name "stats"
_Self = Scala._Self
_Mod = Scala._Mod
_Mod_case = Name "case"
_Pkg = Scala._Pkg
_Pkg_name = Name "name"
_Pkg_ref = Name "ref"
_Pkg_stats = Name "stats"
_Import = Scala._Import
_Import_importers = Name "importers"
_Importer = Scala._Importer
_Importer_ref = Name "ref"
_Importer_importees = Name "importees"
_Importee = Scala._Importee
_Importee_wildcard = Name "wildcard"
_ImportExportStat = Scala._ImportExportStat
_ImportExportStat_import = Name "import"
_Init = Scala._Init
_Init_tpe = Name "tpe"
_Init_name = Name "name"
_Init_argss = Name "argss"
_Case = Scala._Case
_Case_pat = Name "pat"
_Case_cond = Name "cond"
_Case_body = Name "body"


