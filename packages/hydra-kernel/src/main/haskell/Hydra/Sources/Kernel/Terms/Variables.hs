
module Hydra.Sources.Kernel.Terms.Variables where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  freeTypeVariablesInTerm,
  freeVariablesInTerm,
  freeVariablesInType,
  freeVariablesInTypeOrdered,
  freeVariablesInTypeSimple,
  freeVariablesInTypeScheme,
  freeVariablesInTypeSchemeSimple,
  isFreeVariableInTerm,
  normalizeTypeVariablesInTerm,
  replaceFreeTermVariable,
  replaceFreeTypeVariable,
  substituteTypeVariables,
  substituteVariable,
  substituteVariables,
  unshadowVariables)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths        as Paths
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
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Sorting as Sorting


ns :: Namespace
ns = Namespace "hydra.variables"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [Names.ns, Rewriting.ns],
            moduleTypeDependencies = kernelTypesNamespaces,
            moduleDescription = Just ("Free variable analysis, term-level substitution, and unshadowing")}
  where
   definitions = [
     toDefinition freeTypeVariablesInTerm,
     toDefinition freeVariablesInTerm,
     toDefinition freeVariablesInType,
     toDefinition freeVariablesInTypeOrdered,
     toDefinition freeVariablesInTypeScheme,
     toDefinition freeVariablesInTypeSchemeSimple,
     toDefinition freeVariablesInTypeSimple,
     toDefinition isFreeVariableInTerm,
     toDefinition normalizeTypeVariablesInTerm,
     toDefinition replaceFreeTermVariable,
     toDefinition replaceFreeTypeVariable,
     toDefinition substituteTypeVariables,
     toDefinition substituteVariable,
     toDefinition substituteVariables,
     toDefinition unshadowVariables]

freeTypeVariablesInTerm :: TTermDefinition (Term -> S.Set Name)
freeTypeVariablesInTerm = define "freeTypeVariablesInTerm" $
  doc ("Get the set of free type variables in a term (including schema names, where they appear in type annotations)."
    <> " In this context, only the type schemes of let bindings can bind type variables; type lambdas do not.") $
  "term0" ~>
  "allOf" <~ ("sets" ~> Lists.foldl (binaryFunction Sets.union) Sets.empty $ var "sets") $
  "tryType" <~ ("tvars" ~> "typ" ~> Sets.difference (freeVariablesInType @@ var "typ") (var "tvars")) $
  "getAll" <~ ("vars" ~> "term" ~>
    "recurse" <~ var "getAll" @@ var "vars" $
    "dflt" <~ (var "allOf" @@ Lists.map (var "recurse") (Rewriting.subterms @@ var "term")) $
    cases _Term (var "term")
      (Just $ var "dflt") [
      _Term_lambda>>: "l" ~>
        "domt" <~ optCases (Core.lambdaDomain $ var "l") (Sets.empty) (var "tryType" @@ var "vars") $
        Sets.union (var "domt") (var "recurse" @@ (Core.lambdaBody $ var "l")),
      _Term_let>>: "l" ~>
        "forBinding" <~ ("b" ~>
          "newVars" <~ optCases (Core.bindingTypeScheme $ var "b")
             (var "vars")
             ("ts" ~> Sets.union (var "vars") (Sets.fromList $ Core.typeSchemeVariables $ var "ts")) $
          Sets.union
            (var "getAll" @@ var "newVars" @@ (Core.bindingTerm $ var "b"))
            (optCases (Core.bindingTypeScheme $ var "b")
              Sets.empty
              ("ts" ~> var "tryType" @@ var "newVars" @@ (Core.typeSchemeBody $ var "ts")))) $
        Sets.union
          (var "allOf" @@ Lists.map (var "forBinding") (Core.letBindings $ var "l"))
          (var "recurse" @@ (Core.letBody $ var "l")),
      _Term_typeApplication>>: "tt" ~>
        Sets.union
          (var "tryType" @@ var "vars" @@ (Core.typeApplicationTermType $ var "tt"))
          (var "recurse" @@ (Core.typeApplicationTermBody $ var "tt")),
      _Term_typeLambda>>: "tl" ~>
        Sets.union
          -- The type variable introduced by a type lambda is considered unbound unless it is also introduced in an
          -- enclosing let binding, as all type lambda terms are in Hydra.
          (var "tryType" @@ var "vars" @@ (Core.typeVariable $ Core.typeLambdaParameter $ var "tl"))
          (var "recurse" @@ (Core.typeLambdaBody $ var "tl"))]) $
  var "getAll" @@ Sets.empty @@ var "term0"

freeVariablesInTerm :: TTermDefinition (Term -> S.Set Name)
freeVariablesInTerm = define "freeVariablesInTerm" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a term" $
  "term" ~>
  -- Note: the subterm fold is wrapped in a lambda (dfltVars) rather than precomputed in a let binding,
  -- because in eager languages a let-bound default would be evaluated unconditionally, even for
  -- Variable/Lambda/Let cases where it is not needed. This avoids redundant traversal.
  "dfltVars" <~ ("_" ~> Lists.foldl ("s" ~> "t" ~> Sets.union (var "s") (freeVariablesInTerm @@ var "t"))
    Sets.empty
    (Rewriting.subterms @@ var "term")) $
  cases _Term (var "term")
    (Just $ var "dfltVars" @@ unit) [
    _Term_lambda>>: "l" ~> Sets.delete
      (Core.lambdaParameter $ var "l")
      (freeVariablesInTerm @@ (Core.lambdaBody $ var "l")),
    _Term_let>>: "l" ~> Sets.difference
      (var "dfltVars" @@ unit)
      (Sets.fromList (Lists.map (unaryFunction Core.bindingName) (Core.letBindings $ var "l"))),
    _Term_variable>>: "v" ~> Sets.singleton $ var "v"]

freeVariablesInType :: TTermDefinition (Type -> S.Set Name)
freeVariablesInType = define "freeVariablesInType" $
  doc "Find the free variables (i.e. variables not bound by a lambda or let) in a type" $
  "typ" ~>
  "dfltVars" <~ Phantoms.fold ("s" ~> "t" ~> Sets.union (var "s") (recurse @@ var "t"))
    @@ Sets.empty
    @@ (Rewriting.subtypes @@ var "typ") $
  cases _Type (var "typ")
    (Just $ var "dfltVars") [
    _Type_forall>>: "lt" ~> Sets.delete
        (Core.forallTypeParameter $ var "lt")
        (recurse @@ (Core.forallTypeBody $ var "lt")),
    -- TODO: let-types
    _Type_variable>>: "v" ~> Sets.singleton $ var "v"]
  where
    recurse = freeVariablesInType

freeVariablesInTypeOrdered :: TTermDefinition (Type -> [Name])
freeVariablesInTypeOrdered = define "freeVariablesInTypeOrdered" $
  doc "Find the free variables in a type in deterministic left-to-right order" $
  "typ" ~>
  "collectVars" <~ ("boundVars" ~> "t" ~>
    cases _Type (var "t")
      (Just $ Lists.concat $ Lists.map (var "collectVars" @@ var "boundVars") $
              Rewriting.subtypes @@ var "t") [
      _Type_variable>>: "v" ~>
        Logic.ifElse (Sets.member (var "v") (var "boundVars"))
          (list ([] :: [TTerm Name]))
          (list [var "v"]),
      _Type_forall>>: "ft" ~>
        var "collectVars" @@
          (Sets.insert (Core.forallTypeParameter $ var "ft") (var "boundVars")) @@
          (Core.forallTypeBody $ var "ft")]) $
  (Lists.nub :: TTerm [Name] -> TTerm [Name]) $ var "collectVars" @@ Sets.empty @@ var "typ"

freeVariablesInTypeSimple :: TTermDefinition (Type -> S.Set Name)
freeVariablesInTypeSimple = define "freeVariablesInTypeSimple" $
  doc "Same as freeVariablesInType, but ignores the binding action of lambda types" $
  "typ" ~>
  "helper" <~ ("types" ~> "typ" ~> cases _Type (var "typ")
    (Just $ var "types") [
    _Type_variable>>: "v" ~> Sets.insert (var "v") (var "types")]) $
  Rewriting.foldOverType @@ Coders.traversalOrderPre @@ var "helper" @@ Sets.empty @@ var "typ"

freeVariablesInTypeScheme :: TTermDefinition (TypeScheme -> S.Set Name)
freeVariablesInTypeScheme = define "freeVariablesInTypeScheme" $
  doc "Find free variables in a type scheme" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "t" <~ Core.typeSchemeBody (var "ts") $
  Sets.difference (freeVariablesInType @@ var "t") (Sets.fromList $ var "vars")

freeVariablesInTypeSchemeSimple :: TTermDefinition (TypeScheme -> S.Set Name)
freeVariablesInTypeSchemeSimple = define "freeVariablesInTypeSchemeSimple" $
  doc "Find free variables in a type scheme (simple version)" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "t" <~ Core.typeSchemeBody (var "ts") $
  Sets.difference (freeVariablesInTypeSimple @@ var "t") (Sets.fromList $ var "vars")

isFreeVariableInTerm :: TTermDefinition (Name -> Term -> Bool)
isFreeVariableInTerm = define "isFreeVariableInTerm" $
 doc "Check whether a variable is free (not bound) in a term" $
 "v" ~> "term" ~>
   Logic.not $ Sets.member (var "v") (freeVariablesInTerm @@ var "term")

normalizeTypeVariablesInTerm :: TTermDefinition (Term -> Term)
normalizeTypeVariablesInTerm = define "normalizeTypeVariablesInTerm" $
  doc "Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ..." $
  "term" ~>
  "replaceName" <~ ("subst" ~> "v" ~> Maybes.fromMaybe (var "v") $ Maps.lookup (var "v") (var "subst")) $
  "substType" <~ ("subst" ~> "typ" ~>
    "rewrite" <~ ("recurse" ~> "typ" ~> cases _Type (var "typ")
      (Just $ var "recurse" @@ var "typ") [
      _Type_variable>>: "v" ~> Core.typeVariable $ var "replaceName" @@ var "subst" @@ var "v"]) $
    Rewriting.rewriteType @@ var "rewrite" @@ var "typ") $
  -- Thread a triple: ((subst, boundVars), next)
  "rewriteWithSubst" <~ ("state" ~> "term0" ~>
    "sb"   <~ Pairs.first  (var "state") $
    "next" <~ Pairs.second (var "state") $
    "subst"     <~ Pairs.first  (var "sb") $
    "boundVars" <~ Pairs.second (var "sb") $
    "rewrite" <~ ("recurse" ~> "term" ~> cases _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      -- Lambdas have a "domain" type which needs to be rewritten
      _Term_lambda>>: "l" ~>
        "domain" <~ Core.lambdaDomain (var "l") $
        Core.termLambda $ Core.lambda
          (Core.lambdaParameter $ var "l")
          (Maybes.map (var "substType" @@ var "subst") (var "domain"))
          (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.lambdaBody $ var "l")),
      -- Let bindings each have a type which needs to be rewritten
      _Term_let>>: "lt" ~>
        "bindings0" <~ Core.letBindings (var "lt") $
        "body0"     <~ Core.letBody (var "lt") $
        -- Sequentially rewrite bindings without advancing 'next' across siblings
        "step" <~ ("acc" ~> "bs" ~>
          Maybes.maybe
            (Lists.reverse (var "acc"))
            ("uc" ~>
          "b"  <~ Pairs.first (var "uc") $
          "tl" <~ Pairs.second (var "uc") $
          "noType" <~ (
            "newVal" <~ var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.bindingTerm $ var "b") $
            "b1"     <~ Core.binding (Core.bindingName $ var "b") (var "newVal") nothing $
            var "step" @@ (Lists.cons (var "b1") (var "acc")) @@ var "tl") $
          "withType" <~ ("ts" ~>
            "vars" <~ Core.typeSchemeVariables (var "ts") $
            "typ"  <~ Core.typeSchemeBody (var "ts") $
            "k"    <~ Lists.length (var "vars") $
            -- Build exactly k fresh names t{next}, t{next+1}, ...
            "gen"  <~ ("i" ~> "rem" ~> "acc2" ~>
              "ti" <~ Core.name (Strings.cat2 (string "t") (Literals.showInt32 (Math.add (var "next") (var "i")))) $
              Logic.ifElse (Equality.equal (var "rem") (int32 0))
                (Lists.reverse (var "acc2"))
                (var "gen"
                  @@ (Math.add (var "i") (int32 1))
                  @@ (Math.sub (var "rem") (int32 1))
                  @@ (Lists.cons (var "ti") (var "acc2")))) $
            "newVars"  <~ var "gen" @@ (int32 0) @@ (var "k") @@ (list ([] :: [TTerm Name])) $
            "newSubst" <~ Maps.union (Maps.fromList $ Lists.zip (var "vars") (var "newVars")) (var "subst") $
            "newBound" <~ Sets.union (var "boundVars") (Sets.fromList (var "newVars")) $
            "newVal"   <~ var "rewriteWithSubst" @@ (pair (pair (var "newSubst") (var "newBound")) (Math.add (var "next") (var "k"))) @@ (Core.bindingTerm $ var "b") $
            -- Rename constraint keys using newSubst (a Map Name Name)
            -- For each (varName, metadata), if varName is in newSubst, use the new name
            "renameConstraintKeys" <~ ("constraintMap" ~>
              Maps.fromList $ Lists.map
                ("p" ~>
                  "oldName" <~ Pairs.first (var "p") $
                  "meta" <~ Pairs.second (var "p") $
                  "newName" <~ Maybes.fromMaybe (var "oldName") (Maps.lookup (var "oldName") (var "newSubst")) $
                  pair (var "newName") (var "meta"))
                (Maps.toList $ var "constraintMap")) $
            "oldConstraints" <~ Core.typeSchemeConstraints (var "ts") $
            "newConstraints" <~ Maybes.map (var "renameConstraintKeys") (var "oldConstraints") $
            "b1"       <~ Core.binding
              (Core.bindingName $ var "b")
              (var "newVal")
              (just $ Core.typeScheme (var "newVars") (var "substType" @@ var "newSubst" @@ var "typ") (var "newConstraints")) $
            -- Note: do not advance 'next' for the next sibling; keep current 'next'
            var "step" @@ (Lists.cons (var "b1") (var "acc")) @@ var "tl") $
          optCases (Core.bindingTypeScheme $ var "b")
               -- Untyped binding: rewrite its term with current state; 'next' unchanged for siblings
               (var "noType")
               -- Typed binding: allocate |vars| fresh t{next+i}; bump 'next' only for the binding's TERM
               ("ts" ~> var "withType" @@ var "ts"))
          (Lists.uncons $ var "bs")) $
        "bindings1" <~ var "step" @@ (list ([] :: [TTerm Binding])) @@ (var "bindings0") $
        Core.termLet $ Core.let_
          (var "bindings1")
          -- Body sees the original 'next' (binding lambdas don't bind in the body)
          (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ var "body0"),
      -- Type application terms have a type which needs to be rewritten, and we also recurse into the body term.
      _Term_typeApplication>>: "tt" ~> Core.termTypeApplication $ Core.typeApplicationTerm
        (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.typeApplicationTermBody $ var "tt"))
        (var "substType" @@ var "subst" @@ (Core.typeApplicationTermType $ var "tt")),
      -- Type lambdas introduce a type variable which needs to be replaced, and we also recurse into the body term.
      -- Note: in Hydra currently, type lambdas are exclusively created during type inference in combination with
      -- polymorphic let bindings, so the type variable should already be present in the substitution.
      -- If "free-standing" type lambdas are ever supported in the future, we will have to create a fresh type variable here.
      _Term_typeLambda>>: "ta" ~> Core.termTypeLambda $ Core.typeLambda
        (var "replaceName" @@ var "subst" @@ (Core.typeLambdaParameter $ var "ta"))
        (var "rewriteWithSubst" @@ (pair (pair (var "subst") (var "boundVars")) (var "next")) @@ (Core.typeLambdaBody $ var "ta"))]) $
    Rewriting.rewriteTerm @@ var "rewrite" @@ var "term0") $
  -- initial state: ((emptySubst, emptyBound), next=0)
  var "rewriteWithSubst" @@ (pair (pair Maps.empty Sets.empty) (int32 0)) @@ var "term"

replaceFreeTermVariable :: TTermDefinition (Name -> Term -> Term -> Term)
replaceFreeTermVariable = define "replaceFreeTermVariable" $
  doc "Replace a free variable in a term" $
  "vold" ~> "tnew" ~> "term" ~>
  "rewrite" <~ ("recurse" ~> "t" ~> cases _Term (var "t")
    (Just $ var "recurse" @@ var "t") [
    _Term_lambda>>: "l" ~>
      "v" <~ Core.lambdaParameter (var "l") $
      Logic.ifElse (Equality.equal (var "v") (var "vold"))
        (var "t")
        (var "recurse" @@ var "t"),
    _Term_variable>>: "v" ~>
      Logic.ifElse (Equality.equal (var "v") (var "vold"))
        (var "tnew")
        (Core.termVariable $ var "v")]) $
  Rewriting.rewriteTerm @@ var "rewrite" @@ var "term"

replaceFreeTypeVariable :: TTermDefinition (Name -> Type -> Type -> Type)
replaceFreeTypeVariable = define "replaceFreeTypeVariable" $
  doc "Replace free occurrences of a name in a type" $
  "v" ~> "rep" ~> "typ" ~>
  "mapExpr" <~ ("recurse" ~> "t" ~> cases _Type (var "t")
    (Just $ var "recurse" @@ var "t") [
    _Type_forall>>: "ft" ~> Logic.ifElse
      (Equality.equal (var "v") (Core.forallTypeParameter $ var "ft"))
      (var "t")
      (Core.typeForall $ Core.forallType
        (Core.forallTypeParameter $ var "ft")
        (var "recurse" @@ (Core.forallTypeBody $ var "ft"))),
    _Type_variable>>: "v'" ~> Logic.ifElse
      (Equality.equal (var "v") (var "v'"))
      (var "rep")
      (var "t")]) $
  Rewriting.rewriteType @@ var "mapExpr" @@ var "typ"

substituteTypeVariables :: TTermDefinition (M.Map Name Name -> Type -> Type)
substituteTypeVariables = define "substituteTypeVariables" $
  doc "Substitute type variables in a type" $
  "subst" ~> "typ" ~>
  "replace" <~ ("recurse" ~> "typ" ~> cases _Type (var "typ")
    (Just $ var "recurse" @@ var "typ") [
    _Type_variable>>: "n" ~>
      Core.typeVariable $ Maybes.fromMaybe (var "n") $ Maps.lookup (var "n") (var "subst")]) $
  Rewriting.rewriteType @@ var "replace" @@ var "typ"

substituteVariable :: TTermDefinition (Name -> Name -> Term -> Term)
substituteVariable = define "substituteVariable" $
  doc "Substitute one variable for another in a term" $
  "from" ~> "to" ~> "term" ~>
  "replace" <~ ("recurse" ~> "term" ~>
    cases _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      _Term_variable>>: "x" ~>
        Core.termVariable $ Logic.ifElse (Equality.equal (var "x") (var "from")) (var "to") (var "x"),
      _Term_lambda>>: "l" ~> Logic.ifElse
        (Equality.equal (Core.lambdaParameter $ var "l") (var "from"))
        (var "term")
        (var "recurse" @@ var "term")]) $
  Rewriting.rewriteTerm @@ var "replace" @@ var "term"

substituteVariables :: TTermDefinition (M.Map Name Name -> Term -> Term)
substituteVariables = define "substituteVariables" $
  doc "Substitute multiple variables in a term" $
  "subst" ~> "term" ~>
  "replace" <~ ("recurse" ~> "term" ~>
    cases _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      _Term_variable>>: "n" ~>
        Core.termVariable $ Maybes.fromMaybe (var "n") $ Maps.lookup (var "n") (var "subst"),
      _Term_lambda>>: "l" ~>
        Maybes.maybe
          (var "recurse" @@ var "term")
          (constant $ var "term")
          (Maps.lookup (Core.lambdaParameter $ var "l") (var "subst"))]) $
  Rewriting.rewriteTerm @@ var "replace" @@ var "term"

unshadowVariables :: TTermDefinition (Term -> Term)
unshadowVariables = define "unshadowVariables" $
  doc ("Rename all shadowed variables (both lambda parameters and let-bound variables"
    <> " that shadow lambda parameters) in a term.") $
  "term0" ~>
  -- Find a fresh name not in the key set of the map, trying base2, base3, etc.
  "freshName" <~ ("base" ~> "i" ~> "m" ~>
    "candidate" <~ Core.name (Strings.cat2 (Core.unName $ var "base") (Literals.showInt32 $ var "i")) $
    Logic.ifElse (Maps.member (var "candidate") (var "m"))
      (var "freshName" @@ var "base" @@ Math.add (var "i") (int32 1) @@ var "m")
      (var "candidate")) $
  "f" <~ ("recurse" ~> "m" ~> "term" ~>
    cases _Term (var "term") (Just $ var "recurse" @@ var "m" @@ var "term") [
    _Term_lambda>>: "l" ~>
      "v" <~ Core.lambdaParameter (var "l") $
      "domain" <~ Core.lambdaDomain (var "l") $
      "body" <~ Core.lambdaBody (var "l") $
      Logic.ifElse (Maps.member (var "v") (var "m"))
        -- Shadowed: find a fresh name, add v -> fresh to map, recurse into body
        ("v2" <~ var "freshName" @@ var "v" @@ int32 2 @@ var "m" $
          "m2" <~ Maps.insert (var "v") (var "v2") (Maps.insert (var "v2") (var "v2") (var "m")) $
          Core.termLambda $ Core.lambda (var "v2") (var "domain")
            (var "f" @@ var "recurse" @@ var "m2" @@ var "body"))
        -- First occurrence: register v -> v (identity), recurse into body
        (Core.termLambda $ Core.lambda (var "v") (var "domain")
          (var "f" @@ var "recurse" @@ Maps.insert (var "v") (var "v") (var "m") @@ var "body")),
    _Term_let>>: "lt" ~>
      -- Register all let-bound names as in-scope (identity mapping) so inner lambdas know about them
      "m2" <~ Lists.foldl ("acc" ~> "b" ~>
        "bname" <~ Core.bindingName (var "b") $
        Logic.ifElse (Maps.member (var "bname") (var "acc"))
          (var "acc")
          (Maps.insert (var "bname") (var "bname") (var "acc")))
        (var "m") (Core.letBindings $ var "lt") $
      var "recurse" @@ var "m2" @@ var "term",
    _Term_variable>>: "v" ~> Core.termVariable $ optCases (Maps.lookup (var "v") (var "m"))
      (var "v")
      ("renamed" ~> var "renamed")]) $
  Rewriting.rewriteTermWithContext @@ var "f" @@ Maps.empty @@ var "term0"
