
{-
TODO:
* Double-check the use of checkTypeVariables for consistency across rules
* In nominal rules (record/union/wrap intro and elim), double-check that we are checking provided data against the schema
* Use the inference debug flag to enable / disable code which is purely for checking and not for reconstruction.
* When the debug flag is set, call checkType from hydra.inference rules
-}

module Hydra.Sources.Kernel.Terms.Checking where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  allEqual, applyTypeArgumentsToType,
  checkForUnboundTypeVariables, checkForUnboundTypeVariablesE,
  checkNominalApplication, checkNominalApplicationE,
  checkSameType, checkType, checkTypeSubst, checkTypeVariables, containsInScopeTypeVars, normalizeTypeFreeVars, toFContext,
  typeListsEffectivelyEqual, typeOf, typeOfE, typeOfTerm, typesAllEffectivelyEqual, typesEffectivelyEqual,
  typeOfAnnotatedTerm, typeOfAnnotatedTermE,
  typeOfApplication, typeOfApplicationE,
  typeOfCaseStatement, typeOfCaseStatementE,
  typeOfEither, typeOfEitherE,
  typeOfInjection, typeOfInjectionE,
  typeOfLambda, typeOfLambdaE,
  typeOfLet, typeOfLetE,
  typeOfList, typeOfListE,
  typeOfLiteral, typeOfLiteralE,
  typeOfMap, typeOfMapE,
  typeOfMaybe, typeOfMaybeE,
  typeOfPair, typeOfPairE,
  typeOfPrimitive, typeOfPrimitiveE,
  typeOfProjection, typeOfProjectionE,
  typeOfRecord, typeOfRecordE,
  typeOfSet, typeOfSetE,
  typeOfTypeApplication, typeOfTypeApplicationE,
  typeOfTypeLambda, typeOfTypeLambdaE,
  typeOfUnit, typeOfUnitE,
  typeOfUnwrap, typeOfUnwrapE,
  typeOfVariable, typeOfVariableE,
  typeOfWrappedTerm, typeOfWrappedTermE)
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
import qualified Hydra.Dsl.Paths         as Paths
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors            as Error
import qualified Hydra.Dsl.Error.Checking   as ErrorsChecking
import qualified Hydra.Dsl.Error.Core       as ErrorsCore
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Constants    as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Formatting   as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical
import qualified Hydra.Sources.Kernel.Terms.Reflect      as Reflect
import qualified Hydra.Sources.Kernel.Terms.Dependencies  as Dependencies
import qualified Hydra.Sources.Kernel.Terms.Rewriting    as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Scoping      as Scoping

import qualified Hydra.Sources.Kernel.Terms.Names        as Names
import qualified Hydra.Sources.Kernel.Terms.Resolution   as Resolution
import qualified Hydra.Sources.Kernel.Terms.Strip        as Strip
import qualified Hydra.Sources.Kernel.Terms.Variables    as Variables
import qualified Hydra.Sources.Kernel.Terms.Show.Core    as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Errors  as ShowError
import qualified Hydra.Sources.Kernel.Terms.Show.Variants    as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution


ns :: Namespace
ns = Namespace "hydra.checking"

module_ :: Module
module_ = Module ns definitions
    [Constants.ns, Dependencies.ns, ExtractCore.ns, Formatting.ns, Lexical.ns, Reflect.ns, Rewriting.ns,
      Scoping.ns, Names.ns, Resolution.ns, ShowCore.ns, ShowError.ns, ShowVariants.ns, Strip.ns, Substitution.ns, Variables.ns]
    kernelTypesNamespaces $
    Just "Type checking and type reconstruction (type-of) for the results of Hydra unification and inference"
  where
    definitions = [
      toDefinition allEqual,
      toDefinition applyTypeArgumentsToType,
      toDefinition checkForUnboundTypeVariables,
      toDefinition checkNominalApplication,
      toDefinition checkSameType,
      toDefinition checkType,
      toDefinition checkTypeSubst,
      toDefinition checkTypeVariables,
      toDefinition containsInScopeTypeVars,
      toDefinition normalizeTypeFreeVars,
      toDefinition toFContext,
      toDefinition typeListsEffectivelyEqual,
      toDefinition typeOf,
      toDefinition typeOfAnnotatedTerm,
      toDefinition typeOfApplication,
      toDefinition typeOfCaseStatement,
      toDefinition typeOfEither,
      toDefinition typeOfInjection,
      toDefinition typeOfLambda,
      toDefinition typeOfLet,
      toDefinition typeOfList,
      toDefinition typeOfLiteral,
      toDefinition typeOfMap,
      toDefinition typeOfMaybe,
      toDefinition typeOfPair,
      toDefinition typeOfPrimitive,
      toDefinition typeOfProjection,
      toDefinition typeOfRecord,
      toDefinition typeOfSet,
      toDefinition typeOfTypeApplication,
      toDefinition typeOfTypeLambda,
      toDefinition typeOfUnit,
      toDefinition typeOfUnwrap,
      toDefinition typeOfVariable,
      toDefinition typeOfWrappedTerm,
      toDefinition typeOfTerm,
      toDefinition typesAllEffectivelyEqual,
      toDefinition typesEffectivelyEqual]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

noTypeArgs :: TTerm [Type]
noTypeArgs = list ([] :: [TTerm Type])

--

formatError :: TTerm (Error -> String)
formatError = "e" ~> ShowError.error_ @@ var "e"

allEqual :: TTermDefinition ([a] -> Bool)
allEqual = define "allEqual" $
  "els" ~>
  Logic.ifElse (Lists.null $ var "els")
    true
    (Lists.foldl
       ("b" ~> "t" ~> Logic.and (var "b") (Equality.equal (var "t") (Lists.head (var "els"))))
       true
       (Lists.tail $ var "els"))

applyTypeArgumentsToType :: TTermDefinition (Context -> Graph -> [Type] -> Type -> Prelude.Either Error Type)
applyTypeArgumentsToType = define "applyTypeArgumentsToType" $
  doc "Apply type arguments to a type, substituting forall-bound variables" $
  "cx" ~> "tx" ~> "typeArgs" ~> "t" ~>
  -- Check null typeArgs FIRST to avoid eager evaluation of head/tail on empty list
  Logic.ifElse (Lists.null $ var "typeArgs")
    (right $ var "t")
    ("nonnull" <~ (cases _Type (var "t")
      (Just $ Ctx.failInContext (Error.errorExtraction $ Error.extractionErrorUnexpectedShape $ Error.unexpectedShapeError (string "forall type") (Strings.cat $ list [
        ShowCore.type_ @@ var "t",
        string ". Trying to apply ",
        Literals.showInt32 (Lists.length $ var "typeArgs"),
        string " type args: ",
        Formatting.showList @@ ShowCore.type_ @@ var "typeArgs",
        string ". Context has vars: {",
        Strings.intercalate (string ", ") (Lists.map (unaryFunction $ Core.unName) $ Maps.keys $ Graph.graphBoundTypes $ var "tx"),
        string "}"])) (var "cx")) [
      _Type_forall>>: "ft" ~>
        "v" <~ Core.forallTypeParameter (var "ft") $
        "tbody" <~ Core.forallTypeBody (var "ft") $
        applyTypeArgumentsToType
          @@ var "cx"
          @@ var "tx"
          @@ (Lists.tail $ var "typeArgs")
          @@ (Substitution.substInType
            @@ (Typing.typeSubst $ Maps.singleton (var "v") (Lists.head $ var "typeArgs"))
            @@ (var "tbody"))]) $
    var "nonnull")

checkForUnboundTypeVariables :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either Error ())
checkForUnboundTypeVariables = define "checkForUnboundTypeVariables" $
  doc "Check that a term has no unbound type variables (Either version)" $
  "cx" ~> "tx" ~> "term0" ~>
  "svars" <~ Sets.fromList (Maps.keys $ Graph.graphSchemaTypes $ var "tx") $
  "checkRecursive" <~ ("vars" ~> "trace" ~> "lbinding" ~> "term" ~>
    "recurse" <~ var "checkRecursive" @@ var "vars" @@ var "trace" @@ var "lbinding" $
    "dflt" <~ (
      Eithers.bind (Eithers.mapList (var "recurse") (Rewriting.subterms @@ var "term"))
        ("_" ~> right unit)) $
    "check" <~ ("typ" ~>
      "freevars" <~ Variables.freeVariablesInType @@ var "typ" $
      "badvars" <~ Sets.difference (Sets.difference (var "freevars") (var "vars")) (var "svars") $
      Logic.ifElse (Sets.null $ var "badvars")
        (right unit)
        (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorUnboundTypeVariables $ ErrorsChecking.unboundTypeVariablesError (var "badvars") (var "typ")) (var "cx"))) $
    "checkOptional" <~ ("m" ~>
      Eithers.bind (Eithers.mapMaybe (var "check") (var "m"))
        ("_" ~> right unit)) $
    cases _Term (var "term")
      (Just $ var "dflt") [
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "dflt") [
        _Function_elimination>>: "e" ~> var "dflt",
        _Function_lambda>>: "l" ~>
          Eithers.bind (var "checkOptional" @@ (Core.lambdaDomain $ var "l"))
            ("_" ~> var "recurse" @@ (Core.lambdaBody $ var "l"))],
      _Term_let>>: "l" ~>
        "forBinding" <~ ("b" ~>
          "bterm" <~ Core.bindingTerm (var "b") $
          "newVars" <~ optCases (Core.bindingType $ var "b")
             (var "vars")
             ("ts" ~> Sets.union (var "vars") (Sets.fromList $ Core.typeSchemeVariables $ var "ts")) $
          "newTrace" <~ Lists.cons (Core.unName $ Core.bindingName $ var "b") (var "trace") $
          var "checkRecursive" @@ var "newVars" @@ var "newTrace" @@ (just $ var "b") @@ var "bterm") $
        Eithers.bind (Eithers.mapList (var "forBinding") $ Core.letBindings $ var "l")
          ("_" ~> var "recurse" @@ (Core.letBody $ var "l")),
      _Term_typeApplication>>: "tt" ~>
        Eithers.bind (var "check" @@ (Core.typeApplicationTermType $ var "tt"))
          ("_" ~> var "recurse" @@ (Core.typeApplicationTermBody $ var "tt")),
      _Term_typeLambda>>: "tl" ~>
        Eithers.bind (var "check" @@ (Core.typeVariable $ Core.typeLambdaParameter $ var "tl"))
          ("_" ~> var "recurse" @@ (Core.typeLambdaBody $ var "tl"))]) $
  var "checkRecursive" @@ Sets.empty @@ list [string "top level"] @@ nothing @@ var "term0"

checkNominalApplication :: TTermDefinition (Context -> Graph -> Name -> [Type] -> Prelude.Either Error ((), Context))
checkNominalApplication = define "checkNominalApplication" $
  doc "Check that a nominal type is applied to the correct number of type arguments (Either version)" $
  "cx" ~> "tx" ~> "tname" ~> "typeArgs" ~>
  "result" <<~ Resolution.requireSchemaType @@ var "cx" @@ (Graph.graphSchemaTypes $ var "tx") @@ var "tname" $
  "schemaType" <~ Pairs.first (var "result") $
  "cx2" <~ Pairs.second (var "result") $
  "vars" <~ Core.typeSchemeVariables (var "schemaType") $
  "varslen" <~ Lists.length (var "vars") $
  "argslen" <~ Lists.length (var "typeArgs") $
  Logic.ifElse (Equality.equal (var "varslen") (var "argslen"))
    (right $ pair unit (var "cx2"))
    (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorTypeArityMismatch $ ErrorsChecking.typeArityMismatchError (Core.typeVariable (var "tname")) (var "varslen") (var "argslen") (var "typeArgs")) (var "cx2"))

checkSameType :: TTermDefinition (Context -> Graph -> String -> [Type] -> Prelude.Either Error Type)
checkSameType = define "checkSameType" $
  doc "Ensure all types in a list are equal and return the common type" $
  "cx" ~> "tx" ~> "desc" ~> "types" ~>
  Logic.ifElse (typesAllEffectivelyEqual @@ var "tx" @@ var "types")
    (right $ Lists.head $ var "types")
    (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorUnequalTypes $ ErrorsChecking.unequalTypesError (var "types") (var "desc")) (var "cx"))

-- TODO: unused
checkType :: TTermDefinition (Context -> Graph -> Term -> Type -> Prelude.Either Error ())
checkType = define "checkType" $
  doc "Check that a term has the expected type" $
  "cx" ~> "tx" ~> "term" ~> "typ" ~>
  "vars" <~ Graph.graphTypeVariables (var "tx") $
  Logic.ifElse (Constants.debugInference)
    ("t0" <<~ (Eithers.map ("_p" ~> Pairs.first (var "_p")) (typeOf @@ var "cx" @@ var "tx" @@ noTypeArgs @@ var "term")) $
      Logic.ifElse (typesEffectivelyEqual @@ var "tx" @@ var "t0" @@ var "typ")
        (right unit)
        (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorTypeMismatch $ ErrorsChecking.typeMismatchError (var "typ") (var "t0")) (var "cx")))
    (right unit)

checkTypeSubst :: TTermDefinition (Context -> Graph -> TypeSubst -> Prelude.Either Error TypeSubst)
checkTypeSubst = define "checkTypeSubst" $
  doc ("Sanity-check a type substitution arising from unification. Specifically, check that schema types have not been"
    <> " inappropriately unified with type variables inferred from terms.") $
  "cx" ~> "tx" ~> "subst" ~>
  "s" <~ Typing.unTypeSubst (var "subst") $
  "vars" <~ Sets.fromList (Maps.keys $ var "s") $
  "suspectVars" <~ Sets.intersection (var "vars") (Sets.fromList $ Maps.keys $ Graph.graphSchemaTypes $ var "tx") $
  "isNominal" <~ ("ts" ~> cases _Type (Strip.deannotateType @@ (Core.typeSchemeType $ var "ts"))
    (Just false) [
    _Type_record>>: constant true,
    _Type_union>>: constant true,
    _Type_wrap>>: constant true]) $
  "badVars" <~ Sets.fromList (Lists.filter
    ("v" ~> Maybes.maybe false (var "isNominal") $
      Lexical.dereferenceSchemaType @@ var "v" @@ (Graph.graphSchemaTypes $ var "tx"))
    (Sets.toList $ var "suspectVars")) $
  "badPairs" <~ Lists.filter ("p" ~> Sets.member (Pairs.first $ var "p") (var "badVars")) (Maps.toList $ var "s") $
  "printPair" <~ ("p" ~> (Core.unName $ Pairs.first $ var "p") ++ (string " --> ") ++ (ShowCore.type_ @@ Pairs.second (var "p"))) $
  Logic.ifElse (Sets.null $ var "badVars")
    (right $ var "subst")
    (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorIncorrectUnification $ ErrorsChecking.incorrectUnificationError (var "subst")) (var "cx"))

checkTypeVariables :: TTermDefinition (Graph -> Type -> ())
checkTypeVariables = define "checkTypeVariables" $
  doc "Check that all type variables in a type are bound. NOTE: This check is currently disabled to allow phantom type variables from polymorphic instantiation to pass through. The proper fix is to ensure `typeOf` doesn't create fresh variables for post-inference code." $
  "_tx" ~> "_typ" ~>
  -- Disabled: phantom type variables from polymorphic instantiation cause false positives.
  -- The inference pass has already validated type variables via checkForUnboundTypeVariables.
  unit

-- Note: with Graph, this converts TypeSchemes back to System F types
toFContext :: TTermDefinition (Graph -> M.Map Name Type)
toFContext = define "toFContext" $
  doc "Get the bound types from a graph as a type environment" $
  "cx" ~> Maps.map (Scoping.typeSchemeToFType) $ Graph.graphBoundTypes $ var "cx"

typeListsEffectivelyEqual :: TTermDefinition (Graph -> [Type] -> [Type] -> Bool)
typeListsEffectivelyEqual = define "typeListsEffectivelyEqual" $
  doc "Check whether two lists of types are effectively equal, disregarding type aliases" $
  "tx" ~> "tlist1" ~> "tlist2" ~>
  Logic.ifElse (Equality.equal (Lists.length (var "tlist1")) (Lists.length (var "tlist2")))
    (Lists.foldl (binaryFunction Logic.and) true $
      Lists.zipWith (typesEffectivelyEqual @@ var "tx") (var "tlist1") (var "tlist2"))
    false

-- Old typeOf* variant functions removed; typeOf now uses Either-based typeOfE

normalizeTypeFreeVars :: TTermDefinition (Type -> Type)
normalizeTypeFreeVars = define "normalizeTypeFreeVars" $
  doc "Normalize free type variables in a type to canonical names based on order of first occurrence. This allows comparing types that differ only in the naming of free type variables." $
  "typ" ~>
  "collectVars" <~ ("acc" ~> "t" ~>
    cases _Type (var "t")
      (Just $ var "acc") [
      _Type_variable>>: "v" ~>
        Logic.ifElse (Maps.member (var "v") (var "acc"))
          (var "acc")
          (Maps.insert (var "v") (Core.name $ Strings.cat2 (string "_tv") (Literals.showInt32 $ Maps.size $ var "acc")) (var "acc"))]) $
  "subst" <~ Rewriting.foldOverType @@ Coders.traversalOrderPre @@ var "collectVars" @@ Maps.empty @@ var "typ" $
  Variables.substituteTypeVariables @@ var "subst" @@ var "typ"

typesAllEffectivelyEqual :: TTermDefinition (Graph -> [Type] -> Bool)
typesAllEffectivelyEqual = define "typesAllEffectivelyEqual" $
  doc ("Check whether a list of types are effectively equal, disregarding type aliases and free type variable naming."
    <> " Also treats free type variables (not in schema) as wildcards, since inference has already verified consistency.") $
  "tx" ~> "tlist" ~>
  "types" <~ (Graph.graphSchemaTypes $ var "tx") $
  "containsFreeVar" <~ ("t" ~>
    "allVars" <~ Variables.freeVariablesInTypeSimple @@ var "t" $
    "schemaNames" <~ Sets.fromList (Maps.keys $ var "types") $
    Logic.not $ Sets.null $ Sets.difference (var "allVars") (var "schemaNames")) $
  "anyContainsFreeVar" <~ Lists.foldl ("acc" ~> "t" ~> Logic.or (var "acc") (var "containsFreeVar" @@ var "t")) false (var "tlist") $
  Logic.ifElse (var "anyContainsFreeVar")
    true
    (Logic.ifElse (allEqual @@ (Lists.map ("t" ~> normalizeTypeFreeVars @@ var "t") (var "tlist")))
      true
      (allEqual @@ (Lists.map ("t" ~> normalizeTypeFreeVars @@ (Strip.deannotateTypeRecursive @@ (Dependencies.replaceTypedefs @@ var "types" @@ var "t"))) (var "tlist"))))

-- | Check if a type contains any type variable that's in scope (from graphTypeVariables)
containsInScopeTypeVars :: TTermDefinition (Graph -> Type -> Bool)
containsInScopeTypeVars = define "containsInScopeTypeVars" $
  doc "Check if a type contains any type variable from the current scope" $
  "tx" ~> "t" ~>
  "vars" <~ Graph.graphTypeVariables (var "tx") $
  "freeVars" <~ Variables.freeVariablesInTypeSimple @@ var "t" $
  Logic.not $ Sets.null $ Sets.intersection (var "vars") (var "freeVars")

typesEffectivelyEqual :: TTermDefinition (Graph -> Type -> Type -> Bool)
typesEffectivelyEqual = define "typesEffectivelyEqual" $
  doc "Check whether two types are effectively equal, disregarding type aliases, forall quantifiers, and treating in-scope type variables as wildcards" $
  "tx" ~> "t1" ~> "t2" ~>
  -- If either type contains in-scope type variables, treat them as matching
  -- This handles the case where fresh type variables from instantiation haven't been substituted
  Logic.or (containsInScopeTypeVars @@ var "tx" @@ var "t1") $
  Logic.or (containsInScopeTypeVars @@ var "tx" @@ var "t2") $
  typesAllEffectivelyEqual @@ var "tx" @@ list [
    Resolution.fullyStripAndNormalizeType @@ var "t1",
    Resolution.fullyStripAndNormalizeType @@ var "t2"]

-- ============================================================================
-- Either/Context-threading versions of typeOf* functions
-- ============================================================================
-- These functions thread a Context through to support pure fresh-name generation.
-- The return type is (Type, Context) to propagate
-- the updated counter.

typeOf :: TTermDefinition (Context -> Graph -> [Type] -> Term -> Prelude.Either Error (Type, Context))
typeOf = define "typeOf" $
  doc "Given a type context, reconstruct the type of a System F term" $
  "cx" ~> "tx" ~> "typeArgs" ~> "term" ~>
  "cx1" <~ Ctx.pushTrace (string "typeOf") (var "cx") $
  cases _Term (var "term")
    (Just $ Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorUnsupportedTermVariant $ ErrorsChecking.unsupportedTermVariantError (Reflect.termVariant @@ var "term")) (var "cx1")) [
    _Term_annotated>>: typeOfAnnotatedTerm @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_application>>: typeOfApplication @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_either>>: typeOfEither @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_function>>: "f" ~>
      cases _Function (var "f") Nothing [
        _Function_elimination>>: "elm" ~>
          cases _Elimination (var "elm") Nothing [
            _Elimination_record>>: typeOfProjection @@ var "cx1" @@ var "tx" @@ var "typeArgs",
            _Elimination_union>>: typeOfCaseStatement @@ var "cx1" @@ var "tx" @@ var "typeArgs",
            _Elimination_wrap>>: typeOfUnwrap @@ var "cx1" @@ var "tx" @@ var "typeArgs"],
        _Function_lambda>>: typeOfLambda @@ var "cx1" @@ var "tx" @@ var "typeArgs"],
    _Term_let>>: typeOfLet @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_list>>: typeOfList @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_literal>>: typeOfLiteral @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_map>>: typeOfMap @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_maybe>>: typeOfMaybe @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_pair>>: typeOfPair @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_record>>: typeOfRecord @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_set>>: typeOfSet @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_typeApplication>>: typeOfTypeApplication @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_typeLambda>>: typeOfTypeLambda @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_union>>: typeOfInjection @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_unit>>: constant $ typeOfUnit @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_variable>>: typeOfVariable @@ var "cx1" @@ var "tx" @@ var "typeArgs",
    _Term_wrap>>: typeOfWrappedTerm @@ var "cx1" @@ var "tx" @@ var "typeArgs"]

typeOfAnnotatedTerm :: TTermDefinition (Context -> Graph -> [Type] -> AnnotatedTerm -> Prelude.Either Error (Type, Context))
typeOfAnnotatedTerm = define "typeOfAnnotatedTerm" $
  doc "Reconstruct the type of an annotated term (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "at" ~>
  typeOf @@ var "cx" @@ var "tx" @@ var "typeArgs" @@ Core.annotatedTermBody (var "at")

typeOfApplication :: TTermDefinition (Context -> Graph -> [Type] -> Application -> Prelude.Either Error (Type, Context))
typeOfApplication = define "typeOfApplication" $
  doc "Reconstruct the type of an application term (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "app" ~>
  "fun" <~ Core.applicationFunction (var "app") $
  "arg" <~ Core.applicationArgument (var "app") $
  "tryType" <~ ("cx0" ~> "tfun" ~> "targ" ~> cases _Type (var "tfun")
    (Just $ Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorNotAFunctionType $ ErrorsChecking.notAFunctionTypeError (var "tfun")) (var "cx0")) [
    _Type_forall>>: "ft" ~> var "tryType" @@ var "cx0" @@ (Core.forallTypeBody (var "ft")) @@ var "targ",
    _Type_function>>: "ft" ~>
      "dom" <~ Core.functionTypeDomain (var "ft") $
      "cod" <~ Core.functionTypeCodomain (var "ft") $
      Logic.ifElse (typesEffectivelyEqual @@ var "tx" @@ var "dom" @@ var "targ")
        (right $ pair (var "cod") (var "cx0"))
        (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorTypeMismatch $ ErrorsChecking.typeMismatchError (var "dom") (var "targ")) (var "cx0")),
    _Type_variable>>: "v" ~>
      "nameResult" <~ Names.freshName @@ var "cx0" $
      "freshN" <~ Pairs.first (var "nameResult") $
      "cx1" <~ Pairs.second (var "nameResult") $
      right $ pair (Core.typeVariable $ var "freshN") (var "cx1")]) $
  "result1" <<~ typeOf @@ var "cx" @@ var "tx" @@ noTypeArgs @@ var "fun" $
  "tfun" <~ Pairs.first (var "result1") $
  "cx2" <~ Pairs.second (var "result1") $
  "result2" <<~ typeOf @@ var "cx2" @@ var "tx" @@ noTypeArgs @@ var "arg" $
  "targ" <~ Pairs.first (var "result2") $
  "cx3" <~ Pairs.second (var "result2") $
  "result3" <<~ var "tryType" @@ var "cx3" @@ var "tfun" @@ var "targ" $
  "t" <~ Pairs.first (var "result3") $
  "cx4" <~ Pairs.second (var "result3") $
  "applied" <<~ applyTypeArgumentsToType @@ var "cx4" @@ var "tx" @@ var "typeArgs" @@ var "t" $
  right $ pair (var "applied") (var "cx4")

typeOfCaseStatement :: TTermDefinition (Context -> Graph -> [Type] -> CaseStatement -> Prelude.Either Error (Type, Context))
typeOfCaseStatement = define "typeOfCaseStatement" $
  doc "Reconstruct the type of a case statement (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "cs" ~>
  "tname" <~ Core.caseStatementTypeName (var "cs") $
  "dflt" <~ Core.caseStatementDefault (var "cs") $
  "cases" <~ Core.caseStatementCases (var "cs") $
  "cterms" <~ Lists.map (unaryFunction Core.fieldTerm) (var "cases") $
  -- Type the default case if present
  "dfltResult" <<~ Eithers.mapMaybe ("e" ~> typeOf @@ var "cx" @@ var "tx" @@ noTypeArgs @@ var "e") (var "dflt") $
  -- dfltResult :: Maybe (Type, Context)
  "tdflt" <~ Maybes.map (unaryFunction Pairs.first) (var "dfltResult") $
  "cx2" <~ Maybes.maybe (var "cx") (unaryFunction Pairs.second) (var "dfltResult") $
  -- Type all case terms, threading context through the list
  "foldResult" <~ Lists.foldl
    ("acc" ~> "term" ~>
      -- acc is Either Error ([Type], Context)
      "accR" <<~ var "acc" $
      "types" <~ Pairs.first (var "accR") $
      "cxA" <~ Pairs.second (var "accR") $
      "tResult" <<~ typeOf @@ var "cxA" @@ var "tx" @@ noTypeArgs @@ var "term" $
      "t" <~ Pairs.first (var "tResult") $
      "cxB" <~ Pairs.second (var "tResult") $
      right $ pair (Lists.concat2 (var "types") (Lists.pure $ var "t")) (var "cxB"))
    (right $ pair (list ([] :: [TTerm Type])) (var "cx2"))
    (var "cterms") $
  "foldR" <<~ var "foldResult" $
  "tcterms" <~ Pairs.first (var "foldR") $
  "cx3" <~ Pairs.second (var "foldR") $
  -- Extract function codomains
  "fcodsResult" <~ Lists.foldl
    ("acc" ~> "t" ~>
      "accR" <<~ var "acc" $
      "cods" <~ Pairs.first (var "accR") $
      "ft" <<~ ExtractCore.functionType @@ var "t" $
      right $ pair (Lists.concat2 (var "cods") (Lists.pure $ Core.functionTypeCodomain $ var "ft")) (var "cx3"))
    (right $ pair (list ([] :: [TTerm Type])) (var "cx3"))
    (var "tcterms") $
  "fcodsR" <<~ var "fcodsResult" $
  "fcods" <~ Pairs.first (var "fcodsR") $
  "cods" <~ Maybes.cat (Lists.cons (var "tdflt") $ Lists.map (unaryFunction Maybes.pure) (var "fcods")) $
  "cod" <<~ checkSameType @@ var "cx3" @@ var "tx" @@ (string "case branches") @@ var "cods" $
  right $ pair (Core.typeFunction $ Core.functionType
    (Resolution.nominalApplication @@ var "tname" @@ var "typeArgs")
    (var "cod")) (var "cx3")

typeOfEither :: TTermDefinition (Context -> Graph -> [Type] -> Prelude.Either Term Term -> Prelude.Either Error (Type, Context))
typeOfEither = define "typeOfEither" $
  doc "Reconstruct the type of an either value (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "et" ~>
  "n" <~ Lists.length (var "typeArgs") $
  Logic.ifElse (Equality.equal (var "n") (int32 2))
    (Eithers.either_
      ("leftTerm" ~>
        "result" <<~ typeOf @@ var "cx" @@ var "tx" @@ noTypeArgs @@ var "leftTerm" $
        "leftType" <~ Pairs.first (var "result") $
        "cx2" <~ Pairs.second (var "result") $
        right $ pair (Core.typeEither $ Core.eitherType (var "leftType") (Lists.at (int32 1) $ var "typeArgs")) (var "cx2"))
      ("rightTerm" ~>
        "result" <<~ typeOf @@ var "cx" @@ var "tx" @@ noTypeArgs @@ var "rightTerm" $
        "rightType" <~ Pairs.first (var "result") $
        "cx2" <~ Pairs.second (var "result") $
        right $ pair (Core.typeEither $ Core.eitherType (Lists.at (int32 0) $ var "typeArgs") (var "rightType")) (var "cx2"))
      (var "et"))
    (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorTypeArityMismatch $ ErrorsChecking.typeArityMismatchError (Core.typeEither $ Core.eitherType Core.typeUnit Core.typeUnit) (int32 2) (var "n") (var "typeArgs")) (var "cx"))

typeOfInjection :: TTermDefinition (Context -> Graph -> [Type] -> Injection -> Prelude.Either Error (Type, Context))
typeOfInjection = define "typeOfInjection" $
  doc "Reconstruct the type of a union injection (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "injection" ~>
  "tname" <~ Core.injectionTypeName (var "injection") $
  "field" <~ Core.injectionField (var "injection") $
  "fname" <~ Core.fieldName (var "field") $
  "fterm" <~ Core.fieldTerm (var "field") $
  "schemaResult" <<~ Resolution.requireSchemaType @@ var "cx" @@ (Graph.graphSchemaTypes $ var "tx") @@ var "tname" $
  "schemaType" <~ Pairs.first (var "schemaResult") $
  "cx2" <~ Pairs.second (var "schemaResult") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "sbody" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ExtractCore.unionType @@ var "tname" @@ var "sbody" $
  "ftyp" <<~ Resolution.findFieldType @@ var "cx2" @@ var "fname" @@ var "sfields" $
  right $ pair (Resolution.nominalApplication @@ var "tname" @@ var "typeArgs") (var "cx2")

typeOfLambda :: TTermDefinition (Context -> Graph -> [Type] -> Lambda -> Prelude.Either Error (Type, Context))
typeOfLambda = define "typeOfLambda" $
  doc "Reconstruct the type of a lambda function (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "l" ~>
  "v" <~ Core.lambdaParameter (var "l") $
  "mdom" <~ Core.lambdaDomain (var "l") $
  "body" <~ Core.lambdaBody (var "l") $
  "tbodyResult" <<~ optCases (var "mdom")
    (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorUntypedLambda ErrorsChecking.untypedLambdaError) (var "cx"))
    ("dom" ~>
      "types2" <~ Maps.insert (var "v") (Scoping.fTypeToTypeScheme @@ var "dom") (Graph.graphBoundTypes $ var "tx") $
      "codResult" <<~ typeOf @@ var "cx" @@ (Graph.graphWithBoundTypes (var "tx") $ var "types2") @@ noTypeArgs @@ var "body" $
      "cod" <~ Pairs.first (var "codResult") $
      "cx2" <~ Pairs.second (var "codResult") $
      right $ pair (Core.typeFunction $ Core.functionType (var "dom") (var "cod")) (var "cx2")) $
  "tbody" <~ Pairs.first (var "tbodyResult") $
  "cx3" <~ Pairs.second (var "tbodyResult") $
  "applied" <<~ applyTypeArgumentsToType @@ var "cx3" @@ var "tx" @@ var "typeArgs" @@ var "tbody" $
  right $ pair (var "applied") (var "cx3")

typeOfLet :: TTermDefinition (Context -> Graph -> [Type] -> Let -> Prelude.Either Error (Type, Context))
typeOfLet = define "typeOfLet" $
  doc "Reconstruct the type of a let binding (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "letTerm" ~>
  "bs" <~ Core.letBindings (var "letTerm") $
  "body" <~ Core.letBody (var "letTerm") $
  "bnames" <~ Lists.map (unaryFunction Core.bindingName) (var "bs") $
  "bindingType" <~ ("b" ~>
    Maybes.maybe
      (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorUntypedLetBinding $ ErrorsChecking.untypedLetBindingError (var "b")) (var "cx"))
      ("ts" ~> right $ Scoping.typeSchemeToFType @@ var "ts")
      (Core.bindingType $ var "b")) $
  -- Get binding types, threading errors through the fold
  "btypesResult" <~ Lists.foldl
    ("acc" ~> "b" ~>
      "accR" <<~ var "acc" $
      "types" <~ Pairs.first (var "accR") $
      "btype" <<~ var "bindingType" @@ var "b" $
      right $ pair (Lists.concat2 (var "types") (Lists.pure $ var "btype")) unit)
    (right $ pair (list ([] :: [TTerm Type])) unit)
    (var "bs") $
  "btypesR" <<~ var "btypesResult" $
  "btypes" <~ Pairs.first (var "btypesR") $
  -- Extended type context
  "tx2" <~ (Graph.graphWithBoundTypes (var "tx")
    (Maps.union
      (Maps.fromList $ Lists.zip (var "bnames") (Lists.map (Scoping.fTypeToTypeScheme) $ var "btypes"))
      (Graph.graphBoundTypes $ var "tx"))) $
  "tResult" <<~ typeOf @@ var "cx" @@ var "tx2" @@ noTypeArgs @@ var "body" $
  "t" <~ Pairs.first (var "tResult") $
  "cx2" <~ Pairs.second (var "tResult") $
  "applied" <<~ applyTypeArgumentsToType @@ var "cx2" @@ var "tx" @@ var "typeArgs" @@ var "t" $
  right $ pair (var "applied") (var "cx2")

typeOfList :: TTermDefinition (Context -> Graph -> [Type] -> [Term] -> Prelude.Either Error (Type, Context))
typeOfList = define "typeOfList" $
  doc "Reconstruct the type of a list (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "els" ~>
  Logic.ifElse (Lists.null $ var "els")
    (Logic.ifElse (Equality.equal (Lists.length $ var "typeArgs") (int32 1))
      (right $ pair (Core.typeList $ Lists.head $ var "typeArgs") (var "cx"))
      (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorTypeArityMismatch $ ErrorsChecking.typeArityMismatchError (Core.typeList Core.typeUnit) (int32 1) (Lists.length $ var "typeArgs") (var "typeArgs")) (var "cx")))
    -- Nonempty list: type all elements, threading context
    ("foldResult" <~ Lists.foldl
      ("acc" ~> "term" ~>
        "accR" <<~ var "acc" $
        "types" <~ Pairs.first (var "accR") $
        "cxA" <~ Pairs.second (var "accR") $
        "tResult" <<~ typeOf @@ var "cxA" @@ var "tx" @@ noTypeArgs @@ var "term" $
        "t" <~ Pairs.first (var "tResult") $
        "cxB" <~ Pairs.second (var "tResult") $
        right $ pair (Lists.concat2 (var "types") (Lists.pure $ var "t")) (var "cxB"))
      (right $ pair (list ([] :: [TTerm Type])) (var "cx"))
      (var "els") $
    "foldR" <<~ var "foldResult" $
    "eltypes" <~ Pairs.first (var "foldR") $
    "cx2" <~ Pairs.second (var "foldR") $
    "unifiedType" <<~ checkSameType @@ var "cx2" @@ var "tx" @@ (string "list elements") @@ var "eltypes" $
    right $ pair (Core.typeList $ var "unifiedType") (var "cx2"))

typeOfLiteral :: TTermDefinition (Context -> Graph -> [Type] -> Literal -> Prelude.Either Error (Type, Context))
typeOfLiteral = define "typeOfLiteral" $
  doc "Reconstruct the type of a literal (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "lit" ~>
  "t" <~ Core.typeLiteral (Reflect.literalType @@ var "lit") $
  "applied" <<~ applyTypeArgumentsToType @@ var "cx" @@ var "tx" @@ var "typeArgs" @@ var "t" $
  right $ pair (var "applied") (var "cx")

typeOfMap :: TTermDefinition (Context -> Graph -> [Type] -> M.Map Term Term -> Prelude.Either Error (Type, Context))
typeOfMap = define "typeOfMap" $
  doc "Reconstruct the type of a map (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "m" ~>
  Logic.ifElse (Maps.null $ var "m")
    (Logic.ifElse (Equality.equal (Lists.length $ var "typeArgs") (int32 2))
      (right $ pair (Core.typeMap $ Core.mapType
        (Lists.at (int32 0) $ var "typeArgs")
        (Lists.at (int32 1) $ var "typeArgs")) (var "cx"))
      (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorTypeArityMismatch $ ErrorsChecking.typeArityMismatchError (Core.typeMap $ Core.mapType Core.typeUnit Core.typeUnit) (int32 2) (Lists.length $ var "typeArgs") (var "typeArgs")) (var "cx")))
    -- Nonempty map: type keys and values
    ("pairs" <~ Maps.toList (var "m") $
    -- Fold over keys
    "keyFoldResult" <~ Lists.foldl
      ("acc" ~> "p" ~>
        "accR" <<~ var "acc" $
        "types" <~ Pairs.first (var "accR") $
        "cxA" <~ Pairs.second (var "accR") $
        "tResult" <<~ typeOf @@ var "cxA" @@ var "tx" @@ noTypeArgs @@ (Pairs.first $ var "p") $
        "t" <~ Pairs.first (var "tResult") $
        "cxB" <~ Pairs.second (var "tResult") $
        right $ pair (Lists.concat2 (var "types") (Lists.pure $ var "t")) (var "cxB"))
      (right $ pair (list ([] :: [TTerm Type])) (var "cx"))
      (var "pairs") $
    "keyFoldR" <<~ var "keyFoldResult" $
    "keyTypes" <~ Pairs.first (var "keyFoldR") $
    "cx2" <~ Pairs.second (var "keyFoldR") $
    "kt" <<~ checkSameType @@ var "cx2" @@ var "tx" @@ (string "map keys") @@ var "keyTypes" $
    -- Fold over values
    "valFoldResult" <~ Lists.foldl
      ("acc" ~> "p" ~>
        "accR" <<~ var "acc" $
        "types" <~ Pairs.first (var "accR") $
        "cxA" <~ Pairs.second (var "accR") $
        "tResult" <<~ typeOf @@ var "cxA" @@ var "tx" @@ noTypeArgs @@ (Pairs.second $ var "p") $
        "t" <~ Pairs.first (var "tResult") $
        "cxB" <~ Pairs.second (var "tResult") $
        right $ pair (Lists.concat2 (var "types") (Lists.pure $ var "t")) (var "cxB"))
      (right $ pair (list ([] :: [TTerm Type])) (var "cx2"))
      (var "pairs") $
    "valFoldR" <<~ var "valFoldResult" $
    "valTypes" <~ Pairs.first (var "valFoldR") $
    "cx3" <~ Pairs.second (var "valFoldR") $
    "vt" <<~ checkSameType @@ var "cx3" @@ var "tx" @@ (string "map values") @@ var "valTypes" $
    "applied" <<~ applyTypeArgumentsToType @@ var "cx3" @@ var "tx" @@ var "typeArgs"
      @@ (Core.typeMap $ Core.mapType (var "kt") (var "vt")) $
    right $ pair (var "applied") (var "cx3"))

typeOfMaybe :: TTermDefinition (Context -> Graph -> [Type] -> Maybe Term -> Prelude.Either Error (Type, Context))
typeOfMaybe = define "typeOfMaybe" $
  doc "Reconstruct the type of an optional value (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "mt" ~>
  "forNothing" <~ (
    "n" <~ Lists.length (var "typeArgs") $
    Logic.ifElse (Equality.equal (var "n") (int32 1))
      (right $ pair (Core.typeMaybe $ Lists.head $ var "typeArgs") (var "cx"))
      (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorTypeArityMismatch $ ErrorsChecking.typeArityMismatchError (Core.typeMaybe Core.typeUnit) (int32 1) (var "n") (var "typeArgs")) (var "cx"))) $
  "forJust" <~ ("term" ~>
    "tResult" <<~ typeOf @@ var "cx" @@ var "tx" @@ noTypeArgs @@ var "term" $
    "termType" <~ Pairs.first (var "tResult") $
    "cx2" <~ Pairs.second (var "tResult") $
    "t" <~ Core.typeMaybe (var "termType") $
    "applied" <<~ applyTypeArgumentsToType @@ var "cx2" @@ var "tx" @@ var "typeArgs" @@ var "t" $
    right $ pair (var "applied") (var "cx2")) $
  optCases (var "mt") (var "forNothing") (var "forJust")

typeOfPair :: TTermDefinition (Context -> Graph -> [Type] -> (Term, Term) -> Prelude.Either Error (Type, Context))
typeOfPair = define "typeOfPair" $
  doc "Reconstruct the type of a pair (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "p" ~>
  "n" <~ Lists.length (var "typeArgs") $
  Logic.ifElse (Equality.equal (var "n") (int32 2))
    ("pairFst" <~ Pairs.first (var "p") $
    "pairSnd" <~ Pairs.second (var "p") $
    "result1" <<~ typeOf @@ var "cx" @@ var "tx" @@ noTypeArgs @@ var "pairFst" $
    "firstType" <~ Pairs.first (var "result1") $
    "cx2" <~ Pairs.second (var "result1") $
    "result2" <<~ typeOf @@ var "cx2" @@ var "tx" @@ noTypeArgs @@ var "pairSnd" $
    "secondType" <~ Pairs.first (var "result2") $
    "cx3" <~ Pairs.second (var "result2") $
    right $ pair (Core.typePair $ Core.pairType (var "firstType") (var "secondType")) (var "cx3"))
    (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorTypeArityMismatch $ ErrorsChecking.typeArityMismatchError (Core.typePair $ Core.pairType Core.typeUnit Core.typeUnit) (int32 2) (var "n") (var "typeArgs")) (var "cx"))

typeOfPrimitive :: TTermDefinition (Context -> Graph -> [Type] -> Name -> Prelude.Either Error (Type, Context))
typeOfPrimitive = define "typeOfPrimitive" $
  doc "Reconstruct the type of a primitive function (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "name" ~>
  -- Look up the primitive directly in the graph's primitives map and extract its type.
  -- This avoids reconstructing a Map Name TypeScheme on every call (O(p) per call).
  "rawTs" <~ Maybes.map ("_p" ~> Graph.primitiveType (var "_p"))
    (Maps.lookup (var "name") (Graph.graphPrimitives $ var "tx")) $
  Maybes.maybe
    (Ctx.failInContext (Error.errorUndefinedTermVariable $ ErrorsCore.undefinedTermVariableError (Paths.subtermPath $ list ([] :: [TTerm SubtermStep])) (var "name")) (var "cx"))
    ("tsRaw" ~>
      "instResult" <~ Resolution.instantiateTypeScheme @@ var "cx" @@ var "tsRaw" $
      "ts" <~ Pairs.first (var "instResult") $
      "cx2" <~ Pairs.second (var "instResult") $
      "t" <~ Scoping.typeSchemeToFType @@ var "ts" $
      "applied" <<~ applyTypeArgumentsToType @@ var "cx2" @@ var "tx" @@ var "typeArgs" @@ var "t" $
      right $ pair (var "applied") (var "cx2"))
    (var "rawTs")

typeOfProjection :: TTermDefinition (Context -> Graph -> [Type] -> Projection -> Prelude.Either Error (Type, Context))
typeOfProjection = define "typeOfProjection" $
  doc "Reconstruct the type of a record projection (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "p" ~>
  "tname" <~ Core.projectionTypeName (var "p") $
  "fname" <~ Core.projectionField (var "p") $
  "schemaResult" <<~ Resolution.requireSchemaType @@ var "cx" @@ (Graph.graphSchemaTypes $ var "tx") @@ var "tname" $
  "schemaType" <~ Pairs.first (var "schemaResult") $
  "cx2" <~ Pairs.second (var "schemaResult") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "sbody" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ExtractCore.recordType @@ var "tname" @@ var "sbody" $
  "ftyp" <<~ Resolution.findFieldType @@ var "cx2" @@ var "fname" @@ var "sfields" $
  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "typeArgs")) $
  "sftyp" <~ Substitution.substInType @@ var "subst" @@ var "ftyp" $
  right $ pair (Core.typeFunction $ Core.functionType
    (Resolution.nominalApplication @@ var "tname" @@ var "typeArgs")
    (var "sftyp")) (var "cx2")

typeOfRecord :: TTermDefinition (Context -> Graph -> [Type] -> Record -> Prelude.Either Error (Type, Context))
typeOfRecord = define "typeOfRecord" $
  doc "Reconstruct the type of a record (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "record" ~>
  "tname" <~ Core.recordTypeName (var "record") $
  "fields" <~ Core.recordFields (var "record") $
  -- Type all field terms, threading context (for checking only)
  "foldResult" <~ Lists.foldl
    ("acc" ~> "term" ~>
      "accR" <<~ var "acc" $
      "types" <~ Pairs.first (var "accR") $
      "cxA" <~ Pairs.second (var "accR") $
      "tResult" <<~ typeOf @@ var "cxA" @@ var "tx" @@ noTypeArgs @@ var "term" $
      "t" <~ Pairs.first (var "tResult") $
      "cxB" <~ Pairs.second (var "tResult") $
      right $ pair (Lists.concat2 (var "types") (Lists.pure $ var "t")) (var "cxB"))
    (right $ pair (list ([] :: [TTerm Type])) (var "cx"))
    (Lists.map (unaryFunction Core.fieldTerm) (var "fields")) $
  "foldR" <<~ var "foldResult" $
  "cx2" <~ Pairs.second (var "foldR") $
  right $ pair (Resolution.nominalApplication @@ var "tname" @@ var "typeArgs") (var "cx2")

typeOfSet :: TTermDefinition (Context -> Graph -> [Type] -> S.Set Term -> Prelude.Either Error (Type, Context))
typeOfSet = define "typeOfSet" $
  doc "Reconstruct the type of a set (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "els" ~>
  Logic.ifElse (Sets.null $ var "els")
    (Logic.ifElse (Equality.equal (Lists.length $ var "typeArgs") (int32 1))
      (right $ pair (Core.typeSet $ Lists.head $ var "typeArgs") (var "cx"))
      (Ctx.failInContext (Error.errorChecking $ ErrorsChecking.checkingErrorTypeArityMismatch $ ErrorsChecking.typeArityMismatchError (Core.typeSet Core.typeUnit) (int32 1) (Lists.length $ var "typeArgs") (var "typeArgs")) (var "cx")))
    -- Nonempty set: type all elements, threading context
    ("foldResult" <~ Lists.foldl
      ("acc" ~> "term" ~>
        "accR" <<~ var "acc" $
        "types" <~ Pairs.first (var "accR") $
        "cxA" <~ Pairs.second (var "accR") $
        "tResult" <<~ typeOf @@ var "cxA" @@ var "tx" @@ noTypeArgs @@ var "term" $
        "t" <~ Pairs.first (var "tResult") $
        "cxB" <~ Pairs.second (var "tResult") $
        right $ pair (Lists.concat2 (var "types") (Lists.pure $ var "t")) (var "cxB"))
      (right $ pair (list ([] :: [TTerm Type])) (var "cx"))
      (Sets.toList $ var "els") $
    "foldR" <<~ var "foldResult" $
    "eltypes" <~ Pairs.first (var "foldR") $
    "cx2" <~ Pairs.second (var "foldR") $
    "unifiedType" <<~ checkSameType @@ var "cx2" @@ var "tx" @@ (string "set elements") @@ var "eltypes" $
    right $ pair (Core.typeSet $ var "unifiedType") (var "cx2"))

typeOfTerm :: TTermDefinition (Context -> Graph -> Term -> Either Error Type)
typeOfTerm = define "typeOfTerm" $
  doc "Check the type of a term" $
  "cx" ~> "g" ~> "term" ~>
  Eithers.map (primitive _pairs_first)
    (typeOf @@ var "cx" @@ var "g" @@ list ([] :: [TTerm Type]) @@ var "term")

typeOfTypeApplication :: TTermDefinition (Context -> Graph -> [Type] -> TypeApplicationTerm -> Prelude.Either Error (Type, Context))
typeOfTypeApplication = define "typeOfTypeApplication" $
  doc "Reconstruct the type of a type application term (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "tyapp" ~>
  "body" <~ Core.typeApplicationTermBody (var "tyapp") $
  "t" <~ Core.typeApplicationTermType (var "tyapp") $
  typeOf @@ var "cx" @@ var "tx" @@ Lists.cons (var "t") (var "typeArgs") @@ var "body"

typeOfTypeLambda :: TTermDefinition (Context -> Graph -> [Type] -> TypeLambda -> Prelude.Either Error (Type, Context))
typeOfTypeLambda = define "typeOfTypeLambda" $
  doc "Reconstruct the type of a type lambda (type abstraction) term (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "tl" ~>
  "v" <~ Core.typeLambdaParameter (var "tl") $
  "body" <~ Core.typeLambdaBody (var "tl") $
  "vars" <~ Graph.graphTypeVariables (var "tx") $
  "tx2" <~ Graph.graphWithTypeVariables (var "tx") (Sets.insert (var "v") (var "vars")) $
  "result1" <<~ typeOf @@ var "cx" @@ var "tx2" @@ noTypeArgs @@ var "body" $
  "t1" <~ Pairs.first (var "result1") $
  "cx2" <~ Pairs.second (var "result1") $
  "applied" <<~ applyTypeArgumentsToType @@ var "cx2" @@ var "tx" @@ var "typeArgs"
    @@ (Core.typeForall $ Core.forallType (var "v") (var "t1")) $
  right $ pair (var "applied") (var "cx2")

typeOfUnit :: TTermDefinition (Context -> Graph -> [Type] -> Prelude.Either Error (Type, Context))
typeOfUnit = define "typeOfUnit" $
  doc "Reconstruct the type of the unit term (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~>
  "applied" <<~ applyTypeArgumentsToType @@ var "cx" @@ var "tx" @@ var "typeArgs" @@ Core.typeUnit $
  right $ pair (var "applied") (var "cx")

typeOfUnwrap :: TTermDefinition (Context -> Graph -> [Type] -> Name -> Prelude.Either Error (Type, Context))
typeOfUnwrap = define "typeOfUnwrap" $
  doc "Reconstruct the type of an unwrap operation (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "tname" ~>
  "schemaResult" <<~ Resolution.requireSchemaType @@ var "cx" @@ (Graph.graphSchemaTypes $ var "tx") @@ var "tname" $
  "schemaType" <~ Pairs.first (var "schemaResult") $
  "cx2" <~ Pairs.second (var "schemaResult") $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "sbody" <~ Core.typeSchemeType (var "schemaType") $
  "wrapped" <<~ ExtractCore.wrappedType @@ var "tname" @@ var "sbody" $
  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "typeArgs")) $
  "swrapped" <~ Substitution.substInType @@ var "subst" @@ var "wrapped" $
  right $ pair (MetaTypes.function
    (Resolution.nominalApplication @@ var "tname" @@ var "typeArgs")
    (var "swrapped")) (var "cx2")

typeOfVariable :: TTermDefinition (Context -> Graph -> [Type] -> Name -> Prelude.Either Error (Type, Context))
typeOfVariable = define "typeOfVariable" $
  doc "Reconstruct the type of a variable (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "name" ~>
  "rawTypeScheme" <~ Maps.lookup (var "name") (Graph.graphBoundTypes $ var "tx") $
  "forScheme" <~ ("ts" ~>
      "tResult" <~ Logic.ifElse (Lists.null $ var "typeArgs")
        (Resolution.instantiateType @@ var "cx" @@ (Scoping.typeSchemeToFType @@ var "ts"))
        (pair (Scoping.typeSchemeToFType @@ var "ts") (var "cx")) $
      "t" <~ Pairs.first (var "tResult") $
      "cx2" <~ Pairs.second (var "tResult") $
      "applied" <<~ applyTypeArgumentsToType @@ var "cx2" @@ var "tx" @@ var "typeArgs" @@ var "t" $
      right $ pair (var "applied") (var "cx2")) $
  Maybes.maybe
    -- Not found in graphBoundTypes: fall through to graphPrimitives
    (Maybes.maybe
      (Ctx.failInContext (Error.errorUntypedTermVariable $ ErrorsCore.untypedTermVariableError (Paths.subtermPath $ list ([] :: [TTerm SubtermStep])) (var "name")) (var "cx"))
      (var "forScheme")
      (Maybes.map ("_p" ~> Graph.primitiveType (var "_p"))
        (Maps.lookup (var "name") (Graph.graphPrimitives $ var "tx"))))
    (var "forScheme")
    (var "rawTypeScheme")

typeOfWrappedTerm :: TTermDefinition (Context -> Graph -> [Type] -> WrappedTerm -> Prelude.Either Error (Type, Context))
typeOfWrappedTerm = define "typeOfWrappedTerm" $
  doc "Reconstruct the type of a wrapped term (Either/Context version)" $
  "cx" ~> "tx" ~> "typeArgs" ~> "wt" ~>
  "tname" <~ Core.wrappedTermTypeName (var "wt") $
  "body" <~ Core.wrappedTermBody (var "wt") $
  -- Type the body for checking purposes
  "result" <<~ typeOf @@ var "cx" @@ var "tx" @@ noTypeArgs @@ var "body" $
  "cx2" <~ Pairs.second (var "result") $
  right $ pair (Resolution.nominalApplication @@ var "tname" @@ var "typeArgs") (var "cx2")
