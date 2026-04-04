
module Hydra.Sources.Kernel.Terms.Validate.Core where

-- Standard imports for kernel terms modules
import Hydra.Kernel
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
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (
  binding, elimination, field, fields, fieldType, floatType, floatValue, function, injection, integerType,
  integerValue, lambda, literal, literalType, term, type_, typeScheme)
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
import qualified Hydra.Dsl.Errors            as Error
import qualified Hydra.Dsl.Error.Core       as ErrorsCore
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Variables as Variables


ns :: Namespace
ns = Namespace "hydra.validate.core"

module_ :: Module
module_ = Module ns elements
    [Rewriting.ns, Variables.ns]
    kernelTypesNamespaces $
    Just "Validation functions for core terms and types"
  where
   elements = [
     toDefinition checkDuplicateBindings,
     toDefinition checkDuplicateFields,
     toDefinition checkDuplicateFieldTypes,
     toDefinition checkShadowing,
     toDefinition checkTerm,
     toDefinition validateTypeNode,
     toDefinition checkUndefinedTypeVariablesInType,
     toDefinition checkUndefinedTypeVariablesInTypeScheme,
     toDefinition checkVoid,
     toDefinition findDuplicate,
     toDefinition findDuplicateFieldType,
     toDefinition firstError,
     toDefinition firstTypeError,
     toDefinition isValidName,
     toDefinition term,
     toDefinition type_]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- | A Nothing of type Maybe InvalidTermError
noError :: TTerm (Maybe InvalidTermError)
noError = TTerm $ TermMaybe Nothing

-- | A Just of type InvalidTermError -> Maybe InvalidTermError
justError :: TTerm InvalidTermError -> TTerm (Maybe InvalidTermError)
justError (TTerm t) = TTerm $ TermMaybe $ Just t

-- | Helper to make a just from a TTerm
mkJust :: TTerm InvalidTermError -> TTerm (Maybe InvalidTermError)
mkJust = just

-- | Return the first Just from a list of Maybe values, or Nothing
firstError :: TTermDefinition ([Maybe InvalidTermError] -> Maybe InvalidTermError)
firstError = define "firstError" $
  doc "Return the first error from a list of optional errors, or nothing if all are valid" $
  "checks" ~>
  Lists.foldl
    ("acc" ~> "check" ~>
      Maybes.cases (var "acc")
        (var "check")
        (constant $ var "acc"))
    noError
    (var "checks")

term :: TTermDefinition (Bool -> Graph -> Term -> Maybe InvalidTermError)
term = define "term" $
  doc "Validate a term, returning the first error found or nothing if valid. The 'typed' parameter indicates whether to expect System F (typed) terms; when true, type variable binding checks and UntypedTermVariableError are active." $
  "typed" ~> "g" ~> "t" ~>
  Rewriting.foldTermWithGraphAndPath
    @@ ("recurse" ~> "path" ~> "cx" ~> "acc" ~> "trm" ~>
      -- If we already found an error, short-circuit
      Maybes.cases (var "acc")
        -- No error yet: check the current term, then recurse into subterms
        ("checkResult" <~ (checkTerm @@ var "typed" @@ (wrap _SubtermPath $ var "path") @@ var "cx" @@ var "trm") $
          Maybes.cases (var "checkResult")
            -- No error at this term: let the framework recurse into subterms
            (var "recurse" @@ noError @@ var "trm")
            -- Found an error: return it
            ("err" ~> justError (var "err")))
        -- Already have an error: propagate it
        ("_" ~> var "acc"))
    @@ var "g"
    @@ noError
    @@ var "t"

-- | Check a single term node for validation errors (without recursing into subterms).
-- The Graph provides bound variable information; the typed flag controls System F checks.
checkTerm :: TTermDefinition (Bool -> SubtermPath -> Graph -> Term -> Maybe InvalidTermError)
checkTerm = define "checkTerm" $
  doc "Check a single term node for validation errors" $
  "typed" ~> "path" ~> "cx" ~> "term" ~>
  cases _Term (var "term") (Just noError) [

    -- T16/T20/T21: TermAnnotated — check for nested or empty annotations
    _Term_annotated>>: "ann" ~>
      "body" <~ Core.annotatedTermBody (var "ann") $
      "annMap" <~ Core.annotatedTermAnnotation (var "ann") $
      firstError @@ list [
        -- T21. EmptyTermAnnotationError
        Logic.ifElse (Maps.null $ var "annMap")
          (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTermAnnotation $
            record _EmptyTermAnnotationError [
              _EmptyTermAnnotationError_location>>: var "path"])
          noError,
        -- T20. NestedTermAnnotationError
        cases _Term (var "body") (Just noError) [
          _Term_annotated>>: constant $
            mkJust $ inject _InvalidTermError _InvalidTermError_nestedTermAnnotation $
              record _NestedTermAnnotationError [
                _NestedTermAnnotationError_location>>: var "path"]]],

    -- TermApplication — check for constant conditions, self-application,
    -- identity application, redundant wrap/unwrap
    _Term_application>>: "app" ~>
      "fun" <~ Core.applicationFunction (var "app") $
      "arg" <~ Core.applicationArgument (var "app") $
      firstError @@ list [
        -- T13. ConstantConditionError: ifElse applied to a literal boolean
        cases _Term (var "fun") (Just noError) [
          _Term_function>>: "f" ~>
            cases _Function (var "f") (Just noError) [
              _Function_primitive>>: "primName" ~>
                Logic.ifElse (Equality.equal (Core.unName $ var "primName") (string "hydra.lib.logic.ifElse"))
                  (cases _Term (var "arg") (Just noError) [
                    _Term_literal>>: "lit" ~>
                      cases _Literal (var "lit") (Just noError) [
                        _Literal_boolean>>: "boolVal" ~>
                          mkJust $ inject _InvalidTermError _InvalidTermError_constantCondition $
                            record _ConstantConditionError [
                              _ConstantConditionError_location>>: var "path",
                              _ConstantConditionError_value>>: var "boolVal"]]])
                  noError]],
        -- T15. SelfApplicationError: (TermVariable x) applied to (TermVariable x)
        cases _Term (var "fun") (Just noError) [
          _Term_variable>>: "funName" ~>
            cases _Term (var "arg") (Just noError) [
              _Term_variable>>: "argName" ~>
                Logic.ifElse (Equality.equal (var "funName") (var "argName"))
                  (mkJust $ inject _InvalidTermError _InvalidTermError_selfApplication $
                    record _SelfApplicationError [
                      _SelfApplicationError_location>>: var "path",
                      _SelfApplicationError_name>>: var "funName"])
                  noError]],
        -- T16. UnnecessaryIdentityApplicationError: (\x -> x) applied to arg
        cases _Term (var "fun") (Just noError) [
          _Term_function>>: "f" ~>
            cases _Function (var "f") (Just noError) [
              _Function_lambda>>: "lam" ~>
                "param" <~ Core.lambdaParameter (var "lam") $
                "body" <~ Core.lambdaBody (var "lam") $
                cases _Term (var "body") (Just noError) [
                  _Term_variable>>: "bodyVar" ~>
                    Logic.ifElse (Equality.equal (var "param") (var "bodyVar"))
                      (mkJust $ inject _InvalidTermError _InvalidTermError_unnecessaryIdentityApplication $
                        record _UnnecessaryIdentityApplicationError [
                          _UnnecessaryIdentityApplicationError_location>>: var "path"])
                      noError]]],
        -- T14. RedundantWrapUnwrapError: unwrap(n) applied to wrap(n, _)
        cases _Term (var "fun") (Just noError) [
          _Term_function>>: "f" ~>
            cases _Function (var "f") (Just noError) [
              _Function_elimination>>: "elim" ~>
                cases _Elimination (var "elim") (Just noError) [
                  _Elimination_wrap>>: "unwrapName" ~>
                    cases _Term (var "arg") (Just noError) [
                      _Term_wrap>>: "wt" ~>
                        "wrapName" <~ Core.wrappedTermTypeName (var "wt") $
                        Logic.ifElse (Equality.equal (var "unwrapName") (var "wrapName"))
                          (mkJust $ inject _InvalidTermError _InvalidTermError_redundantWrapUnwrap $
                            record _RedundantWrapUnwrapError [
                              _RedundantWrapUnwrapError_location>>: var "path",
                              _RedundantWrapUnwrapError_typeName>>: var "unwrapName"])
                          noError]]]]],

    -- T5: TermRecord — check for empty type name, duplicate fields
    _Term_record>>: "rec" ~>
      "tname" <~ Core.recordTypeName (var "rec") $
      "flds" <~ Core.recordFields (var "rec") $
      firstError @@ list [
        -- T5. EmptyTypeNameInTermError
        Logic.ifElse (Equality.equal (Core.unName $ var "tname") (string ""))
          (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
            record _EmptyTypeNameInTermError [
              _EmptyTypeNameInTermError_location>>: var "path"])
          noError,
        -- T3. DuplicateRecordFieldNamesError (via DuplicateFieldError)
        checkDuplicateFields @@ var "path" @@ (Lists.map
          (unaryFunction Core.fieldName)
          (var "flds"))],

    -- T1/T2/T10/T11/T18: TermLet — empty bindings, duplicates, shadowing, naming, type var checks
    _Term_let>>: "lt" ~>
      "bindings" <~ Core.letBindings (var "lt") $
      "names" <~ Lists.map (unaryFunction Core.bindingName) (var "bindings") $
      firstError @@ list [
        -- T1. EmptyLetBindingsError
        Logic.ifElse (Lists.null $ var "bindings")
          (mkJust $ inject _InvalidTermError _InvalidTermError_emptyLetBindings $
            record _EmptyLetBindingsError [
              _EmptyLetBindingsError_location>>: var "path"])
          noError,
        -- T2. DuplicateLetBindingNamesError (via DuplicateBindingError)
        checkDuplicateBindings @@ var "path" @@ var "bindings",
        -- T11. TermVariableShadowingError — deferred for let bindings.
        -- The fold framework extends the graph BEFORE calling checkTerm, so we cannot
        -- reliably detect shadowing without a pre-extension graph. Needs a new rewriting
        -- helper that passes both pre- and post-extension graphs.
        noError,
        -- T18. InvalidLetBindingNameError — check each binding name
        firstError @@ (Lists.map
          ("bname" ~> Logic.ifElse (isValidName @@ var "bname")
            noError
            (mkJust $ inject _InvalidTermError _InvalidTermError_invalidLetBindingName $
              record _InvalidLetBindingNameError [
                _InvalidLetBindingNameError_location>>: var "path",
                _InvalidLetBindingNameError_name>>: var "bname"]))
          (var "names")),
        -- T10. UndefinedTypeVariableInBindingTypeError (typed mode only)
        Logic.ifElse (var "typed")
          (firstError @@ (Lists.map
            ("b" ~> Maybes.cases (Core.bindingType $ var "b")
              noError
              ("ts" ~> checkUndefinedTypeVariablesInTypeScheme
                @@ var "path" @@ var "cx" @@ var "ts"
                @@ ("uvName" ~>
                  mkJust $ inject _InvalidTermError _InvalidTermError_undefinedTypeVariableInBindingType $
                    record _UndefinedTypeVariableInBindingTypeError [
                      _UndefinedTypeVariableInBindingTypeError_location>>: var "path",
                      _UndefinedTypeVariableInBindingTypeError_name>>: var "uvName"])))
            (var "bindings")))
          noError],

    -- T5: TermUnion (injection) — check for empty type name
    _Term_union>>: "inj" ~>
      "tname" <~ Core.injectionTypeName (var "inj") $
      Logic.ifElse (Equality.equal (Core.unName $ var "tname") (string ""))
        (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
          record _EmptyTypeNameInTermError [
            _EmptyTypeNameInTermError_location>>: var "path"])
        noError,

    -- T11/T22/T5: TermFunction — check for shadowing, unknown primitives, empty type names
    _Term_function>>: "fun" ~>
      cases _Function (var "fun") (Just noError) [
        -- T11/T17/T8: Lambda — shadowing, naming, undefined type vars in domain
        _Function_lambda>>: "lam" ~>
          "paramName" <~ Core.lambdaParameter (var "lam") $
          firstError @@ list [
            -- T11. TermVariableShadowingError
            -- Note: the graph has already been extended with this lambda's parameter.
            -- We check graphBoundTerms (not modified by lambdas) to detect let-to-lambda shadowing.
            -- Lambda-to-lambda shadowing is not detected here due to the pre-extension.
            Logic.ifElse
              (Maybes.isJust $ Maps.lookup (var "paramName") (Graph.graphBoundTerms $ var "cx"))
              (mkJust $ inject _InvalidTermError _InvalidTermError_termVariableShadowing $
                record _TermVariableShadowingError [
                  _TermVariableShadowingError_location>>: var "path",
                  _TermVariableShadowingError_name>>: var "paramName"])
              noError,
            -- T17. InvalidLambdaParameterNameError
            Logic.ifElse (isValidName @@ var "paramName")
              noError
              (mkJust $ inject _InvalidTermError _InvalidTermError_invalidLambdaParameterName $
                record _InvalidLambdaParameterNameError [
                  _InvalidLambdaParameterNameError_location>>: var "path",
                  _InvalidLambdaParameterNameError_name>>: var "paramName"]),
            -- T8. UndefinedTypeVariableInLambdaDomainError (typed mode only)
            Logic.ifElse (var "typed")
              (Maybes.cases (Core.lambdaDomain $ var "lam")
                noError
                ("dom" ~> checkUndefinedTypeVariablesInType
                  @@ var "path" @@ var "cx" @@ var "dom"
                  @@ ("uvName" ~>
                    mkJust $ inject _InvalidTermError _InvalidTermError_undefinedTypeVariableInLambdaDomain $
                      record _UndefinedTypeVariableInLambdaDomainError [
                        _UndefinedTypeVariableInLambdaDomainError_location>>: var "path",
                        _UndefinedTypeVariableInLambdaDomainError_name>>: var "uvName"])))
              noError],
        -- T22. UnknownPrimitiveNameError
        _Function_primitive>>: "primName" ~>
          Logic.ifElse
            (Maybes.isJust $ Maps.lookup (var "primName") (Graph.graphPrimitives $ var "cx"))
            noError
            (mkJust $ inject _InvalidTermError _InvalidTermError_unknownPrimitiveName $
              record _UnknownPrimitiveNameError [
                _UnknownPrimitiveNameError_location>>: var "path",
                _UnknownPrimitiveNameError_name>>: var "primName"]),
        -- Elimination checks
        _Function_elimination>>: "elim" ~>
          cases _Elimination (var "elim") (Just noError) [
            -- T5: Projection — check empty type name
            _Elimination_record>>: "proj" ~>
              "tname" <~ Core.projectionTypeName (var "proj") $
              Logic.ifElse (Equality.equal (Core.unName $ var "tname") (string ""))
                (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
                  record _EmptyTypeNameInTermError [
                    _EmptyTypeNameInTermError_location>>: var "path"])
                noError,
            -- T4/T5/T6: CaseStatement — check empty type name, empty cases, duplicate case fields
            _Elimination_union>>: "cs" ~>
              "tname" <~ Core.caseStatementTypeName (var "cs") $
              "csDefault" <~ Core.caseStatementDefault (var "cs") $
              "csCases" <~ Core.caseStatementCases (var "cs") $
              firstError @@ list [
                -- T5. EmptyTypeNameInTermError
                Logic.ifElse (Equality.equal (Core.unName $ var "tname") (string ""))
                  (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
                    record _EmptyTypeNameInTermError [
                      _EmptyTypeNameInTermError_location>>: var "path"])
                  noError,
                -- T6. EmptyCaseStatementError
                Logic.ifElse
                  (Logic.and (Lists.null $ var "csCases") (Maybes.isNothing $ var "csDefault"))
                  (mkJust $ inject _InvalidTermError _InvalidTermError_emptyCaseStatement $
                    record _EmptyCaseStatementError [
                      _EmptyCaseStatementError_location>>: var "path",
                      _EmptyCaseStatementError_typeName>>: var "tname"])
                  noError,
                -- T4. DuplicateCaseStatementFieldNamesError (via DuplicateFieldError)
                checkDuplicateFields @@ var "path" @@ (Lists.map
                  (unaryFunction Core.fieldName)
                  (var "csCases"))]]],

    -- T9. UndefinedTypeVariableInTypeApplicationError (typed mode only)
    _Term_typeApplication>>: "ta" ~>
      Logic.ifElse (var "typed")
        (checkUndefinedTypeVariablesInType
          @@ var "path" @@ var "cx" @@ (Core.typeApplicationTermType $ var "ta")
          @@ ("uvName" ~>
            mkJust $ inject _InvalidTermError _InvalidTermError_undefinedTypeVariableInTypeApplication $
              record _UndefinedTypeVariableInTypeApplicationError [
                _UndefinedTypeVariableInTypeApplicationError_location>>: var "path",
                _UndefinedTypeVariableInTypeApplicationError_name>>: var "uvName"]))
        noError,

    -- T12/T19: TypeLambda — type variable shadowing and naming
    _Term_typeLambda>>: "tl" ~>
      "tvName" <~ Core.typeLambdaParameter (var "tl") $
      firstError @@ list [
        -- T12. TypeVariableShadowingInTypeLambdaError
        -- Note: the graph has already been extended with this type lambda's parameter.
        Logic.ifElse
          (Sets.member (var "tvName") (Sets.delete (var "tvName") (Graph.graphTypeVariables $ var "cx")))
          (mkJust $ inject _InvalidTermError _InvalidTermError_typeVariableShadowingInTypeLambda $
            record _TypeVariableShadowingInTypeLambdaError [
              _TypeVariableShadowingInTypeLambdaError_location>>: var "path",
              _TypeVariableShadowingInTypeLambdaError_name>>: var "tvName"])
          noError,
        -- T19. InvalidTypeLambdaParameterNameError
        Logic.ifElse (isValidName @@ var "tvName")
          noError
          (mkJust $ inject _InvalidTermError _InvalidTermError_invalidTypeLambdaParameterName $
            record _InvalidTypeLambdaParameterNameError [
              _InvalidTypeLambdaParameterNameError_location>>: var "path",
              _InvalidTypeLambdaParameterNameError_name>>: var "tvName"])],

    -- T7. UndefinedTermVariableError — check boundTerms, lambdaVariables, and primitives
    _Term_variable>>: "varName" ~>
      Logic.ifElse
        (Logic.or
          (Maybes.isJust $ Maps.lookup (var "varName") (Graph.graphBoundTerms $ var "cx"))
          (Logic.or
            (Sets.member (var "varName") (Graph.graphLambdaVariables $ var "cx"))
            (Maybes.isJust $ Maps.lookup (var "varName") (Graph.graphPrimitives $ var "cx"))))
        noError
        (mkJust $ inject _InvalidTermError _InvalidTermError_undefinedTermVariable $
          record _UndefinedTermVariableError [
            _UndefinedTermVariableError_location>>: var "path",
            _UndefinedTermVariableError_name>>: var "varName"]),

    -- T5: TermWrap — check for empty type name
    _Term_wrap>>: "wt" ~>
      "tname" <~ Core.wrappedTermTypeName (var "wt") $
      Logic.ifElse (Equality.equal (Core.unName $ var "tname") (string ""))
        (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
          record _EmptyTypeNameInTermError [
            _EmptyTypeNameInTermError_location>>: var "path"])
        noError]

-- | Check a list of names for shadowing against the current graph scope
checkShadowing :: TTermDefinition (SubtermPath -> Graph -> [Name] -> Maybe InvalidTermError)
checkShadowing = define "checkShadowing" $
  doc "Check if any name in a list shadows a variable already in scope" $
  "path" ~> "cx" ~> "names" ~>
  -- Find the first name that is already bound
  "result" <~ Lists.foldl
    ("acc" ~> "name" ~>
      Maybes.cases (var "acc")
        (Logic.ifElse
          (Logic.or
            (Maybes.isJust $ Maps.lookup (var "name") (Graph.graphBoundTerms $ var "cx"))
            (Sets.member (var "name") (Graph.graphLambdaVariables $ var "cx")))
          (mkJust $ inject _InvalidTermError _InvalidTermError_termVariableShadowing $
            record _TermVariableShadowingError [
              _TermVariableShadowingError_location>>: var "path",
              _TermVariableShadowingError_name>>: var "name"])
          noError)
        (constant $ var "acc"))
    noError
    (var "names") $
  var "result"

-- | Check a list of bindings for duplicate names
checkDuplicateBindings :: TTermDefinition (SubtermPath -> [Binding] -> Maybe InvalidTermError)
checkDuplicateBindings = define "checkDuplicateBindings" $
  doc "Check for duplicate binding names in a list of bindings" $
  "path" ~> "bindings" ~>
  "names" <~ Lists.map (unaryFunction Core.bindingName) (var "bindings") $
  "dup" <~ findDuplicate @@ var "names" $
  Maybes.map ("name" ~>
    inject _InvalidTermError _InvalidTermError_duplicateBinding $
      record _DuplicateBindingError [
        _DuplicateBindingError_location>>: var "path",
        _DuplicateBindingError_name>>: var "name"])
    (var "dup")

-- | Check a list of field names for duplicates
checkDuplicateFields :: TTermDefinition (SubtermPath -> [Name] -> Maybe InvalidTermError)
checkDuplicateFields = define "checkDuplicateFields" $
  doc "Check for duplicate field names in a list of fields" $
  "path" ~> "names" ~>
  "dup" <~ findDuplicate @@ var "names" $
  Maybes.map ("name" ~>
    inject _InvalidTermError _InvalidTermError_duplicateField $
      record _DuplicateFieldError [
        _DuplicateFieldError_location>>: var "path",
        _DuplicateFieldError_name>>: var "name"])
    (var "dup")

-- | Find the first duplicate in a list, if any
findDuplicate :: TTermDefinition ([Name] -> Maybe Name)
findDuplicate = define "findDuplicate" $
  doc "Find the first duplicate name in a list" $
  "names" ~>
  -- Fold through names, tracking seen names in a set.
  -- Accumulator is (Set Name, Maybe Name): seen set and first duplicate found.
  "result" <~ Lists.foldl
    ("acc" ~> "name" ~>
      "seen" <~ Pairs.first (var "acc") $
      "dup" <~ Pairs.second (var "acc") $
      Maybes.cases (var "dup")
        (Logic.ifElse (Sets.member (var "name") (var "seen"))
          (pair (var "seen") (just $ var "name"))
          (pair (Sets.insert (var "name") (var "seen")) nothing))
        (constant $ var "acc"))
    (pair Sets.empty nothing)
    (var "names") $
  Pairs.second (var "result")

-- | Validate a name at an introduction site.
-- Currently only rejects empty strings; may be extended with additional naming conventions.
isValidName :: TTermDefinition (Name -> Bool)
isValidName = define "isValidName" $
  doc "Check whether a name is valid at an introduction site. Currently rejects empty strings." $
  "name" ~>
  Logic.not $ Equality.equal (Core.unName $ var "name") (string "")

-- | Check a type for undefined type variables against the current graph scope.
-- Takes a path, graph, type, and a handler function that receives the first undefined variable name
-- and returns an error. Returns Nothing if all type variables are defined.
checkUndefinedTypeVariablesInType :: TTermDefinition (SubtermPath -> Graph -> Type -> (Name -> Maybe InvalidTermError) -> Maybe InvalidTermError)
checkUndefinedTypeVariablesInType = define "checkUndefinedTypeVariablesInType" $
  doc "Check a type for type variables not bound in the current scope" $
  "path" ~> "cx" ~> "typ" ~> "mkError" ~>
  "freeVars" <~ Variables.freeVariablesInType @@ var "typ" $
  "undefined" <~ Sets.difference (var "freeVars") (Graph.graphTypeVariables $ var "cx") $
  Logic.ifElse (Sets.null $ var "undefined")
    noError
    ("firstUndefined" <~ Lists.head (Sets.toList $ var "undefined") $
      var "mkError" @@ var "firstUndefined")

-- | Check a type scheme for undefined type variables against the current graph scope.
-- The scheme's own bound variables are excluded before checking.
checkUndefinedTypeVariablesInTypeScheme :: TTermDefinition (SubtermPath -> Graph -> TypeScheme -> (Name -> Maybe InvalidTermError) -> Maybe InvalidTermError)
checkUndefinedTypeVariablesInTypeScheme = define "checkUndefinedTypeVariablesInTypeScheme" $
  doc "Check a type scheme for type variables not bound by the scheme or the current scope" $
  "path" ~> "cx" ~> "ts" ~> "mkError" ~>
  "freeVars" <~ Variables.freeVariablesInTypeScheme @@ var "ts" $
  "undefined" <~ Sets.difference (var "freeVars") (Graph.graphTypeVariables $ var "cx") $
  Logic.ifElse (Sets.null $ var "undefined")
    noError
    ("firstUndefined" <~ Lists.head (Sets.toList $ var "undefined") $
      var "mkError" @@ var "firstUndefined")

-- ============================================================================
-- Type validation
-- ============================================================================

-- | A Nothing of type Maybe InvalidTypeError
noTypeError :: TTerm (Maybe InvalidTypeError)
noTypeError = TTerm $ TermMaybe Nothing

-- | A Just of type InvalidTypeError -> Maybe InvalidTypeError
mkJustType :: TTerm InvalidTypeError -> TTerm (Maybe InvalidTypeError)
mkJustType = just

-- | Return the first Just from a list of Maybe InvalidTypeError values
firstTypeError :: TTermDefinition ([Maybe InvalidTypeError] -> Maybe InvalidTypeError)
firstTypeError = define "firstTypeError" $
  doc "Return the first type error from a list of optional errors, or nothing if all are valid" $
  "checks" ~>
  Lists.foldl
    ("acc" ~> "check" ~>
      Maybes.cases (var "acc")
        (var "check")
        (constant $ var "acc"))
    noTypeError
    (var "checks")

-- | Validate a type, returning the first error found or nothing if valid.
-- Recursively traverses the type, tracking bound type variables through forall binders.
type_ :: TTermDefinition (S.Set Name -> Type -> Maybe InvalidTypeError)
type_ = define "type" $
  doc "Validate a type, returning the first error found or nothing if valid. The first argument is the set of type variables already in scope." $
  "boundVars" ~> "typ" ~>
  -- Check the current node first
  "checkResult" <~ validateTypeNode @@ var "boundVars" @@ var "typ" $
  Maybes.cases (var "checkResult")
    -- No error at this node: recurse into subtypes with updated scope
    (cases _Type (var "typ") (Just noTypeError) [
      -- For forall, extend bound vars before recursing into the body
      _Type_forall>>: "ft" ~>
        "newBound" <~ Sets.insert (Core.forallTypeParameter $ var "ft") (var "boundVars") $
        type_ @@ var "newBound" @@ (Core.forallTypeBody $ var "ft"),
      -- For all other types, recurse into subtypes with the same bound vars
      _Type_annotated>>: "ann" ~> type_ @@ var "boundVars" @@ (Core.annotatedTypeBody $ var "ann"),
      _Type_application>>: "at" ~>
        firstTypeError @@ list [
          type_ @@ var "boundVars" @@ (Core.applicationTypeFunction $ var "at"),
          type_ @@ var "boundVars" @@ (Core.applicationTypeArgument $ var "at")],
      _Type_either>>: "et" ~>
        firstTypeError @@ list [
          type_ @@ var "boundVars" @@ (Core.eitherTypeLeft $ var "et"),
          type_ @@ var "boundVars" @@ (Core.eitherTypeRight $ var "et")],
      _Type_function>>: "ft" ~>
        firstTypeError @@ list [
          type_ @@ var "boundVars" @@ (Core.functionTypeDomain $ var "ft"),
          type_ @@ var "boundVars" @@ (Core.functionTypeCodomain $ var "ft")],
      _Type_list>>: "lt" ~> type_ @@ var "boundVars" @@ var "lt",
      _Type_map>>: "mt" ~>
        firstTypeError @@ list [
          type_ @@ var "boundVars" @@ (Core.mapTypeKeys $ var "mt"),
          type_ @@ var "boundVars" @@ (Core.mapTypeValues $ var "mt")],
      _Type_maybe>>: "mt" ~> type_ @@ var "boundVars" @@ var "mt",
      _Type_pair>>: "pt" ~>
        firstTypeError @@ list [
          type_ @@ var "boundVars" @@ (Core.pairTypeFirst $ var "pt"),
          type_ @@ var "boundVars" @@ (Core.pairTypeSecond $ var "pt")],
      _Type_record>>: "fields" ~>
        firstTypeError @@ (Lists.map
          ("f" ~> type_ @@ var "boundVars" @@ (Core.fieldTypeType $ var "f"))
          (var "fields")),
      _Type_set>>: "st" ~> type_ @@ var "boundVars" @@ var "st",
      _Type_union>>: "fields" ~>
        firstTypeError @@ (Lists.map
          ("f" ~> type_ @@ var "boundVars" @@ (Core.fieldTypeType $ var "f"))
          (var "fields")),
      _Type_wrap>>: "wt" ~> type_ @@ var "boundVars" @@ var "wt"])
    -- Found an error: return it immediately
    ("err" ~> mkJustType (var "err"))

-- | Check if a type is TypeVoid and return a VoidInNonBottomPositionError if so
checkVoid :: TTermDefinition (Type -> Maybe InvalidTypeError)
checkVoid = define "checkVoid" $
  doc "Return an error if the given type is TypeVoid" $
  "typ" ~>
  cases _Type (var "typ") (Just noTypeError) [
    _Type_void>>: constant $
      mkJustType $ inject _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition $
        record _VoidInNonBottomPositionError [
          _VoidInNonBottomPositionError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep]))]]

-- | Check a single type node for validation errors (without recursing into subtypes).
validateTypeNode :: TTermDefinition (S.Set Name -> Type -> Maybe InvalidTypeError)
validateTypeNode = define "validateTypeNode" $
  doc "Check a single type node for validation errors" $
  "boundVars" ~> "typ" ~>
  cases _Type (var "typ") (Just noTypeError) [

    -- Y8/Y9: TypeAnnotated — nested or empty annotations
    _Type_annotated>>: "ann" ~>
      "body" <~ Core.annotatedTypeBody (var "ann") $
      "annMap" <~ Core.annotatedTypeAnnotation (var "ann") $
      firstTypeError @@ list [
        -- Y9. EmptyTypeAnnotationError
        Logic.ifElse (Maps.null $ var "annMap")
          (mkJustType $ inject _InvalidTypeError _InvalidTypeError_emptyTypeAnnotation $
            record _EmptyTypeAnnotationError [
              _EmptyTypeAnnotationError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep]))])
          noTypeError,
        -- Y8. NestedTypeAnnotationError
        cases _Type (var "body") (Just noTypeError) [
          _Type_annotated>>: constant $
            mkJustType $ inject _InvalidTypeError _InvalidTypeError_nestedTypeAnnotation $
              record _NestedTypeAnnotationError [
                _NestedTypeAnnotationError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep]))]]],

    -- Y10: TypeEither — void in either components
    _Type_either>>: "et" ~>
      firstTypeError @@ list [
        checkVoid @@ (Core.eitherTypeLeft $ var "et"),
        checkVoid @@ (Core.eitherTypeRight $ var "et")],

    -- Y7/Y13: TypeForall — type variable shadowing and naming
    _Type_forall>>: "ft" ~>
      "paramName" <~ Core.forallTypeParameter (var "ft") $
      firstTypeError @@ list [
        -- Y7. TypeVariableShadowingInForallError
        Logic.ifElse (Sets.member (var "paramName") (var "boundVars"))
          (mkJustType $ inject _InvalidTypeError _InvalidTypeError_typeVariableShadowingInForall $
            record _TypeVariableShadowingInForallError [
              _TypeVariableShadowingInForallError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep])),
              _TypeVariableShadowingInForallError_name>>: var "paramName"])
          noTypeError,
        -- Y13. InvalidForallParameterNameError
        Logic.ifElse (isValidName @@ var "paramName")
          noTypeError
          (mkJustType $ inject _InvalidTypeError _InvalidTypeError_invalidForallParameterName $
            record _InvalidForallParameterNameError [
              _InvalidForallParameterNameError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep])),
              _InvalidForallParameterNameError_name>>: var "paramName"])],

    -- Y10: TypeFunction — void in codomain
    _Type_function>>: "ft" ~>
      checkVoid @@ (Core.functionTypeCodomain $ var "ft"),

    -- Y10: TypeList — void element type
    _Type_list>>: "lt" ~> checkVoid @@ var "lt",

    -- Y11/Y10: TypeMap — non-comparable key type, void in key/value
    _Type_map>>: "mt" ~>
      "keyType" <~ Core.mapTypeKeys (var "mt") $
      firstTypeError @@ list [
        cases _Type (var "keyType") (Just noTypeError) [
          _Type_function>>: constant $
            mkJustType $ inject _InvalidTypeError _InvalidTypeError_nonComparableMapKeyType $
              record _NonComparableMapKeyTypeError [
                _NonComparableMapKeyTypeError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep])),
                _NonComparableMapKeyTypeError_keyType>>: var "keyType"]],
        checkVoid @@ var "keyType",
        checkVoid @@ (Core.mapTypeValues $ var "mt")],

    -- Y10: TypePair — void in pair components
    _Type_pair>>: "pt" ~>
      firstTypeError @@ list [
        checkVoid @@ (Core.pairTypeFirst $ var "pt"),
        checkVoid @@ (Core.pairTypeSecond $ var "pt")],

    -- Y1/Y4/Y10: TypeRecord — empty, duplicate field names, void in fields
    _Type_record>>: "fields" ~>
      firstTypeError @@ list [
        -- Y1. EmptyRecordTypeError
        Logic.ifElse (Lists.null $ var "fields")
          (mkJustType $ inject _InvalidTypeError _InvalidTypeError_emptyRecordType $
            record _EmptyRecordTypeError [
              _EmptyRecordTypeError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep]))])
          noTypeError,
        -- Y4. DuplicateRecordTypeFieldNamesError
        checkDuplicateFieldTypes @@ (var "fields")
          @@ ("dupName" ~>
            mkJustType $ inject _InvalidTypeError _InvalidTypeError_duplicateRecordTypeFieldNames $
              record _DuplicateRecordTypeFieldNamesError [
                _DuplicateRecordTypeFieldNamesError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep])),
                _DuplicateRecordTypeFieldNamesError_name>>: var "dupName"]),
        -- Y10. VoidInNonBottomPositionError — check field types
        firstTypeError @@ (Lists.map
          ("f" ~> checkVoid @@ (Core.fieldTypeType $ var "f"))
          (var "fields"))],

    -- Y12/Y10: TypeSet — non-comparable element type, void element
    _Type_set>>: "elemType" ~>
      firstTypeError @@ list [
        cases _Type (var "elemType") (Just noTypeError) [
          _Type_function>>: constant $
            mkJustType $ inject _InvalidTypeError _InvalidTypeError_nonComparableSetElementType $
              record _NonComparableSetElementTypeError [
                _NonComparableSetElementTypeError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep])),
                _NonComparableSetElementTypeError_elementType>>: var "elemType"]],
        checkVoid @@ var "elemType"],

    -- Y2/Y3/Y5/Y10: TypeUnion — empty, single-variant, duplicate field names, void in fields
    _Type_union>>: "fields" ~>
      firstTypeError @@ list [
        -- Y2. EmptyUnionTypeError
        Logic.ifElse (Lists.null $ var "fields")
          (mkJustType $ inject _InvalidTypeError _InvalidTypeError_emptyUnionType $
            record _EmptyUnionTypeError [
              _EmptyUnionTypeError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep]))])
          noTypeError,
        -- Y3. SingleVariantUnionError
        Logic.ifElse (Equality.equal (Lists.length $ var "fields") (int32 1))
          ("singleField" <~ Lists.head (var "fields") $
            mkJustType $ inject _InvalidTypeError _InvalidTypeError_singleVariantUnion $
              record _SingleVariantUnionError [
                _SingleVariantUnionError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep])),
                _SingleVariantUnionError_fieldName>>: Core.fieldTypeName (var "singleField")])
          noTypeError,
        -- Y5. DuplicateUnionTypeFieldNamesError
        checkDuplicateFieldTypes @@ (var "fields")
          @@ ("dupName" ~>
            mkJustType $ inject _InvalidTypeError _InvalidTypeError_duplicateUnionTypeFieldNames $
              record _DuplicateUnionTypeFieldNamesError [
                _DuplicateUnionTypeFieldNamesError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep])),
                _DuplicateUnionTypeFieldNamesError_name>>: var "dupName"]),
        -- Y10. VoidInNonBottomPositionError — check field types
        firstTypeError @@ (Lists.map
          ("f" ~> checkVoid @@ (Core.fieldTypeType $ var "f"))
          (var "fields"))],

    -- Y6. UndefinedTypeVariableError
    _Type_variable>>: "varName" ~>
      Logic.ifElse (Sets.member (var "varName") (var "boundVars"))
        noTypeError
        (mkJustType $ inject _InvalidTypeError _InvalidTypeError_undefinedTypeVariable $
          record _UndefinedTypeVariableError [
            _UndefinedTypeVariableError_location>>: wrap _SubtermPath (list ([] :: [TTerm SubtermStep])),
            _UndefinedTypeVariableError_name>>: var "varName"])]

-- | Check a list of FieldType for duplicate names, calling a handler on the first duplicate found
checkDuplicateFieldTypes :: TTermDefinition ([FieldType] -> (Name -> Maybe InvalidTypeError) -> Maybe InvalidTypeError)
checkDuplicateFieldTypes = define "checkDuplicateFieldTypes" $
  doc "Check for duplicate field names in a list of field types" $
  "fields" ~> "mkError" ~>
  "names" <~ Lists.map (unaryFunction Core.fieldTypeName) (var "fields") $
  "dup" <~ findDuplicateFieldType @@ var "names" $
  Maybes.cases (var "dup")
    noTypeError
    ("name" ~> var "mkError" @@ var "name")

-- | Find the first duplicate in a list of names (for field types)
findDuplicateFieldType :: TTermDefinition ([Name] -> Maybe Name)
findDuplicateFieldType = define "findDuplicateFieldType" $
  doc "Find the first duplicate name in a list (for field type validation)" $
  "names" ~>
  "result" <~ Lists.foldl
    ("acc" ~> "name" ~>
      "seen" <~ Pairs.first (var "acc") $
      "dup" <~ Pairs.second (var "acc") $
      Maybes.cases (var "dup")
        (Logic.ifElse (Sets.member (var "name") (var "seen"))
          (pair (var "seen") (just $ var "name"))
          (pair (Sets.insert (var "name") (var "seen")) nothing))
        (constant $ var "acc"))
    (pair Sets.empty nothing)
    (var "names") $
  Pairs.second (var "result")
