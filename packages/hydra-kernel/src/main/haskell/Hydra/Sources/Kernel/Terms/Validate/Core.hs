
module Hydra.Sources.Kernel.Terms.Validate.Core where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Sources.Kernel.Terms.Annotations as KernelAnnotations
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
import qualified Hydra.Dsl.Meta.Lib.Strings  as Strings
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
import qualified Hydra.Dsl.Validation   as Validation
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

import qualified Hydra.Sources.Kernel.Terms.Reflect   as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Variables as Variables


ns :: ModuleName
ns = ModuleName "hydra.validate.core"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Reflect.ns, Rewriting.ns, Variables.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Validation functions for core terms and types")}
  where
   definitions = [
     toDefinition appendFinding,
     toDefinition appendFindingType,
     toDefinition checkDuplicateBindings,
     toDefinition checkDuplicateFieldTypes,
     toDefinition checkDuplicateFields,
     toDefinition checkLiteral,
     toDefinition checkShadowing,
     toDefinition checkTerm,
     toDefinition checkUndefinedTypeVariablesInType,
     toDefinition checkUndefinedTypeVariablesInTypeScheme,
     toDefinition checkVoid,
     toDefinition enabled,
     toDefinition findDuplicate,
     toDefinition findDuplicateFieldType,
     toDefinition firstError,
     toDefinition firstFinding,
     toDefinition firstFindingType,
     toDefinition firstTypeError,
     toDefinition isValidName,
     toDefinition kernelDefaultCoreProfile,
     toDefinition term,
     toDefinition type_,
     toDefinition validateTypeNode]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Classify a rule-tagged 'Maybe (Name, InvalidTermError)' finding against
-- the active profile and append the payload (without its rule tag) to the
-- appropriate list in the accumulator. Findings whose rule appears in
-- 'errorRules' go to the errors list; findings whose rule appears in
-- 'warningRules' go to the warnings list. Each list respects its bound
-- ('maxErrors' / 'maxWarnings'); attempting to append past a bound is a
-- silent drop. A 'Nothing' input leaves the accumulator unchanged.
appendFinding :: TypedTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidTermError
  -> Maybe (Name, InvalidTermError)
  -> ValidationResult InvalidTermError)
appendFinding = define "appendFinding" $
  doc "Append a rule-tagged InvalidTermError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds." $
  "p" ~> "acc" ~> "finding" ~>
  Maybes.cases (var "finding")
    (var "acc")
    ("rp" ~>
      "ruleName" <~ Pairs.first (var "rp") $
      "payload" <~ Pairs.second (var "rp") $
      "errs" <~ Validation.validationResultErrors (var "acc") $
      "wrns" <~ Validation.validationResultWarnings (var "acc") $
      Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
        (Logic.ifElse (Equality.lt (Lists.length $ var "errs") (Validation.validationProfileMaxErrors $ var "p"))
          (Validation.validationResult
            (Lists.concat2 (var "errs") (Lists.singleton $ var "payload"))
            (var "wrns"))
          (var "acc"))
        (Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))
          (Logic.ifElse (Equality.lt (Lists.length $ var "wrns") (Validation.validationProfileMaxWarnings $ var "p"))
            (Validation.validationResult
              (var "errs")
              (Lists.concat2 (var "wrns") (Lists.singleton $ var "payload")))
            (var "acc"))
          (var "acc")))

-- | Type-side counterpart of 'appendFinding'.
appendFindingType :: TypedTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidTypeError
  -> Maybe (Name, InvalidTypeError)
  -> ValidationResult InvalidTypeError)
appendFindingType = define "appendFindingType" $
  doc "Append a rule-tagged InvalidTypeError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds." $
  "p" ~> "acc" ~> "finding" ~>
  Maybes.cases (var "finding")
    (var "acc")
    ("rp" ~>
      "ruleName" <~ Pairs.first (var "rp") $
      "payload" <~ Pairs.second (var "rp") $
      "errs" <~ Validation.validationResultErrors (var "acc") $
      "wrns" <~ Validation.validationResultWarnings (var "acc") $
      Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
        (Logic.ifElse (Equality.lt (Lists.length $ var "errs") (Validation.validationProfileMaxErrors $ var "p"))
          (Validation.validationResult
            (Lists.concat2 (var "errs") (Lists.singleton $ var "payload"))
            (var "wrns"))
          (var "acc"))
        (Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))
          (Logic.ifElse (Equality.lt (Lists.length $ var "wrns") (Validation.validationProfileMaxWarnings $ var "p"))
            (Validation.validationResult
              (var "errs")
              (Lists.concat2 (var "wrns") (Lists.singleton $ var "payload")))
            (var "acc"))
          (var "acc")))

-- | Check a list of bindings for duplicate names
checkDuplicateBindings :: TypedTermDefinition (SubtermPath -> [Binding] -> Maybe InvalidTermError)
checkDuplicateBindings = define "checkDuplicateBindings" $
  doc "Check for duplicate binding names in a list of bindings" $
  "path" ~> "bindings" ~>
  "names" <~ Lists.map (reify Core.bindingName) (var "bindings") $
  "dup" <~ findDuplicate @@ var "names" $
  Maybes.map ("name" ~>
    inject _InvalidTermError _InvalidTermError_duplicateBinding $
      record _DuplicateBindingError [
        _DuplicateBindingError_location>>: var "path",
        _DuplicateBindingError_name>>: var "name"])
    (var "dup")

-- | Check a list of field names for duplicates

-- | Check a list of FieldType for duplicate names, calling a handler on the first duplicate found
checkDuplicateFieldTypes :: TypedTermDefinition ([FieldType] -> (Name -> Maybe InvalidTypeError) -> Maybe InvalidTypeError)
checkDuplicateFieldTypes = define "checkDuplicateFieldTypes" $
  doc "Check for duplicate field names in a list of field types" $
  "fields" ~> "mkError" ~>
  "names" <~ Lists.map (reify Core.fieldTypeName) (var "fields") $
  "dup" <~ findDuplicateFieldType @@ var "names" $
  Maybes.cases (var "dup")
    noTypeError
    ("name" ~> var "mkError" @@ var "name")

-- | Find the first duplicate in a list of names (for field types)
-- | Check a list of field names for duplicates
checkDuplicateFields :: TypedTermDefinition (SubtermPath -> [Name] -> Maybe InvalidTermError)
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

-- | Check that a literal value's type matches an expected literal type
checkLiteral :: TypedTermDefinition (LiteralType -> Literal -> Maybe InvalidLiteralError)
checkLiteral = define "checkLiteral" $
  doc "Check that a literal value's type matches an expected literal type" $
  "expected" ~> "value" ~>
  "actual" <~ Reflect.literalType @@ var "value" $
  Logic.ifElse (Equality.equal (var "expected") (var "actual"))
    nothing
    (just $ inject _InvalidLiteralError _InvalidLiteralError_typeMismatch $
      record _LiteralTypeMismatchError [
        _LiteralTypeMismatchError_expectedType>>: var "expected",
        _LiteralTypeMismatchError_actualType>>: var "actual"])

-- | Find the first duplicate in a list, if any
-- | Check a list of names for shadowing against the current graph scope
checkShadowing :: TypedTermDefinition (SubtermPath -> Graph -> [Name] -> Maybe InvalidTermError)
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
-- | Check a single term node for validation errors (without recursing into subterms).
-- The Graph provides bound variable information; the typed flag controls System F checks.
-- The ValidationProfile gates which rules are evaluated; rules in neither
-- errorRules nor warningRules are skipped entirely. Findings are returned as
-- (ruleName, payload) pairs; the orchestrator uses the rule name to classify.
checkTerm :: TypedTermDefinition (ValidationProfile -> Bool -> SubtermPath -> Graph -> Term -> Maybe (Name, InvalidTermError))
checkTerm = define "checkTerm" $
  doc "Check a single term node for validation errors. Rules disabled by the profile are not evaluated." $
  "p" ~> "typed" ~> "path" ~> "cx" ~> "term" ~>
  cases _Term (var "term") (Just nothing) [

    -- T16/T20/T21: TermAnnotated — check for nested or empty annotations
    _Term_annotated>>: "ann" ~>
      "body" <~ Core.annotatedTermBody (var "ann") $
      "annMap" <~ KernelAnnotations.getAnnotationMap @@ (Core.annotatedTermAnnotation (var "ann")) $
      firstFinding @@ list [
        -- T21. EmptyTermAnnotationError
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_emptyTermAnnotation
          (Logic.ifElse (Maps.null $ var "annMap")
            (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTermAnnotation $
              record _EmptyTermAnnotationError [
                _EmptyTermAnnotationError_location>>: var "path"])
            noError),
        -- T20. NestedTermAnnotationError
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_nestedTermAnnotation
          (cases _Term (var "body") (Just noError) [
            _Term_annotated>>: constant $
              mkJust $ inject _InvalidTermError _InvalidTermError_nestedTermAnnotation $
                record _NestedTermAnnotationError [
                  _NestedTermAnnotationError_location>>: var "path"]])],

    -- TermApplication — check for constant conditions, self-application,
    -- identity application, redundant wrap/unwrap
    _Term_application>>: "app" ~>
      "fun" <~ Core.applicationFunction (var "app") $
      "arg" <~ Core.applicationArgument (var "app") $
      firstFinding @@ list [
        -- T13. ConstantConditionError: ifElse applied to a literal boolean
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_constantCondition
          (cases _Term (var "fun") (Just noError) [
            _Term_variable>>: "primName" ~>
              Logic.ifElse (Equality.equal (Core.unName $ var "primName") (string "hydra.lib.logic.ifElse"))
                (cases _Term (var "arg") (Just noError) [
                  _Term_literal>>: "lit" ~>
                    cases _Literal (var "lit") (Just noError) [
                      _Literal_boolean>>: "boolVal" ~>
                        mkJust $ inject _InvalidTermError _InvalidTermError_constantCondition $
                          record _ConstantConditionError [
                            _ConstantConditionError_location>>: var "path",
                            _ConstantConditionError_value>>: var "boolVal"]]])
                noError]),
        -- T15. SelfApplicationError: (TermVariable x) applied to (TermVariable x)
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_selfApplication
          (cases _Term (var "fun") (Just noError) [
            _Term_variable>>: "funName" ~>
              cases _Term (var "arg") (Just noError) [
                _Term_variable>>: "argName" ~>
                  Logic.ifElse (Equality.equal (var "funName") (var "argName"))
                    (mkJust $ inject _InvalidTermError _InvalidTermError_selfApplication $
                      record _SelfApplicationError [
                        _SelfApplicationError_location>>: var "path",
                        _SelfApplicationError_name>>: var "funName"])
                    noError]]),
        -- T16. UnnecessaryIdentityApplicationError: (\x -> x) applied to arg
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_unnecessaryIdentityApplication
          (cases _Term (var "fun") (Just noError) [
            _Term_lambda>>: "lam" ~>
              "param" <~ Core.lambdaParameter (var "lam") $
              "body" <~ Core.lambdaBody (var "lam") $
              cases _Term (var "body") (Just noError) [
                _Term_variable>>: "bodyVar" ~>
                  Logic.ifElse (Equality.equal (var "param") (var "bodyVar"))
                    (mkJust $ inject _InvalidTermError _InvalidTermError_unnecessaryIdentityApplication $
                      record _UnnecessaryIdentityApplicationError [
                        _UnnecessaryIdentityApplicationError_location>>: var "path"])
                    noError]]),
        -- T14. RedundantWrapUnwrapError: unwrap(n) applied to wrap(n, _)
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_redundantWrapUnwrap
          (cases _Term (var "fun") (Just noError) [
            _Term_unwrap>>: "unwrapName" ~>
              cases _Term (var "arg") (Just noError) [
                _Term_wrap>>: "wt" ~>
                  "wrapName" <~ Core.wrappedTermTypeName (var "wt") $
                  Logic.ifElse (Equality.equal (var "unwrapName") (var "wrapName"))
                    (mkJust $ inject _InvalidTermError _InvalidTermError_redundantWrapUnwrap $
                      record _RedundantWrapUnwrapError [
                        _RedundantWrapUnwrapError_location>>: var "path",
                        _RedundantWrapUnwrapError_typeName>>: var "unwrapName"])
                    noError]])],

    -- T5: TermRecord — check for empty type name, duplicate fields
    _Term_record>>: "rec" ~>
      "tname" <~ Core.recordTypeName (var "rec") $
      "flds" <~ Core.recordFields (var "rec") $
      firstFinding @@ list [
        -- T5. EmptyTypeNameInTermError
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_emptyTypeNameInTerm
          (Logic.ifElse (Equality.equal (Core.unName $ var "tname") (string ""))
            (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
              record _EmptyTypeNameInTermError [
                _EmptyTypeNameInTermError_location>>: var "path"])
            noError),
        -- T3. DuplicateRecordFieldNamesError (via DuplicateFieldError)
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_duplicateField
          (checkDuplicateFields @@ var "path" @@ (Lists.map
            (reify Core.fieldName)
            (var "flds")))],

    -- T1/T2/T10/T11/T18: TermLet — empty bindings, duplicates, shadowing, naming, type var checks
    _Term_let>>: "lt" ~>
      "bindings" <~ Core.letBindings (var "lt") $
      "names" <~ Lists.map (reify Core.bindingName) (var "bindings") $
      firstFinding @@ list [
        -- T1. EmptyLetBindingsError
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_emptyLetBindings
          (Logic.ifElse (Lists.null $ var "bindings")
            (mkJust $ inject _InvalidTermError _InvalidTermError_emptyLetBindings $
              record _EmptyLetBindingsError [
                _EmptyLetBindingsError_location>>: var "path"])
            noError),
        -- T2. DuplicateLetBindingNamesError (via DuplicateBindingError)
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_duplicateBinding
          (checkDuplicateBindings @@ var "path" @@ var "bindings"),
        -- T11. TermVariableShadowingError — deferred for let bindings.
        -- The fold framework extends the graph BEFORE calling checkTerm, so we cannot
        -- reliably detect shadowing without a pre-extension graph. Needs a new rewriting
        -- helper that passes both pre- and post-extension graphs.
        nothing,
        -- T18. InvalidLetBindingNameError — check each binding name
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_invalidLetBindingName
          (firstError @@ (Lists.map
            ("bname" ~> Logic.ifElse (isValidName @@ var "bname")
              noError
              (mkJust $ inject _InvalidTermError _InvalidTermError_invalidLetBindingName $
                record _InvalidLetBindingNameError [
                  _InvalidLetBindingNameError_location>>: var "path",
                  _InvalidLetBindingNameError_name>>: var "bname"]))
            (var "names"))),
        -- T10. UndefinedTypeVariableInBindingTypeError (typed mode only)
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_undefinedTypeVariableInBindingType
          (Logic.ifElse (var "typed")
            (firstError @@ (Lists.map
              ("b" ~> Maybes.cases (Core.bindingTypeScheme $ var "b")
                noError
                ("ts" ~> checkUndefinedTypeVariablesInTypeScheme
                  @@ var "path" @@ var "cx" @@ var "ts"
                  @@ ("uvName" ~>
                    mkJust $ inject _InvalidTermError _InvalidTermError_undefinedTypeVariableInBindingType $
                      record _UndefinedTypeVariableInBindingTypeError [
                        _UndefinedTypeVariableInBindingTypeError_location>>: var "path",
                        _UndefinedTypeVariableInBindingTypeError_name>>: var "uvName"])))
              (var "bindings")))
            noError)],

    -- T5: TermInject (injection) — check for empty type name
    _Term_inject>>: "inj" ~>
      "tname" <~ Core.injectionTypeName (var "inj") $
      guardedTermRule (var "p") _InvalidTermError _InvalidTermError_emptyTypeNameInTerm
        (Logic.ifElse (Equality.equal (Core.unName $ var "tname") (string ""))
          (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
            record _EmptyTypeNameInTermError [
              _EmptyTypeNameInTermError_location>>: var "path"])
          noError),

    -- T11/T17/T8: Lambda — shadowing, naming, undefined type vars in domain
    _Term_lambda>>: "lam" ~>
      "paramName" <~ Core.lambdaParameter (var "lam") $
      firstFinding @@ list [
        -- T11. TermVariableShadowingError
        -- Note: the graph has already been extended with this lambda's parameter.
        -- We check graphBoundTerms (not modified by lambdas) to detect let-to-lambda shadowing.
        -- Lambda-to-lambda shadowing is not detected here due to the pre-extension.
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_termVariableShadowing
          (Logic.ifElse
            (Maybes.isJust $ Maps.lookup (var "paramName") (Graph.graphBoundTerms $ var "cx"))
            (mkJust $ inject _InvalidTermError _InvalidTermError_termVariableShadowing $
              record _TermVariableShadowingError [
                _TermVariableShadowingError_location>>: var "path",
                _TermVariableShadowingError_name>>: var "paramName"])
            noError),
        -- T17. InvalidLambdaParameterNameError
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_invalidLambdaParameterName
          (Logic.ifElse (isValidName @@ var "paramName")
            noError
            (mkJust $ inject _InvalidTermError _InvalidTermError_invalidLambdaParameterName $
              record _InvalidLambdaParameterNameError [
                _InvalidLambdaParameterNameError_location>>: var "path",
                _InvalidLambdaParameterNameError_name>>: var "paramName"])),
        -- T8. UndefinedTypeVariableInLambdaDomainError (typed mode only)
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_undefinedTypeVariableInLambdaDomain
          (Logic.ifElse (var "typed")
            (Maybes.cases (Core.lambdaDomain $ var "lam")
              noError
              ("dom" ~> checkUndefinedTypeVariablesInType
                @@ var "path" @@ var "cx" @@ var "dom"
                @@ ("uvName" ~>
                  mkJust $ inject _InvalidTermError _InvalidTermError_undefinedTypeVariableInLambdaDomain $
                    record _UndefinedTypeVariableInLambdaDomainError [
                      _UndefinedTypeVariableInLambdaDomainError_location>>: var "path",
                      _UndefinedTypeVariableInLambdaDomainError_name>>: var "uvName"])))
            noError)],
    -- T5: Projection — check empty type name
    _Term_project>>: "proj" ~>
      "tname" <~ Core.projectionTypeName (var "proj") $
      guardedTermRule (var "p") _InvalidTermError _InvalidTermError_emptyTypeNameInTerm
        (Logic.ifElse (Equality.equal (Core.unName $ var "tname") (string ""))
          (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
            record _EmptyTypeNameInTermError [
              _EmptyTypeNameInTermError_location>>: var "path"])
          noError),
    -- T4/T5/T6: CaseStatement — check empty type name, empty cases, duplicate case fields
    _Term_cases>>: "cs" ~>
      "tname" <~ Core.caseStatementTypeName (var "cs") $
      "csDefault" <~ Core.caseStatementDefault (var "cs") $
      "csCases" <~ Core.caseStatementCases (var "cs") $
      firstFinding @@ list [
        -- T5. EmptyTypeNameInTermError
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_emptyTypeNameInTerm
          (Logic.ifElse (Equality.equal (Core.unName $ var "tname") (string ""))
            (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
              record _EmptyTypeNameInTermError [
                _EmptyTypeNameInTermError_location>>: var "path"])
            noError),
        -- T6. EmptyCaseStatementError
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_emptyCaseStatement
          (Logic.ifElse
            (Logic.and (Lists.null $ var "csCases") (Maybes.isNothing $ var "csDefault"))
            (mkJust $ inject _InvalidTermError _InvalidTermError_emptyCaseStatement $
              record _EmptyCaseStatementError [
                _EmptyCaseStatementError_location>>: var "path",
                _EmptyCaseStatementError_typeName>>: var "tname"])
            noError),
        -- T4. DuplicateCaseStatementFieldNamesError (via DuplicateFieldError)
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_duplicateField
          (checkDuplicateFields @@ var "path" @@ (Lists.map
            (reify Core.caseAlternativeName)
            (var "csCases")))],

    -- T9. UndefinedTypeVariableInTypeApplicationError (typed mode only)
    _Term_typeApplication>>: "ta" ~>
      guardedTermRule (var "p") _InvalidTermError _InvalidTermError_undefinedTypeVariableInTypeApplication
        (Logic.ifElse (var "typed")
          (checkUndefinedTypeVariablesInType
            @@ var "path" @@ var "cx" @@ (Core.typeApplicationTermType $ var "ta")
            @@ ("uvName" ~>
              mkJust $ inject _InvalidTermError _InvalidTermError_undefinedTypeVariableInTypeApplication $
                record _UndefinedTypeVariableInTypeApplicationError [
                  _UndefinedTypeVariableInTypeApplicationError_location>>: var "path",
                  _UndefinedTypeVariableInTypeApplicationError_name>>: var "uvName"]))
          noError),

    -- T12/T19: TypeLambda — type variable shadowing and naming
    _Term_typeLambda>>: "tl" ~>
      "tvName" <~ Core.typeLambdaParameter (var "tl") $
      firstFinding @@ list [
        -- T12. TypeVariableShadowingInTypeLambdaError
        -- Note: the graph has already been extended with this type lambda's parameter.
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_typeVariableShadowingInTypeLambda
          (Logic.ifElse
            (Sets.member (var "tvName") (Sets.delete (var "tvName") (Graph.graphTypeVariables $ var "cx")))
            (mkJust $ inject _InvalidTermError _InvalidTermError_typeVariableShadowingInTypeLambda $
              record _TypeVariableShadowingInTypeLambdaError [
                _TypeVariableShadowingInTypeLambdaError_location>>: var "path",
                _TypeVariableShadowingInTypeLambdaError_name>>: var "tvName"])
            noError),
        -- T19. InvalidTypeLambdaParameterNameError
        guardedTermRule (var "p") _InvalidTermError _InvalidTermError_invalidTypeLambdaParameterName
          (Logic.ifElse (isValidName @@ var "tvName")
            noError
            (mkJust $ inject _InvalidTermError _InvalidTermError_invalidTypeLambdaParameterName $
              record _InvalidTypeLambdaParameterNameError [
                _InvalidTypeLambdaParameterNameError_location>>: var "path",
                _InvalidTypeLambdaParameterNameError_name>>: var "tvName"]))],

    -- T7. UndefinedTermVariableError — check boundTerms, lambdaVariables, and primitives
    _Term_variable>>: "varName" ~>
      guardedTermRule (var "p") _InvalidTermError _InvalidTermError_undefinedTermVariable
        (Logic.ifElse
          (Logic.or
            (Maybes.isJust $ Maps.lookup (var "varName") (Graph.graphBoundTerms $ var "cx"))
            (Logic.or
              (Sets.member (var "varName") (Graph.graphLambdaVariables $ var "cx"))
              (Maybes.isJust $ Maps.lookup (var "varName") (Graph.graphPrimitives $ var "cx"))))
          noError
          (mkJust $ inject _InvalidTermError _InvalidTermError_undefinedTermVariable $
            record _UndefinedTermVariableError [
              _UndefinedTermVariableError_location>>: var "path",
              _UndefinedTermVariableError_name>>: var "varName"])),

    -- T5: TermWrap — check for empty type name
    _Term_wrap>>: "wt" ~>
      "tname" <~ Core.wrappedTermTypeName (var "wt") $
      guardedTermRule (var "p") _InvalidTermError _InvalidTermError_emptyTypeNameInTerm
        (Logic.ifElse (Equality.equal (Core.unName $ var "tname") (string ""))
          (mkJust $ inject _InvalidTermError _InvalidTermError_emptyTypeNameInTerm $
            record _EmptyTypeNameInTermError [
              _EmptyTypeNameInTermError_location>>: var "path"])
          noError)]

-- | Check a list of names for shadowing against the current graph scope
-- | Check a type for undefined type variables against the current graph scope.
-- Takes a path, graph, type, and a handler function that receives the first undefined variable name
-- and returns an error. Returns Nothing if all type variables are defined.
checkUndefinedTypeVariablesInType :: TypedTermDefinition (SubtermPath -> Graph -> Type -> (Name -> Maybe InvalidTermError) -> Maybe InvalidTermError)
checkUndefinedTypeVariablesInType = define "checkUndefinedTypeVariablesInType" $
  doc "Check a type for type variables not bound in the current scope" $
  "path" ~> "cx" ~> "typ" ~> "mkError" ~>
  "freeVars" <~ Variables.freeVariablesInType @@ var "typ" $
  "undefined" <~ Sets.difference (var "freeVars") (Graph.graphTypeVariables $ var "cx") $
  Maybes.cases (Lists.maybeHead $ Sets.toList $ var "undefined") noError ("firstUndefined" ~> var "mkError" @@ var "firstUndefined")

-- | Check a type scheme for undefined type variables against the current graph scope.
-- The scheme's own bound variables are excluded before checking.
-- | Check a type scheme for undefined type variables against the current graph scope.
-- The scheme's own bound variables are excluded before checking.
checkUndefinedTypeVariablesInTypeScheme :: TypedTermDefinition (SubtermPath -> Graph -> TypeScheme -> (Name -> Maybe InvalidTermError) -> Maybe InvalidTermError)
checkUndefinedTypeVariablesInTypeScheme = define "checkUndefinedTypeVariablesInTypeScheme" $
  doc "Check a type scheme for type variables not bound by the scheme or the current scope" $
  "path" ~> "cx" ~> "ts" ~> "mkError" ~>
  "freeVars" <~ Variables.freeVariablesInTypeScheme @@ var "ts" $
  "undefined" <~ Sets.difference (var "freeVars") (Graph.graphTypeVariables $ var "cx") $
  Maybes.cases (Lists.maybeHead $ Sets.toList $ var "undefined") noError ("firstUndefined" ~> var "mkError" @@ var "firstUndefined")

-- ============================================================================
-- Type validation
-- ============================================================================

-- | A Nothing of type Maybe InvalidTypeError
-- | Check if a type is TypeVoid and return a VoidInNonBottomPositionError if so
checkVoid :: TypedTermDefinition (Type -> Maybe InvalidTypeError)
checkVoid = define "checkVoid" $
  doc "Return an error if the given type is TypeVoid" $
  "typ" ~>
  cases _Type (var "typ") (Just noTypeError) [
    _Type_void>>: constant $
      mkJustType $ inject _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition $
        record _VoidInNonBottomPositionError [
          _VoidInNonBottomPositionError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep]))]]

-- | An empty ValidationResult (no errors, no warnings) parameterized by 'e'.
-- Used as the initial accumulator for the per-tree fold. The phantom type
-- variable 'e' is left polymorphic so the same constant works for both
-- InvalidTermError and InvalidTypeError walks.
emptyResult :: TypedTerm (ValidationResult e)
emptyResult = Validation.validationResult
  (list ([] :: [TypedTerm e]))
  (list ([] :: [TypedTerm e]))

-- | Wrap a leaf-shaped 'Maybe InvalidTermError' finding with the rule that
-- produced it, gated by the active profile. If the rule's qualified name is
-- not in either errorRules or warningRules, the inner finding expression
-- is never evaluated (Logic.ifElse short-circuits at DSL runtime). When the
-- rule is enabled and the inner finding is Just, returns 'Just (ruleName,
-- payload)'; otherwise 'Nothing'. Used to retrofit existing per-rule checks
-- in checkTerm without restructuring them.
guardedTermRule
  :: TypedTerm ValidationProfile
  -> Name -- ^ Union-type qualified name (e.g. _InvalidTermError).
  -> Name -- ^ Variant local name (e.g. _InvalidTermError_duplicateBinding).
  -> TypedTerm (Maybe InvalidTermError) -- ^ The leaf-shaped finding term.
  -> TypedTerm (Maybe (Name, InvalidTermError))
guardedTermRule profile unionName variantName findingExpr =
  Logic.ifElse (enabled @@ profile @@ ruleNameTerm)
    (Maybes.map ("f" ~> pair ruleNameTerm (var "f")) findingExpr)
    nothing
  where
    ruleNameTerm = nameLift (qualifiedRule unionName variantName)

-- | Type-side counterpart of 'guardedTermRule'.
guardedTypeRule
  :: TypedTerm ValidationProfile
  -> Name
  -> Name
  -> TypedTerm (Maybe InvalidTypeError)
  -> TypedTerm (Maybe (Name, InvalidTypeError))
guardedTypeRule profile unionName variantName findingExpr =
  Logic.ifElse (enabled @@ profile @@ ruleNameTerm)
    (Maybes.map ("f" ~> pair ruleNameTerm (var "f")) findingExpr)
    nothing
  where
    ruleNameTerm = nameLift (qualifiedRule unionName variantName)

-- | Test whether a rule is active in a profile (in either errorRules or
-- warningRules). A rule that is active is evaluated by validators; a rule
-- that is inactive is skipped entirely.
enabled :: TypedTermDefinition (ValidationProfile -> Name -> Bool)
enabled = define "enabled" $
  doc "True iff the given rule name appears in the profile's errorRules or warningRules." $
  "p" ~> "ruleName" ~>
  Logic.or
    (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
    (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))

-- | Check a single type node for validation errors (without recursing into subtypes).
-- | Find the first duplicate in a list, if any
findDuplicate :: TypedTermDefinition ([Name] -> Maybe Name)
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
-- | Find the first duplicate in a list of names (for field types)
findDuplicateFieldType :: TypedTermDefinition ([Name] -> Maybe Name)
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
-- | Return the first Just from a list of Maybe values, or Nothing
firstError :: TypedTermDefinition ([Maybe InvalidTermError] -> Maybe InvalidTermError)
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

-- | Return the first Just from a list of rule-tagged optional InvalidTermError findings.
-- Used by the new profile-aware validators where each finding carries its rule name.
firstFinding :: TypedTermDefinition ([Maybe (Name, InvalidTermError)] -> Maybe (Name, InvalidTermError))
firstFinding = define "firstFinding" $
  doc "Return the first rule-tagged finding from a list, or nothing if all are valid" $
  "checks" ~>
  Lists.foldl
    ("acc" ~> "check" ~>
      Maybes.cases (var "acc")
        (var "check")
        (constant $ var "acc"))
    nothing
    (var "checks")

-- | Type-side counterpart of 'firstFinding' for rule-tagged InvalidTypeError findings.
firstFindingType :: TypedTermDefinition ([Maybe (Name, InvalidTypeError)] -> Maybe (Name, InvalidTypeError))
firstFindingType = define "firstFindingType" $
  doc "Return the first rule-tagged type finding from a list, or nothing if all are valid" $
  "checks" ~>
  Lists.foldl
    ("acc" ~> "check" ~>
      Maybes.cases (var "acc")
        (var "check")
        (constant $ var "acc"))
    nothing
    (var "checks")

-- | Return the first Just from a list of Maybe InvalidTypeError values
firstTypeError :: TypedTermDefinition ([Maybe InvalidTypeError] -> Maybe InvalidTypeError)
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
-- | Validate a name at an introduction site.
-- Currently only rejects empty strings; may be extended with additional naming conventions.
isValidName :: TypedTermDefinition (Name -> Bool)
isValidName = define "isValidName" $
  doc "Check whether a name is valid at an introduction site. Currently rejects empty strings." $
  "name" ~>
  Logic.not $ Equality.equal (Core.unName $ var "name") (string "")

-- | A Just of type InvalidTermError -> Maybe InvalidTermError
justError :: TypedTerm InvalidTermError -> TypedTerm (Maybe InvalidTermError)
justError (TypedTerm t) = TypedTerm $ TermMaybe $ Just t

-- | The default validation profile for hydra.validate.core (term and type
-- validators). Every check currently wired up is in 'errorRules' except
-- 'InvalidTypeError.singleVariantUnion', which is treated as a warning.
-- 'maxErrors = 1' preserves the legacy 'first error wins' behaviour;
-- 'maxWarnings = 20' is a deliberately small starting cap.
kernelDefaultCoreProfile :: TypedTermDefinition ValidationProfile
kernelDefaultCoreProfile = define "kernelDefaultCoreProfile" $
  doc "The default validation profile for term and type validation, with every check classified as an error except InvalidTypeError.singleVariantUnion (warning); maxErrors=1, maxWarnings=20." $
  Validation.validationProfile
    (Sets.fromList $ list $ nameLift <$> coreErrorRules)
    (Sets.fromList $ list $ nameLift <$> coreWarningRules)
    (int32 1)
    (int32 20)
  where
    coreErrorRules :: [Name]
    coreErrorRules = L.concat
      [ fmap (qualifiedRule _InvalidTermError)
          [ _InvalidTermError_constantCondition
          , _InvalidTermError_duplicateBinding
          , _InvalidTermError_duplicateField
          , _InvalidTermError_emptyCaseStatement
          , _InvalidTermError_emptyLetBindings
          , _InvalidTermError_emptyTermAnnotation
          , _InvalidTermError_emptyTypeNameInTerm
          , _InvalidTermError_invalidLambdaParameterName
          , _InvalidTermError_invalidLetBindingName
          , _InvalidTermError_invalidTypeLambdaParameterName
          , _InvalidTermError_nestedTermAnnotation
          , _InvalidTermError_redundantWrapUnwrap
          , _InvalidTermError_selfApplication
          , _InvalidTermError_termVariableShadowing
          , _InvalidTermError_typeVariableShadowingInTypeLambda
          , _InvalidTermError_undefinedTermVariable
          , _InvalidTermError_undefinedTypeVariableInBindingType
          , _InvalidTermError_undefinedTypeVariableInLambdaDomain
          , _InvalidTermError_undefinedTypeVariableInTypeApplication
          , _InvalidTermError_unknownPrimitiveName
          , _InvalidTermError_unnecessaryIdentityApplication
          , _InvalidTermError_untypedTermVariable]
      , fmap (qualifiedRule _InvalidTypeError)
          [ _InvalidTypeError_duplicateRecordTypeFieldNames
          , _InvalidTypeError_duplicateUnionTypeFieldNames
          , _InvalidTypeError_emptyRecordType
          , _InvalidTypeError_emptyTypeAnnotation
          , _InvalidTypeError_emptyUnionType
          , _InvalidTypeError_invalidForallParameterName
          , _InvalidTypeError_invalidTypeSchemeVariableName
          , _InvalidTypeError_nestedTypeAnnotation
          , _InvalidTypeError_nonComparableMapKeyType
          , _InvalidTypeError_nonComparableSetElementType
          , _InvalidTypeError_typeVariableShadowingInForall
          , _InvalidTypeError_undefinedTypeVariable
          , _InvalidTypeError_voidInNonBottomPosition]]

    coreWarningRules :: [Name]
    coreWarningRules =
      [qualifiedRule _InvalidTypeError _InvalidTypeError_singleVariantUnion]

-- | Helper to make a just from a TypedTerm
mkJust :: TypedTerm InvalidTermError -> TypedTerm (Maybe InvalidTermError)
mkJust = just

-- | A Just of type InvalidTypeError -> Maybe InvalidTypeError
mkJustType :: TypedTerm InvalidTypeError -> TypedTerm (Maybe InvalidTypeError)
mkJustType = just

-- | A Nothing of type Maybe InvalidTermError
noError :: TypedTerm (Maybe InvalidTermError)
noError = TypedTerm $ TermMaybe Nothing

noTypeError :: TypedTerm (Maybe InvalidTypeError)
noTypeError = TypedTerm $ TermMaybe Nothing

-- | Compose a fully qualified rule identifier from a union-type qualified
-- name and a variant local name, joined with '.'. Used at profile-construction
-- time to derive rule IDs like 'hydra.error.core.InvalidTermError.duplicateBinding'
-- from the generated _InvalidTermError and _InvalidTermError_duplicateBinding
-- constants. Pure host-side helper; not exported as a kernel term.
qualifiedRule :: Name -> Name -> Name
qualifiedRule (Name u) (Name v) = Name (L.concat [u, ".", v])

-- | Validate a term against a profile, accumulating findings into a
-- 'ValidationResult InvalidTermError'. The profile classifies each rule as
-- error or warning (or skipped if absent from both sets). Traversal hard-stops
-- as soon as the errors list reaches 'maxErrors'; warnings continue to be
-- collected up to 'maxWarnings' but never cause termination. The 'typed'
-- parameter indicates whether to expect System F (typed) terms; when true,
-- type variable binding checks and UntypedTermVariableError are active.
term :: TypedTermDefinition (ValidationProfile -> Bool -> Graph -> Term -> ValidationResult InvalidTermError)
term = define "term" $
  doc "Validate a term against the given ValidationProfile, returning a ValidationResult. Errors hard-stop traversal once maxErrors is reached; warnings are bounded by maxWarnings without causing termination." $
  "p" ~> "typed" ~> "g" ~> "t" ~>
  Rewriting.foldTermWithGraphAndPath
    @@ ("recurse" ~> "path" ~> "cx" ~> "acc" ~> "trm" ~>
      -- Hard stop: if we have already collected maxErrors, leave the
      -- accumulator unchanged and skip the per-node check and the recursion.
      Logic.ifElse
        (Equality.gte
          (Lists.length $ Validation.validationResultErrors $ var "acc")
          (Validation.validationProfileMaxErrors $ var "p"))
        (var "acc")
        ("acc1" <~ appendFinding @@ var "p" @@ var "acc"
            @@ (checkTerm @@ var "p" @@ var "typed"
                  @@ (wrap _SubtermPath $ var "path") @@ var "cx" @@ var "trm") $
          -- After classifying the per-node finding, re-check the cap before
          -- recursing — appending an error may have just hit maxErrors.
          Logic.ifElse
            (Equality.gte
              (Lists.length $ Validation.validationResultErrors $ var "acc1")
              (Validation.validationProfileMaxErrors $ var "p"))
            (var "acc1")
            (var "recurse" @@ var "acc1" @@ var "trm")))
    @@ var "g"
    @@ emptyResult
    @@ var "t"

-- | Validate a type against a profile, accumulating findings into a
-- 'ValidationResult InvalidTypeError'. Recursively traverses the type,
-- tracking bound type variables through forall binders. The accumulator
-- threads through subtypes; recursion stops descending whenever the errors
-- list reaches 'maxErrors'. Warnings continue to be collected up to
-- 'maxWarnings' but never cause termination.
--
-- The accumulator is an explicit parameter (rather than starting fresh at
-- each subtype) so 'maxErrors' is enforced over the entire type tree, not
-- per subtree. The 'boundVars' set is the in-scope type variables; forall
-- binders extend it before recursing into the body.
type_ :: TypedTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidTypeError
  -> S.Set Name
  -> Type
  -> ValidationResult InvalidTypeError)
type_ = define "type" $
  doc "Validate a type against the given ValidationProfile, threading a ValidationResult accumulator through subtypes. Errors hard-stop traversal once maxErrors is reached." $
  "p" ~> "acc" ~> "boundVars" ~> "typ" ~>
  -- Hard stop: if maxErrors already reached, leave accumulator unchanged.
  Logic.ifElse
    (Equality.gte
      (Lists.length $ Validation.validationResultErrors $ var "acc")
      (Validation.validationProfileMaxErrors $ var "p"))
    (var "acc")
    -- Otherwise: classify the per-node finding, append it to the accumulator,
    -- then re-check the cap before recursing into subtypes.
    ("acc1" <~ appendFindingType @@ var "p" @@ var "acc"
        @@ (validateTypeNode @@ var "p" @@ var "boundVars" @@ var "typ") $
      Logic.ifElse
        (Equality.gte
          (Lists.length $ Validation.validationResultErrors $ var "acc1")
          (Validation.validationProfileMaxErrors $ var "p"))
        (var "acc1")
        -- Recurse into subtypes, threading the accumulator through. For
        -- multi-child cases, use Lists.foldl over a list of children so the
        -- max-errors cap is respected between children.
        (cases _Type (var "typ") (Just (var "acc1")) [
          -- For forall, extend bound vars before recursing into the body.
          _Type_forall>>: "ft" ~>
            "newBound" <~ Sets.insert (Core.forallTypeParameter $ var "ft") (var "boundVars") $
            type_ @@ var "p" @@ var "acc1" @@ var "newBound" @@ (Core.forallTypeBody $ var "ft"),
          _Type_annotated>>: "ann" ~>
            type_ @@ var "p" @@ var "acc1" @@ var "boundVars" @@ (Core.annotatedTypeBody $ var "ann"),
          _Type_application>>: "at" ~>
            "acc2" <~ type_ @@ var "p" @@ var "acc1" @@ var "boundVars" @@ (Core.applicationTypeFunction $ var "at") $
            type_ @@ var "p" @@ var "acc2" @@ var "boundVars" @@ (Core.applicationTypeArgument $ var "at"),
          _Type_either>>: "et" ~>
            "acc2" <~ type_ @@ var "p" @@ var "acc1" @@ var "boundVars" @@ (Core.eitherTypeLeft $ var "et") $
            type_ @@ var "p" @@ var "acc2" @@ var "boundVars" @@ (Core.eitherTypeRight $ var "et"),
          _Type_function>>: "ft" ~>
            "acc2" <~ type_ @@ var "p" @@ var "acc1" @@ var "boundVars" @@ (Core.functionTypeDomain $ var "ft") $
            type_ @@ var "p" @@ var "acc2" @@ var "boundVars" @@ (Core.functionTypeCodomain $ var "ft"),
          _Type_list>>: "lt" ~>
            type_ @@ var "p" @@ var "acc1" @@ var "boundVars" @@ var "lt",
          _Type_map>>: "mt" ~>
            "acc2" <~ type_ @@ var "p" @@ var "acc1" @@ var "boundVars" @@ (Core.mapTypeKeys $ var "mt") $
            type_ @@ var "p" @@ var "acc2" @@ var "boundVars" @@ (Core.mapTypeValues $ var "mt"),
          _Type_maybe>>: "mt" ~>
            type_ @@ var "p" @@ var "acc1" @@ var "boundVars" @@ var "mt",
          _Type_pair>>: "pt" ~>
            "acc2" <~ type_ @@ var "p" @@ var "acc1" @@ var "boundVars" @@ (Core.pairTypeFirst $ var "pt") $
            type_ @@ var "p" @@ var "acc2" @@ var "boundVars" @@ (Core.pairTypeSecond $ var "pt"),
          _Type_record>>: "fields" ~>
            -- Fold over field types, threading the accumulator. Each step
            -- re-checks the max-errors cap inside the recursive call.
            Lists.foldl
              ("a" ~> "f" ~> type_ @@ var "p" @@ var "a" @@ var "boundVars" @@ (Core.fieldTypeType $ var "f"))
              (var "acc1")
              (var "fields"),
          _Type_set>>: "st" ~>
            type_ @@ var "p" @@ var "acc1" @@ var "boundVars" @@ var "st",
          _Type_union>>: "fields" ~>
            Lists.foldl
              ("a" ~> "f" ~> type_ @@ var "p" @@ var "a" @@ var "boundVars" @@ (Core.fieldTypeType $ var "f"))
              (var "acc1")
              (var "fields"),
          _Type_wrap>>: "wt" ~>
            type_ @@ var "p" @@ var "acc1" @@ var "boundVars" @@ var "wt"]))

-- | Check if a type is TypeVoid and return a VoidInNonBottomPositionError if so
-- | Check a single type node for validation errors (without recursing into subtypes).
-- Profile-aware variant; rules disabled by the profile are skipped entirely.
validateTypeNode :: TypedTermDefinition (ValidationProfile -> S.Set Name -> Type -> Maybe (Name, InvalidTypeError))
validateTypeNode = define "validateTypeNode" $
  doc "Check a single type node for validation errors. Rules disabled by the profile are not evaluated." $
  "p" ~> "boundVars" ~> "typ" ~>
  cases _Type (var "typ") (Just nothing) [

    -- Y8/Y9: TypeAnnotated — nested or empty annotations
    _Type_annotated>>: "ann" ~>
      "body" <~ Core.annotatedTypeBody (var "ann") $
      "annMap" <~ KernelAnnotations.getAnnotationMap @@ (Core.annotatedTypeAnnotation (var "ann")) $
      firstFindingType @@ list [
        -- Y9. EmptyTypeAnnotationError
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_emptyTypeAnnotation
          (Logic.ifElse (Maps.null $ var "annMap")
            (mkJustType $ inject _InvalidTypeError _InvalidTypeError_emptyTypeAnnotation $
              record _EmptyTypeAnnotationError [
                _EmptyTypeAnnotationError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep]))])
            noTypeError),
        -- Y8. NestedTypeAnnotationError
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_nestedTypeAnnotation
          (cases _Type (var "body") (Just noTypeError) [
            _Type_annotated>>: constant $
              mkJustType $ inject _InvalidTypeError _InvalidTypeError_nestedTypeAnnotation $
                record _NestedTypeAnnotationError [
                  _NestedTypeAnnotationError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep]))]])],

    -- Y10: TypeEither — void in either components
    _Type_either>>: "et" ~>
      firstFindingType @@ list [
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition
          (checkVoid @@ (Core.eitherTypeLeft $ var "et")),
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition
          (checkVoid @@ (Core.eitherTypeRight $ var "et"))],

    -- Y7/Y13: TypeForall — type variable shadowing and naming
    _Type_forall>>: "ft" ~>
      "paramName" <~ Core.forallTypeParameter (var "ft") $
      firstFindingType @@ list [
        -- Y7. TypeVariableShadowingInForallError
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_typeVariableShadowingInForall
          (Logic.ifElse (Sets.member (var "paramName") (var "boundVars"))
            (mkJustType $ inject _InvalidTypeError _InvalidTypeError_typeVariableShadowingInForall $
              record _TypeVariableShadowingInForallError [
                _TypeVariableShadowingInForallError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep])),
                _TypeVariableShadowingInForallError_name>>: var "paramName"])
            noTypeError),
        -- Y13. InvalidForallParameterNameError
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_invalidForallParameterName
          (Logic.ifElse (isValidName @@ var "paramName")
            noTypeError
            (mkJustType $ inject _InvalidTypeError _InvalidTypeError_invalidForallParameterName $
              record _InvalidForallParameterNameError [
                _InvalidForallParameterNameError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep])),
                _InvalidForallParameterNameError_name>>: var "paramName"]))],

    -- Y10: TypeFunction — void in codomain
    _Type_function>>: "ft" ~>
      guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition
        (checkVoid @@ (Core.functionTypeCodomain $ var "ft")),

    -- Y10: TypeList — void element type
    _Type_list>>: "lt" ~>
      guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition
        (checkVoid @@ var "lt"),

    -- Y11/Y10: TypeMap — non-comparable key type, void in key/value
    _Type_map>>: "mt" ~>
      "keyType" <~ Core.mapTypeKeys (var "mt") $
      firstFindingType @@ list [
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_nonComparableMapKeyType
          (cases _Type (var "keyType") (Just noTypeError) [
            _Type_function>>: constant $
              mkJustType $ inject _InvalidTypeError _InvalidTypeError_nonComparableMapKeyType $
                record _NonComparableMapKeyTypeError [
                  _NonComparableMapKeyTypeError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep])),
                  _NonComparableMapKeyTypeError_keyType>>: var "keyType"]]),
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition
          (checkVoid @@ var "keyType"),
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition
          (checkVoid @@ (Core.mapTypeValues $ var "mt"))],

    -- Y10: TypePair — void in pair components
    _Type_pair>>: "pt" ~>
      firstFindingType @@ list [
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition
          (checkVoid @@ (Core.pairTypeFirst $ var "pt")),
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition
          (checkVoid @@ (Core.pairTypeSecond $ var "pt"))],

    -- Y1/Y4/Y10: TypeRecord — empty, duplicate field names, void in fields
    _Type_record>>: "fields" ~>
      firstFindingType @@ list [
        -- Y1. EmptyRecordTypeError
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_emptyRecordType
          (Logic.ifElse (Lists.null $ var "fields")
            (mkJustType $ inject _InvalidTypeError _InvalidTypeError_emptyRecordType $
              record _EmptyRecordTypeError [
                _EmptyRecordTypeError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep]))])
            noTypeError),
        -- Y4. DuplicateRecordTypeFieldNamesError
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_duplicateRecordTypeFieldNames
          (checkDuplicateFieldTypes @@ (var "fields")
            @@ ("dupName" ~>
              mkJustType $ inject _InvalidTypeError _InvalidTypeError_duplicateRecordTypeFieldNames $
                record _DuplicateRecordTypeFieldNamesError [
                  _DuplicateRecordTypeFieldNamesError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep])),
                  _DuplicateRecordTypeFieldNamesError_name>>: var "dupName"])),
        -- Y10. VoidInNonBottomPositionError — check field types
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition
          (firstTypeError @@ (Lists.map
            ("f" ~> checkVoid @@ (Core.fieldTypeType $ var "f"))
            (var "fields")))],

    -- Y12/Y10: TypeSet — non-comparable element type, void element
    _Type_set>>: "elemType" ~>
      firstFindingType @@ list [
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_nonComparableSetElementType
          (cases _Type (var "elemType") (Just noTypeError) [
            _Type_function>>: constant $
              mkJustType $ inject _InvalidTypeError _InvalidTypeError_nonComparableSetElementType $
                record _NonComparableSetElementTypeError [
                  _NonComparableSetElementTypeError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep])),
                  _NonComparableSetElementTypeError_elementType>>: var "elemType"]]),
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition
          (checkVoid @@ var "elemType")],

    -- Y2/Y3/Y5/Y10: TypeUnion — empty, single-variant, duplicate field names, void in fields
    _Type_union>>: "fields" ~>
      firstFindingType @@ list [
        -- Y2. EmptyUnionTypeError
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_emptyUnionType
          (Logic.ifElse (Lists.null $ var "fields")
            (mkJustType $ inject _InvalidTypeError _InvalidTypeError_emptyUnionType $
              record _EmptyUnionTypeError [
                _EmptyUnionTypeError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep]))])
            noTypeError),
        -- Y3. SingleVariantUnionError. Profile-aware: kernelDefaultCoreProfile
        -- classifies this as a warning rather than an error, since
        -- single-variant unions are sometimes a deliberate design choice
        -- (an extensible variant kept as a union from the start so that
        -- adding a second variant is a non-breaking change).
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_singleVariantUnion
          (Logic.ifElse (Equality.equal (Lists.length $ var "fields") (int32 1))
            (Maybes.cases (Lists.maybeHead $ var "fields") noTypeError ("singleField" ~>
                mkJustType $ inject _InvalidTypeError _InvalidTypeError_singleVariantUnion $
                  record _SingleVariantUnionError [
                    _SingleVariantUnionError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep])),
                    _SingleVariantUnionError_fieldName>>: Core.fieldTypeName (var "singleField")]))
            noTypeError),
        -- Y5. DuplicateUnionTypeFieldNamesError
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_duplicateUnionTypeFieldNames
          (checkDuplicateFieldTypes @@ (var "fields")
            @@ ("dupName" ~>
              mkJustType $ inject _InvalidTypeError _InvalidTypeError_duplicateUnionTypeFieldNames $
                record _DuplicateUnionTypeFieldNamesError [
                  _DuplicateUnionTypeFieldNamesError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep])),
                  _DuplicateUnionTypeFieldNamesError_name>>: var "dupName"])),
        -- Y10. VoidInNonBottomPositionError — check field types
        guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_voidInNonBottomPosition
          (firstTypeError @@ (Lists.map
            ("f" ~> checkVoid @@ (Core.fieldTypeType $ var "f"))
            (var "fields")))],

    -- Y6. UndefinedTypeVariableError
    _Type_variable>>: "varName" ~>
      guardedTypeRule (var "p") _InvalidTypeError _InvalidTypeError_undefinedTypeVariable
        (Logic.ifElse (Sets.member (var "varName") (var "boundVars"))
          noTypeError
          (mkJustType $ inject _InvalidTypeError _InvalidTypeError_undefinedTypeVariable $
            record _UndefinedTypeVariableError [
              _UndefinedTypeVariableError_location>>: wrap _SubtermPath (list ([] :: [TypedTerm SubtermStep])),
              _UndefinedTypeVariableError_name>>: var "varName"]))]

-- | Check a list of FieldType for duplicate names, calling a handler on the first duplicate found
