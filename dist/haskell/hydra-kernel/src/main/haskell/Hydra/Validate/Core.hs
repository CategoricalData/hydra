-- Note: this is an automatically generated file. Do not edit.
-- | Validation functions for core terms and types

module Hydra.Validate.Core where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Relational as Relational
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variables as Variables
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | Append a rule-tagged InvalidTermError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds.
appendFinding :: Validation.ValidationProfile -> Validation.ValidationResult t0 -> Maybe (Core.Name, t0) -> Validation.ValidationResult t0
appendFinding p acc finding =
    Maybes.cases finding acc (\rp ->
      let ruleName = Pairs.first rp
          payload = Pairs.second rp
          errs = Validation.validationResultErrors acc
          wrns = Validation.validationResultWarnings acc
      in (Logic.ifElse (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Logic.ifElse (Equality.lt (Lists.length errs) (Validation.validationProfileMaxErrors p)) (Validation.ValidationResult {
        Validation.validationResultErrors = (Lists.concat2 errs (Lists.singleton payload)),
        Validation.validationResultWarnings = wrns}) acc) (Logic.ifElse (Sets.member ruleName (Validation.validationProfileWarningRules p)) (Logic.ifElse (Equality.lt (Lists.length wrns) (Validation.validationProfileMaxWarnings p)) (Validation.ValidationResult {
        Validation.validationResultErrors = errs,
        Validation.validationResultWarnings = (Lists.concat2 wrns (Lists.singleton payload))}) acc) acc)))
-- | Append a rule-tagged InvalidTypeError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds.
appendFindingType :: Validation.ValidationProfile -> Validation.ValidationResult t0 -> Maybe (Core.Name, t0) -> Validation.ValidationResult t0
appendFindingType p acc finding =
    Maybes.cases finding acc (\rp ->
      let ruleName = Pairs.first rp
          payload = Pairs.second rp
          errs = Validation.validationResultErrors acc
          wrns = Validation.validationResultWarnings acc
      in (Logic.ifElse (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Logic.ifElse (Equality.lt (Lists.length errs) (Validation.validationProfileMaxErrors p)) (Validation.ValidationResult {
        Validation.validationResultErrors = (Lists.concat2 errs (Lists.singleton payload)),
        Validation.validationResultWarnings = wrns}) acc) (Logic.ifElse (Sets.member ruleName (Validation.validationProfileWarningRules p)) (Logic.ifElse (Equality.lt (Lists.length wrns) (Validation.validationProfileMaxWarnings p)) (Validation.ValidationResult {
        Validation.validationResultErrors = errs,
        Validation.validationResultWarnings = (Lists.concat2 wrns (Lists.singleton payload))}) acc) acc)))
-- | Check for duplicate binding names in a list of bindings
checkDuplicateBindings :: Paths.SubtermPath -> [Core.Binding] -> Maybe ErrorCore.InvalidTermError
checkDuplicateBindings path bindings =

      let names = Lists.map Core.bindingName bindings
          dup = findDuplicate names
      in (Maybes.map (\name -> ErrorCore.InvalidTermErrorDuplicateBinding (ErrorCore.DuplicateBindingError {
        ErrorCore.duplicateBindingErrorLocation = path,
        ErrorCore.duplicateBindingErrorName = name})) dup)
-- | Check for duplicate field names in a list of field types
checkDuplicateFieldTypes :: [Core.FieldType] -> (Core.Name -> Maybe t0) -> Maybe t0
checkDuplicateFieldTypes fields mkError =

      let names = Lists.map Core.fieldTypeName fields
          dup = findDuplicateFieldType names
      in (Maybes.cases dup Nothing (\name -> mkError name))
-- | Check for duplicate field names in a list of fields
checkDuplicateFields :: Paths.SubtermPath -> [Core.Name] -> Maybe ErrorCore.InvalidTermError
checkDuplicateFields path names =

      let dup = findDuplicate names
      in (Maybes.map (\name -> ErrorCore.InvalidTermErrorDuplicateField (ErrorCore.DuplicateFieldError {
        ErrorCore.duplicateFieldErrorLocation = path,
        ErrorCore.duplicateFieldErrorName = name})) dup)
-- | Check that a literal value's type matches an expected literal type
checkLiteral :: Core.LiteralType -> Core.Literal -> Maybe ErrorCore.InvalidLiteralError
checkLiteral expected value =

      let actual = Reflect.literalType value
      in (Logic.ifElse (Equality.equal expected actual) Nothing (Just (ErrorCore.InvalidLiteralErrorTypeMismatch (ErrorCore.LiteralTypeMismatchError {
        ErrorCore.literalTypeMismatchErrorExpectedType = expected,
        ErrorCore.literalTypeMismatchErrorActualType = actual}))))
-- | Check if any name in a list shadows a variable already in scope
checkShadowing :: Paths.SubtermPath -> Graph.Graph -> [Core.Name] -> Maybe ErrorCore.InvalidTermError
checkShadowing path cx names =

      let result =
              Lists.foldl (\acc -> \name -> Maybes.cases acc (Logic.ifElse (Logic.or (Maybes.isJust (Maps.lookup name (Graph.graphBoundTerms cx))) (Sets.member name (Graph.graphLambdaVariables cx))) (Just (ErrorCore.InvalidTermErrorTermVariableShadowing (ErrorCore.TermVariableShadowingError {
                ErrorCore.termVariableShadowingErrorLocation = path,
                ErrorCore.termVariableShadowingErrorName = name}))) Nothing) (\_ -> acc)) Nothing names
      in result
-- | Check a single term node for validation errors. Rules disabled by the profile are not evaluated.
checkTerm :: Validation.ValidationProfile -> Bool -> Paths.SubtermPath -> Graph.Graph -> Core.Term -> Maybe (Core.Name, ErrorCore.InvalidTermError)
checkTerm p typed path cx term =
    case term of
      Core.TermAnnotated v0 ->
        let body = Core.annotatedTermBody v0
            annMap = Core.annotatedTermAnnotation v0
        in (firstFinding [
          Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.emptyTermAnnotation")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.emptyTermAnnotation", f)) (Logic.ifElse (Maps.null annMap) (Just (ErrorCore.InvalidTermErrorEmptyTermAnnotation (ErrorCore.EmptyTermAnnotationError {
            ErrorCore.emptyTermAnnotationErrorLocation = path}))) Nothing)) Nothing,
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.nestedTermAnnotation")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.nestedTermAnnotation", f)) (case body of
            Core.TermAnnotated _ -> Just (ErrorCore.InvalidTermErrorNestedTermAnnotation (ErrorCore.NestedTermAnnotationError {
              ErrorCore.nestedTermAnnotationErrorLocation = path}))
            _ -> Nothing)) Nothing)])
      Core.TermApplication v0 ->
        let fun = Core.applicationFunction v0
            arg = Core.applicationArgument v0
        in (firstFinding [
          Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.constantCondition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.constantCondition", f)) (case fun of
            Core.TermVariable v1 -> Logic.ifElse (Equality.equal (Core.unName v1) "hydra.lib.logic.ifElse") (case arg of
              Core.TermLiteral v2 -> case v2 of
                Core.LiteralBoolean v3 -> Just (ErrorCore.InvalidTermErrorConstantCondition (ErrorCore.ConstantConditionError {
                  ErrorCore.constantConditionErrorLocation = path,
                  ErrorCore.constantConditionErrorValue = v3}))
                _ -> Nothing
              _ -> Nothing) Nothing
            _ -> Nothing)) Nothing,
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.selfApplication")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.selfApplication", f)) (case fun of
            Core.TermVariable v1 -> case arg of
              Core.TermVariable v2 -> Logic.ifElse (Equality.equal v1 v2) (Just (ErrorCore.InvalidTermErrorSelfApplication (ErrorCore.SelfApplicationError {
                ErrorCore.selfApplicationErrorLocation = path,
                ErrorCore.selfApplicationErrorName = v1}))) Nothing
              _ -> Nothing
            _ -> Nothing)) Nothing),
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.unnecessaryIdentityApplication")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.unnecessaryIdentityApplication", f)) (case fun of
            Core.TermLambda v1 ->
              let param = Core.lambdaParameter v1
                  body = Core.lambdaBody v1
              in case body of
                Core.TermVariable v2 -> Logic.ifElse (Equality.equal param v2) (Just (ErrorCore.InvalidTermErrorUnnecessaryIdentityApplication (ErrorCore.UnnecessaryIdentityApplicationError {
                  ErrorCore.unnecessaryIdentityApplicationErrorLocation = path}))) Nothing
                _ -> Nothing
            _ -> Nothing)) Nothing),
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.redundantWrapUnwrap")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.redundantWrapUnwrap", f)) (case fun of
            Core.TermUnwrap v1 -> case arg of
              Core.TermWrap v2 ->
                let wrapName = Core.wrappedTermTypeName v2
                in (Logic.ifElse (Equality.equal v1 wrapName) (Just (ErrorCore.InvalidTermErrorRedundantWrapUnwrap (ErrorCore.RedundantWrapUnwrapError {
                  ErrorCore.redundantWrapUnwrapErrorLocation = path,
                  ErrorCore.redundantWrapUnwrapErrorTypeName = v1}))) Nothing)
              _ -> Nothing
            _ -> Nothing)) Nothing)])
      Core.TermRecord v0 ->
        let tname = Core.recordTypeName v0
            flds = Core.recordFields v0
        in (firstFinding [
          Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.emptyTypeNameInTerm")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.emptyTypeNameInTerm", f)) (Logic.ifElse (Equality.equal (Core.unName tname) "") (Just (ErrorCore.InvalidTermErrorEmptyTypeNameInTerm (ErrorCore.EmptyTypeNameInTermError {
            ErrorCore.emptyTypeNameInTermErrorLocation = path}))) Nothing)) Nothing,
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.duplicateField")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.duplicateField", f)) (checkDuplicateFields path (Lists.map Core.fieldName flds))) Nothing)])
      Core.TermLet v0 ->
        let bindings = Core.letBindings v0
            names = Lists.map Core.bindingName bindings
        in (firstFinding [
          Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.emptyLetBindings")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.emptyLetBindings", f)) (Logic.ifElse (Lists.null bindings) (Just (ErrorCore.InvalidTermErrorEmptyLetBindings (ErrorCore.EmptyLetBindingsError {
            ErrorCore.emptyLetBindingsErrorLocation = path}))) Nothing)) Nothing,
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.duplicateBinding")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.duplicateBinding", f)) (checkDuplicateBindings path bindings)) Nothing),
          Nothing,
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.invalidLetBindingName")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.invalidLetBindingName", f)) (firstError (Lists.map (\bname -> Logic.ifElse (isValidName bname) Nothing (Just (ErrorCore.InvalidTermErrorInvalidLetBindingName (ErrorCore.InvalidLetBindingNameError {
            ErrorCore.invalidLetBindingNameErrorLocation = path,
            ErrorCore.invalidLetBindingNameErrorName = bname})))) names))) Nothing),
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.undefinedTypeVariableInBindingType")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.undefinedTypeVariableInBindingType", f)) (Logic.ifElse typed (firstError (Lists.map (\b -> Maybes.cases (Core.bindingTypeScheme b) Nothing (\ts -> checkUndefinedTypeVariablesInTypeScheme path cx ts (\uvName -> Just (ErrorCore.InvalidTermErrorUndefinedTypeVariableInBindingType (ErrorCore.UndefinedTypeVariableInBindingTypeError {
            ErrorCore.undefinedTypeVariableInBindingTypeErrorLocation = path,
            ErrorCore.undefinedTypeVariableInBindingTypeErrorName = uvName}))))) bindings)) Nothing)) Nothing)])
      Core.TermInject v0 ->
        let tname = Core.injectionTypeName v0
        in (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.emptyTypeNameInTerm")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.emptyTypeNameInTerm", f)) (Logic.ifElse (Equality.equal (Core.unName tname) "") (Just (ErrorCore.InvalidTermErrorEmptyTypeNameInTerm (ErrorCore.EmptyTypeNameInTermError {
          ErrorCore.emptyTypeNameInTermErrorLocation = path}))) Nothing)) Nothing)
      Core.TermLambda v0 ->
        let paramName = Core.lambdaParameter v0
        in (firstFinding [
          Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.termVariableShadowing")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.termVariableShadowing", f)) (Logic.ifElse (Maybes.isJust (Maps.lookup paramName (Graph.graphBoundTerms cx))) (Just (ErrorCore.InvalidTermErrorTermVariableShadowing (ErrorCore.TermVariableShadowingError {
            ErrorCore.termVariableShadowingErrorLocation = path,
            ErrorCore.termVariableShadowingErrorName = paramName}))) Nothing)) Nothing,
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.invalidLambdaParameterName")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.invalidLambdaParameterName", f)) (Logic.ifElse (isValidName paramName) Nothing (Just (ErrorCore.InvalidTermErrorInvalidLambdaParameterName (ErrorCore.InvalidLambdaParameterNameError {
            ErrorCore.invalidLambdaParameterNameErrorLocation = path,
            ErrorCore.invalidLambdaParameterNameErrorName = paramName}))))) Nothing),
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.undefinedTypeVariableInLambdaDomain")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.undefinedTypeVariableInLambdaDomain", f)) (Logic.ifElse typed (Maybes.cases (Core.lambdaDomain v0) Nothing (\dom -> checkUndefinedTypeVariablesInType path cx dom (\uvName -> Just (ErrorCore.InvalidTermErrorUndefinedTypeVariableInLambdaDomain (ErrorCore.UndefinedTypeVariableInLambdaDomainError {
            ErrorCore.undefinedTypeVariableInLambdaDomainErrorLocation = path,
            ErrorCore.undefinedTypeVariableInLambdaDomainErrorName = uvName}))))) Nothing)) Nothing)])
      Core.TermProject v0 ->
        let tname = Core.projectionTypeName v0
        in (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.emptyTypeNameInTerm")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.emptyTypeNameInTerm", f)) (Logic.ifElse (Equality.equal (Core.unName tname) "") (Just (ErrorCore.InvalidTermErrorEmptyTypeNameInTerm (ErrorCore.EmptyTypeNameInTermError {
          ErrorCore.emptyTypeNameInTermErrorLocation = path}))) Nothing)) Nothing)
      Core.TermCases v0 ->
        let tname = Core.caseStatementTypeName v0
            csDefault = Core.caseStatementDefault v0
            csCases = Core.caseStatementCases v0
        in (firstFinding [
          Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.emptyTypeNameInTerm")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.emptyTypeNameInTerm", f)) (Logic.ifElse (Equality.equal (Core.unName tname) "") (Just (ErrorCore.InvalidTermErrorEmptyTypeNameInTerm (ErrorCore.EmptyTypeNameInTermError {
            ErrorCore.emptyTypeNameInTermErrorLocation = path}))) Nothing)) Nothing,
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.emptyCaseStatement")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.emptyCaseStatement", f)) (Logic.ifElse (Logic.and (Lists.null csCases) (Maybes.isNothing csDefault)) (Just (ErrorCore.InvalidTermErrorEmptyCaseStatement (ErrorCore.EmptyCaseStatementError {
            ErrorCore.emptyCaseStatementErrorLocation = path,
            ErrorCore.emptyCaseStatementErrorTypeName = tname}))) Nothing)) Nothing),
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.duplicateField")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.duplicateField", f)) (checkDuplicateFields path (Lists.map Core.fieldName csCases))) Nothing)])
      Core.TermTypeApplication v0 -> Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.undefinedTypeVariableInTypeApplication")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.undefinedTypeVariableInTypeApplication", f)) (Logic.ifElse typed (checkUndefinedTypeVariablesInType path cx (Core.typeApplicationTermType v0) (\uvName -> Just (ErrorCore.InvalidTermErrorUndefinedTypeVariableInTypeApplication (ErrorCore.UndefinedTypeVariableInTypeApplicationError {
        ErrorCore.undefinedTypeVariableInTypeApplicationErrorLocation = path,
        ErrorCore.undefinedTypeVariableInTypeApplicationErrorName = uvName})))) Nothing)) Nothing
      Core.TermTypeLambda v0 ->
        let tvName = Core.typeLambdaParameter v0
        in (firstFinding [
          Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.typeVariableShadowingInTypeLambda")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.typeVariableShadowingInTypeLambda", f)) (Logic.ifElse (Sets.member tvName (Sets.delete tvName (Graph.graphTypeVariables cx))) (Just (ErrorCore.InvalidTermErrorTypeVariableShadowingInTypeLambda (ErrorCore.TypeVariableShadowingInTypeLambdaError {
            ErrorCore.typeVariableShadowingInTypeLambdaErrorLocation = path,
            ErrorCore.typeVariableShadowingInTypeLambdaErrorName = tvName}))) Nothing)) Nothing,
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.invalidTypeLambdaParameterName")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.invalidTypeLambdaParameterName", f)) (Logic.ifElse (isValidName tvName) Nothing (Just (ErrorCore.InvalidTermErrorInvalidTypeLambdaParameterName (ErrorCore.InvalidTypeLambdaParameterNameError {
            ErrorCore.invalidTypeLambdaParameterNameErrorLocation = path,
            ErrorCore.invalidTypeLambdaParameterNameErrorName = tvName}))))) Nothing)])
      Core.TermVariable v0 -> Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.undefinedTermVariable")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.undefinedTermVariable", f)) (Logic.ifElse (Logic.or (Maybes.isJust (Maps.lookup v0 (Graph.graphBoundTerms cx))) (Logic.or (Sets.member v0 (Graph.graphLambdaVariables cx)) (Maybes.isJust (Maps.lookup v0 (Graph.graphPrimitives cx))))) Nothing (Just (ErrorCore.InvalidTermErrorUndefinedTermVariable (ErrorCore.UndefinedTermVariableError {
        ErrorCore.undefinedTermVariableErrorLocation = path,
        ErrorCore.undefinedTermVariableErrorName = v0}))))) Nothing
      Core.TermWrap v0 ->
        let tname = Core.wrappedTermTypeName v0
        in (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTermError.emptyTypeNameInTerm")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTermError.emptyTypeNameInTerm", f)) (Logic.ifElse (Equality.equal (Core.unName tname) "") (Just (ErrorCore.InvalidTermErrorEmptyTypeNameInTerm (ErrorCore.EmptyTypeNameInTermError {
          ErrorCore.emptyTypeNameInTermErrorLocation = path}))) Nothing)) Nothing)
      _ -> Nothing
-- | Check a type for type variables not bound in the current scope
checkUndefinedTypeVariablesInType :: t0 -> Graph.Graph -> Core.Type -> (Core.Name -> Maybe t1) -> Maybe t1
checkUndefinedTypeVariablesInType path cx typ mkError =

      let freeVars = Variables.freeVariablesInType typ
          undefined = Sets.difference freeVars (Graph.graphTypeVariables cx)
      in (Maybes.maybe Nothing (\firstUndefined -> mkError firstUndefined) (Lists.maybeHead (Sets.toList undefined)))
-- | Check a type scheme for type variables not bound by the scheme or the current scope
checkUndefinedTypeVariablesInTypeScheme :: t0 -> Graph.Graph -> Core.TypeScheme -> (Core.Name -> Maybe t1) -> Maybe t1
checkUndefinedTypeVariablesInTypeScheme path cx ts mkError =

      let freeVars = Variables.freeVariablesInTypeScheme ts
          undefined = Sets.difference freeVars (Graph.graphTypeVariables cx)
      in (Maybes.maybe Nothing (\firstUndefined -> mkError firstUndefined) (Lists.maybeHead (Sets.toList undefined)))
-- | Return an error if the given type is TypeVoid
checkVoid :: Core.Type -> Maybe ErrorCore.InvalidTypeError
checkVoid typ =
    case typ of
      Core.TypeVoid -> Just (ErrorCore.InvalidTypeErrorVoidInNonBottomPosition (ErrorCore.VoidInNonBottomPositionError {
        ErrorCore.voidInNonBottomPositionErrorLocation = (Paths.SubtermPath [])}))
      _ -> Nothing
-- | True iff the given rule name appears in the profile's errorRules or warningRules.
enabled :: Validation.ValidationProfile -> Core.Name -> Bool
enabled p ruleName =
    Logic.or (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Sets.member ruleName (Validation.validationProfileWarningRules p))
-- | Find the first duplicate name in a list
findDuplicate :: Ord t0 => ([t0] -> Maybe t0)
findDuplicate names =

      let result =
              Lists.foldl (\acc -> \name ->
                let seen = Pairs.first acc
                    dup = Pairs.second acc
                in (Maybes.cases dup (Logic.ifElse (Sets.member name seen) (seen, (Just name)) (Sets.insert name seen, Nothing)) (\_ -> acc))) (Sets.empty, Nothing) names
      in (Pairs.second result)
-- | Find the first duplicate name in a list (for field type validation)
findDuplicateFieldType :: Ord t0 => ([t0] -> Maybe t0)
findDuplicateFieldType names =

      let result =
              Lists.foldl (\acc -> \name ->
                let seen = Pairs.first acc
                    dup = Pairs.second acc
                in (Maybes.cases dup (Logic.ifElse (Sets.member name seen) (seen, (Just name)) (Sets.insert name seen, Nothing)) (\_ -> acc))) (Sets.empty, Nothing) names
      in (Pairs.second result)
-- | Return the first error from a list of optional errors, or nothing if all are valid
firstError :: [Maybe t0] -> Maybe t0
firstError checks = Lists.foldl (\acc -> \check -> Maybes.cases acc check (\_ -> acc)) Nothing checks
-- | Return the first rule-tagged finding from a list, or nothing if all are valid
firstFinding :: [Maybe t0] -> Maybe t0
firstFinding checks = Lists.foldl (\acc -> \check -> Maybes.cases acc check (\_ -> acc)) Nothing checks
-- | Return the first rule-tagged type finding from a list, or nothing if all are valid
firstFindingType :: [Maybe t0] -> Maybe t0
firstFindingType checks = Lists.foldl (\acc -> \check -> Maybes.cases acc check (\_ -> acc)) Nothing checks
-- | Return the first type error from a list of optional errors, or nothing if all are valid
firstTypeError :: [Maybe t0] -> Maybe t0
firstTypeError checks = Lists.foldl (\acc -> \check -> Maybes.cases acc check (\_ -> acc)) Nothing checks
-- | Check whether a name is valid at an introduction site. Currently rejects empty strings.
isValidName :: Core.Name -> Bool
isValidName name = Logic.not (Equality.equal (Core.unName name) "")
-- | The default validation profile for term and type validation, with every check classified as an error except InvalidTypeError.singleVariantUnion (warning); maxErrors=1, maxWarnings=20.
kernelDefaultCoreProfile :: Validation.ValidationProfile
kernelDefaultCoreProfile =
    Validation.ValidationProfile {
      Validation.validationProfileErrorRules = (Sets.fromList [
        Core.Name "hydra.error.core.InvalidTermError.constantCondition",
        (Core.Name "hydra.error.core.InvalidTermError.duplicateBinding"),
        (Core.Name "hydra.error.core.InvalidTermError.duplicateField"),
        (Core.Name "hydra.error.core.InvalidTermError.emptyCaseStatement"),
        (Core.Name "hydra.error.core.InvalidTermError.emptyLetBindings"),
        (Core.Name "hydra.error.core.InvalidTermError.emptyTermAnnotation"),
        (Core.Name "hydra.error.core.InvalidTermError.emptyTypeNameInTerm"),
        (Core.Name "hydra.error.core.InvalidTermError.invalidLambdaParameterName"),
        (Core.Name "hydra.error.core.InvalidTermError.invalidLetBindingName"),
        (Core.Name "hydra.error.core.InvalidTermError.invalidTypeLambdaParameterName"),
        (Core.Name "hydra.error.core.InvalidTermError.nestedTermAnnotation"),
        (Core.Name "hydra.error.core.InvalidTermError.redundantWrapUnwrap"),
        (Core.Name "hydra.error.core.InvalidTermError.selfApplication"),
        (Core.Name "hydra.error.core.InvalidTermError.termVariableShadowing"),
        (Core.Name "hydra.error.core.InvalidTermError.typeVariableShadowingInTypeLambda"),
        (Core.Name "hydra.error.core.InvalidTermError.undefinedTermVariable"),
        (Core.Name "hydra.error.core.InvalidTermError.undefinedTypeVariableInBindingType"),
        (Core.Name "hydra.error.core.InvalidTermError.undefinedTypeVariableInLambdaDomain"),
        (Core.Name "hydra.error.core.InvalidTermError.undefinedTypeVariableInTypeApplication"),
        (Core.Name "hydra.error.core.InvalidTermError.unknownPrimitiveName"),
        (Core.Name "hydra.error.core.InvalidTermError.unnecessaryIdentityApplication"),
        (Core.Name "hydra.error.core.InvalidTermError.untypedTermVariable"),
        (Core.Name "hydra.error.core.InvalidTypeError.duplicateRecordTypeFieldNames"),
        (Core.Name "hydra.error.core.InvalidTypeError.duplicateUnionTypeFieldNames"),
        (Core.Name "hydra.error.core.InvalidTypeError.emptyRecordType"),
        (Core.Name "hydra.error.core.InvalidTypeError.emptyTypeAnnotation"),
        (Core.Name "hydra.error.core.InvalidTypeError.emptyUnionType"),
        (Core.Name "hydra.error.core.InvalidTypeError.invalidForallParameterName"),
        (Core.Name "hydra.error.core.InvalidTypeError.invalidTypeSchemeVariableName"),
        (Core.Name "hydra.error.core.InvalidTypeError.nestedTypeAnnotation"),
        (Core.Name "hydra.error.core.InvalidTypeError.nonComparableMapKeyType"),
        (Core.Name "hydra.error.core.InvalidTypeError.nonComparableSetElementType"),
        (Core.Name "hydra.error.core.InvalidTypeError.typeVariableShadowingInForall"),
        (Core.Name "hydra.error.core.InvalidTypeError.undefinedTypeVariable"),
        (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")]),
      Validation.validationProfileWarningRules = (Sets.fromList [
        Core.Name "hydra.error.core.InvalidTypeError.singleVariantUnion"]),
      Validation.validationProfileMaxErrors = 1,
      Validation.validationProfileMaxWarnings = 20}
-- | Validate a term against the given ValidationProfile, returning a ValidationResult. Errors hard-stop traversal once maxErrors is reached; warnings are bounded by maxWarnings without causing termination.
term :: Validation.ValidationProfile -> Bool -> Graph.Graph -> Core.Term -> Validation.ValidationResult ErrorCore.InvalidTermError
term p typed g t =
    Rewriting.foldTermWithGraphAndPath (\recurse -> \path -> \cx -> \acc -> \trm -> Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc)) (Validation.validationProfileMaxErrors p)) acc (
      let acc1 = appendFinding p acc (checkTerm p typed (Paths.SubtermPath path) cx trm)
      in (Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc1)) (Validation.validationProfileMaxErrors p)) acc1 (recurse acc1 trm)))) g (Validation.ValidationResult {
      Validation.validationResultErrors = [],
      Validation.validationResultWarnings = []}) t
-- | Validate a type against the given ValidationProfile, threading a ValidationResult accumulator through subtypes. Errors hard-stop traversal once maxErrors is reached.
type_ :: Validation.ValidationProfile -> Validation.ValidationResult ErrorCore.InvalidTypeError -> S.Set Core.Name -> Core.Type -> Validation.ValidationResult ErrorCore.InvalidTypeError
type_ p acc boundVars typ =
    Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc)) (Validation.validationProfileMaxErrors p)) acc (
      let acc1 = appendFindingType p acc (validateTypeNode p boundVars typ)
      in (Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc1)) (Validation.validationProfileMaxErrors p)) acc1 (case typ of
        Core.TypeForall v0 ->
          let newBound = Sets.insert (Core.forallTypeParameter v0) boundVars
          in (type_ p acc1 newBound (Core.forallTypeBody v0))
        Core.TypeAnnotated v0 -> type_ p acc1 boundVars (Core.annotatedTypeBody v0)
        Core.TypeApplication v0 ->
          let acc2 = type_ p acc1 boundVars (Core.applicationTypeFunction v0)
          in (type_ p acc2 boundVars (Core.applicationTypeArgument v0))
        Core.TypeEither v0 ->
          let acc2 = type_ p acc1 boundVars (Core.eitherTypeLeft v0)
          in (type_ p acc2 boundVars (Core.eitherTypeRight v0))
        Core.TypeFunction v0 ->
          let acc2 = type_ p acc1 boundVars (Core.functionTypeDomain v0)
          in (type_ p acc2 boundVars (Core.functionTypeCodomain v0))
        Core.TypeList v0 -> type_ p acc1 boundVars v0
        Core.TypeMap v0 ->
          let acc2 = type_ p acc1 boundVars (Core.mapTypeKeys v0)
          in (type_ p acc2 boundVars (Core.mapTypeValues v0))
        Core.TypeMaybe v0 -> type_ p acc1 boundVars v0
        Core.TypePair v0 ->
          let acc2 = type_ p acc1 boundVars (Core.pairTypeFirst v0)
          in (type_ p acc2 boundVars (Core.pairTypeSecond v0))
        Core.TypeRecord v0 -> Lists.foldl (\a -> \f -> type_ p a boundVars (Core.fieldTypeType f)) acc1 v0
        Core.TypeSet v0 -> type_ p acc1 boundVars v0
        Core.TypeUnion v0 -> Lists.foldl (\a -> \f -> type_ p a boundVars (Core.fieldTypeType f)) acc1 v0
        Core.TypeWrap v0 -> type_ p acc1 boundVars v0
        _ -> acc1)))
-- | Check a single type node for validation errors. Rules disabled by the profile are not evaluated.
validateTypeNode :: Validation.ValidationProfile -> S.Set Core.Name -> Core.Type -> Maybe (Core.Name, ErrorCore.InvalidTypeError)
validateTypeNode p boundVars typ =
    case typ of
      Core.TypeAnnotated v0 ->
        let body = Core.annotatedTypeBody v0
            annMap = Core.annotatedTypeAnnotation v0
        in (firstFindingType [
          Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.emptyTypeAnnotation")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.emptyTypeAnnotation", f)) (Logic.ifElse (Maps.null annMap) (Just (ErrorCore.InvalidTypeErrorEmptyTypeAnnotation (ErrorCore.EmptyTypeAnnotationError {
            ErrorCore.emptyTypeAnnotationErrorLocation = (Paths.SubtermPath [])}))) Nothing)) Nothing,
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.nestedTypeAnnotation")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.nestedTypeAnnotation", f)) (case body of
            Core.TypeAnnotated _ -> Just (ErrorCore.InvalidTypeErrorNestedTypeAnnotation (ErrorCore.NestedTypeAnnotationError {
              ErrorCore.nestedTypeAnnotationErrorLocation = (Paths.SubtermPath [])}))
            _ -> Nothing)) Nothing)])
      Core.TypeEither v0 -> firstFindingType [
        Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition", f)) (checkVoid (Core.eitherTypeLeft v0))) Nothing,
        (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition", f)) (checkVoid (Core.eitherTypeRight v0))) Nothing)]
      Core.TypeForall v0 ->
        let paramName = Core.forallTypeParameter v0
        in (firstFindingType [
          Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.typeVariableShadowingInForall")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.typeVariableShadowingInForall", f)) (Logic.ifElse (Sets.member paramName boundVars) (Just (ErrorCore.InvalidTypeErrorTypeVariableShadowingInForall (ErrorCore.TypeVariableShadowingInForallError {
            ErrorCore.typeVariableShadowingInForallErrorLocation = (Paths.SubtermPath []),
            ErrorCore.typeVariableShadowingInForallErrorName = paramName}))) Nothing)) Nothing,
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.invalidForallParameterName")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.invalidForallParameterName", f)) (Logic.ifElse (isValidName paramName) Nothing (Just (ErrorCore.InvalidTypeErrorInvalidForallParameterName (ErrorCore.InvalidForallParameterNameError {
            ErrorCore.invalidForallParameterNameErrorLocation = (Paths.SubtermPath []),
            ErrorCore.invalidForallParameterNameErrorName = paramName}))))) Nothing)])
      Core.TypeFunction v0 -> Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition", f)) (checkVoid (Core.functionTypeCodomain v0))) Nothing
      Core.TypeList v0 -> Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition", f)) (checkVoid v0)) Nothing
      Core.TypeMap v0 ->
        let keyType = Core.mapTypeKeys v0
        in (firstFindingType [
          Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.nonComparableMapKeyType")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.nonComparableMapKeyType", f)) (case keyType of
            Core.TypeFunction _ -> Just (ErrorCore.InvalidTypeErrorNonComparableMapKeyType (ErrorCore.NonComparableMapKeyTypeError {
              ErrorCore.nonComparableMapKeyTypeErrorLocation = (Paths.SubtermPath []),
              ErrorCore.nonComparableMapKeyTypeErrorKeyType = keyType}))
            _ -> Nothing)) Nothing,
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition", f)) (checkVoid keyType)) Nothing),
          (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition", f)) (checkVoid (Core.mapTypeValues v0))) Nothing)])
      Core.TypePair v0 -> firstFindingType [
        Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition", f)) (checkVoid (Core.pairTypeFirst v0))) Nothing,
        (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition", f)) (checkVoid (Core.pairTypeSecond v0))) Nothing)]
      Core.TypeRecord v0 -> firstFindingType [
        Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.emptyRecordType")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.emptyRecordType", f)) (Logic.ifElse (Lists.null v0) (Just (ErrorCore.InvalidTypeErrorEmptyRecordType (ErrorCore.EmptyRecordTypeError {
          ErrorCore.emptyRecordTypeErrorLocation = (Paths.SubtermPath [])}))) Nothing)) Nothing,
        (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.duplicateRecordTypeFieldNames")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.duplicateRecordTypeFieldNames", f)) (checkDuplicateFieldTypes v0 (\dupName -> Just (ErrorCore.InvalidTypeErrorDuplicateRecordTypeFieldNames (ErrorCore.DuplicateRecordTypeFieldNamesError {
          ErrorCore.duplicateRecordTypeFieldNamesErrorLocation = (Paths.SubtermPath []),
          ErrorCore.duplicateRecordTypeFieldNamesErrorName = dupName}))))) Nothing),
        (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition", f)) (firstTypeError (Lists.map (\f -> checkVoid (Core.fieldTypeType f)) v0))) Nothing)]
      Core.TypeSet v0 -> firstFindingType [
        Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.nonComparableSetElementType")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.nonComparableSetElementType", f)) (case v0 of
          Core.TypeFunction _ -> Just (ErrorCore.InvalidTypeErrorNonComparableSetElementType (ErrorCore.NonComparableSetElementTypeError {
            ErrorCore.nonComparableSetElementTypeErrorLocation = (Paths.SubtermPath []),
            ErrorCore.nonComparableSetElementTypeErrorElementType = v0}))
          _ -> Nothing)) Nothing,
        (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition", f)) (checkVoid v0)) Nothing)]
      Core.TypeUnion v0 -> firstFindingType [
        Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.emptyUnionType")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.emptyUnionType", f)) (Logic.ifElse (Lists.null v0) (Just (ErrorCore.InvalidTypeErrorEmptyUnionType (ErrorCore.EmptyUnionTypeError {
          ErrorCore.emptyUnionTypeErrorLocation = (Paths.SubtermPath [])}))) Nothing)) Nothing,
        (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.singleVariantUnion")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.singleVariantUnion", f)) (Logic.ifElse (Equality.equal (Lists.length v0) 1) (Maybes.maybe Nothing (\singleField -> Just (ErrorCore.InvalidTypeErrorSingleVariantUnion (ErrorCore.SingleVariantUnionError {
          ErrorCore.singleVariantUnionErrorLocation = (Paths.SubtermPath []),
          ErrorCore.singleVariantUnionErrorFieldName = (Core.fieldTypeName singleField)}))) (Lists.maybeHead v0)) Nothing)) Nothing),
        (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.duplicateUnionTypeFieldNames")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.duplicateUnionTypeFieldNames", f)) (checkDuplicateFieldTypes v0 (\dupName -> Just (ErrorCore.InvalidTypeErrorDuplicateUnionTypeFieldNames (ErrorCore.DuplicateUnionTypeFieldNamesError {
          ErrorCore.duplicateUnionTypeFieldNamesErrorLocation = (Paths.SubtermPath []),
          ErrorCore.duplicateUnionTypeFieldNamesErrorName = dupName}))))) Nothing),
        (Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.voidInNonBottomPosition", f)) (firstTypeError (Lists.map (\f -> checkVoid (Core.fieldTypeType f)) v0))) Nothing)]
      Core.TypeVariable v0 -> Logic.ifElse (enabled p (Core.Name "hydra.error.core.InvalidTypeError.undefinedTypeVariable")) (Maybes.map (\f -> (Core.Name "hydra.error.core.InvalidTypeError.undefinedTypeVariable", f)) (Logic.ifElse (Sets.member v0 boundVars) Nothing (Just (ErrorCore.InvalidTypeErrorUndefinedTypeVariable (ErrorCore.UndefinedTypeVariableError {
        ErrorCore.undefinedTypeVariableErrorLocation = (Paths.SubtermPath []),
        ErrorCore.undefinedTypeVariableErrorName = v0}))))) Nothing
      _ -> Nothing
