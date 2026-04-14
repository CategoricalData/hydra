-- Note: this is an automatically generated file. Do not edit.

-- | Validation functions for core terms and types

module Hydra.Validate.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Paths as Paths
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Set as S

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

-- | Check if any name in a list shadows a variable already in scope
checkShadowing :: Paths.SubtermPath -> Graph.Graph -> [Core.Name] -> Maybe ErrorCore.InvalidTermError
checkShadowing path cx names =

      let result =
              Lists.foldl (\acc -> \name -> Maybes.cases acc (Logic.ifElse (Logic.or (Maybes.isJust (Maps.lookup name (Graph.graphBoundTerms cx))) (Sets.member name (Graph.graphLambdaVariables cx))) (Just (ErrorCore.InvalidTermErrorTermVariableShadowing (ErrorCore.TermVariableShadowingError {
                ErrorCore.termVariableShadowingErrorLocation = path,
                ErrorCore.termVariableShadowingErrorName = name}))) Nothing) (\_ -> acc)) Nothing names
      in result

-- | Check a single term node for validation errors
checkTerm :: Bool -> Paths.SubtermPath -> Graph.Graph -> Core.Term -> Maybe ErrorCore.InvalidTermError
checkTerm typed path cx term =
    case term of
      Core.TermAnnotated v0 ->
        let body = Core.annotatedTermBody v0
            annMap = Core.annotatedTermAnnotation v0
        in (firstError [
          Logic.ifElse (Maps.null annMap) (Just (ErrorCore.InvalidTermErrorEmptyTermAnnotation (ErrorCore.EmptyTermAnnotationError {
            ErrorCore.emptyTermAnnotationErrorLocation = path}))) Nothing,
          case body of
            Core.TermAnnotated _ -> Just (ErrorCore.InvalidTermErrorNestedTermAnnotation (ErrorCore.NestedTermAnnotationError {
              ErrorCore.nestedTermAnnotationErrorLocation = path}))
            _ -> Nothing])
      Core.TermApplication v0 ->
        let fun = Core.applicationFunction v0
            arg = Core.applicationArgument v0
        in (firstError [
          case fun of
            Core.TermVariable v1 -> Logic.ifElse (Equality.equal (Core.unName v1) "hydra.lib.logic.ifElse") (case arg of
              Core.TermLiteral v2 -> case v2 of
                Core.LiteralBoolean v3 -> Just (ErrorCore.InvalidTermErrorConstantCondition (ErrorCore.ConstantConditionError {
                  ErrorCore.constantConditionErrorLocation = path,
                  ErrorCore.constantConditionErrorValue = v3}))
                _ -> Nothing
              _ -> Nothing) Nothing
            _ -> Nothing,
          case fun of
            Core.TermVariable v1 -> case arg of
              Core.TermVariable v2 -> Logic.ifElse (Equality.equal v1 v2) (Just (ErrorCore.InvalidTermErrorSelfApplication (ErrorCore.SelfApplicationError {
                ErrorCore.selfApplicationErrorLocation = path,
                ErrorCore.selfApplicationErrorName = v1}))) Nothing
              _ -> Nothing
            _ -> Nothing,
          case fun of
            Core.TermLambda v1 ->
              let param = Core.lambdaParameter v1
                  body = Core.lambdaBody v1
              in case body of
                Core.TermVariable v2 -> Logic.ifElse (Equality.equal param v2) (Just (ErrorCore.InvalidTermErrorUnnecessaryIdentityApplication (ErrorCore.UnnecessaryIdentityApplicationError {
                  ErrorCore.unnecessaryIdentityApplicationErrorLocation = path}))) Nothing
                _ -> Nothing
            _ -> Nothing,
          case fun of
            Core.TermUnwrap v1 -> case arg of
              Core.TermWrap v2 ->
                let wrapName = Core.wrappedTermTypeName v2
                in (Logic.ifElse (Equality.equal v1 wrapName) (Just (ErrorCore.InvalidTermErrorRedundantWrapUnwrap (ErrorCore.RedundantWrapUnwrapError {
                  ErrorCore.redundantWrapUnwrapErrorLocation = path,
                  ErrorCore.redundantWrapUnwrapErrorTypeName = v1}))) Nothing)
              _ -> Nothing
            _ -> Nothing])
      Core.TermRecord v0 ->
        let tname = Core.recordTypeName v0
            flds = Core.recordFields v0
        in (firstError [
          Logic.ifElse (Equality.equal (Core.unName tname) "") (Just (ErrorCore.InvalidTermErrorEmptyTypeNameInTerm (ErrorCore.EmptyTypeNameInTermError {
            ErrorCore.emptyTypeNameInTermErrorLocation = path}))) Nothing,
          (checkDuplicateFields path (Lists.map Core.fieldName flds))])
      Core.TermLet v0 ->
        let bindings = Core.letBindings v0
            names = Lists.map Core.bindingName bindings
        in (firstError [
          Logic.ifElse (Lists.null bindings) (Just (ErrorCore.InvalidTermErrorEmptyLetBindings (ErrorCore.EmptyLetBindingsError {
            ErrorCore.emptyLetBindingsErrorLocation = path}))) Nothing,
          (checkDuplicateBindings path bindings),
          Nothing,
          (firstError (Lists.map (\bname -> Logic.ifElse (isValidName bname) Nothing (Just (ErrorCore.InvalidTermErrorInvalidLetBindingName (ErrorCore.InvalidLetBindingNameError {
            ErrorCore.invalidLetBindingNameErrorLocation = path,
            ErrorCore.invalidLetBindingNameErrorName = bname})))) names)),
          (Logic.ifElse typed (firstError (Lists.map (\b -> Maybes.cases (Core.bindingType b) Nothing (\ts -> checkUndefinedTypeVariablesInTypeScheme path cx ts (\uvName -> Just (ErrorCore.InvalidTermErrorUndefinedTypeVariableInBindingType (ErrorCore.UndefinedTypeVariableInBindingTypeError {
            ErrorCore.undefinedTypeVariableInBindingTypeErrorLocation = path,
            ErrorCore.undefinedTypeVariableInBindingTypeErrorName = uvName}))))) bindings)) Nothing)])
      Core.TermInject v0 ->
        let tname = Core.injectionTypeName v0
        in (Logic.ifElse (Equality.equal (Core.unName tname) "") (Just (ErrorCore.InvalidTermErrorEmptyTypeNameInTerm (ErrorCore.EmptyTypeNameInTermError {
          ErrorCore.emptyTypeNameInTermErrorLocation = path}))) Nothing)
      Core.TermLambda v0 ->
        let paramName = Core.lambdaParameter v0
        in (firstError [
          Logic.ifElse (Maybes.isJust (Maps.lookup paramName (Graph.graphBoundTerms cx))) (Just (ErrorCore.InvalidTermErrorTermVariableShadowing (ErrorCore.TermVariableShadowingError {
            ErrorCore.termVariableShadowingErrorLocation = path,
            ErrorCore.termVariableShadowingErrorName = paramName}))) Nothing,
          (Logic.ifElse (isValidName paramName) Nothing (Just (ErrorCore.InvalidTermErrorInvalidLambdaParameterName (ErrorCore.InvalidLambdaParameterNameError {
            ErrorCore.invalidLambdaParameterNameErrorLocation = path,
            ErrorCore.invalidLambdaParameterNameErrorName = paramName})))),
          (Logic.ifElse typed (Maybes.cases (Core.lambdaDomain v0) Nothing (\dom -> checkUndefinedTypeVariablesInType path cx dom (\uvName -> Just (ErrorCore.InvalidTermErrorUndefinedTypeVariableInLambdaDomain (ErrorCore.UndefinedTypeVariableInLambdaDomainError {
            ErrorCore.undefinedTypeVariableInLambdaDomainErrorLocation = path,
            ErrorCore.undefinedTypeVariableInLambdaDomainErrorName = uvName}))))) Nothing)])
      Core.TermProject v0 ->
        let tname = Core.projectionTypeName v0
        in (Logic.ifElse (Equality.equal (Core.unName tname) "") (Just (ErrorCore.InvalidTermErrorEmptyTypeNameInTerm (ErrorCore.EmptyTypeNameInTermError {
          ErrorCore.emptyTypeNameInTermErrorLocation = path}))) Nothing)
      Core.TermCases v0 ->
        let tname = Core.caseStatementTypeName v0
            csDefault = Core.caseStatementDefault v0
            csCases = Core.caseStatementCases v0
        in (firstError [
          Logic.ifElse (Equality.equal (Core.unName tname) "") (Just (ErrorCore.InvalidTermErrorEmptyTypeNameInTerm (ErrorCore.EmptyTypeNameInTermError {
            ErrorCore.emptyTypeNameInTermErrorLocation = path}))) Nothing,
          (Logic.ifElse (Logic.and (Lists.null csCases) (Maybes.isNothing csDefault)) (Just (ErrorCore.InvalidTermErrorEmptyCaseStatement (ErrorCore.EmptyCaseStatementError {
            ErrorCore.emptyCaseStatementErrorLocation = path,
            ErrorCore.emptyCaseStatementErrorTypeName = tname}))) Nothing),
          (checkDuplicateFields path (Lists.map Core.fieldName csCases))])
      Core.TermTypeApplication v0 -> Logic.ifElse typed (checkUndefinedTypeVariablesInType path cx (Core.typeApplicationTermType v0) (\uvName -> Just (ErrorCore.InvalidTermErrorUndefinedTypeVariableInTypeApplication (ErrorCore.UndefinedTypeVariableInTypeApplicationError {
        ErrorCore.undefinedTypeVariableInTypeApplicationErrorLocation = path,
        ErrorCore.undefinedTypeVariableInTypeApplicationErrorName = uvName})))) Nothing
      Core.TermTypeLambda v0 ->
        let tvName = Core.typeLambdaParameter v0
        in (firstError [
          Logic.ifElse (Sets.member tvName (Sets.delete tvName (Graph.graphTypeVariables cx))) (Just (ErrorCore.InvalidTermErrorTypeVariableShadowingInTypeLambda (ErrorCore.TypeVariableShadowingInTypeLambdaError {
            ErrorCore.typeVariableShadowingInTypeLambdaErrorLocation = path,
            ErrorCore.typeVariableShadowingInTypeLambdaErrorName = tvName}))) Nothing,
          (Logic.ifElse (isValidName tvName) Nothing (Just (ErrorCore.InvalidTermErrorInvalidTypeLambdaParameterName (ErrorCore.InvalidTypeLambdaParameterNameError {
            ErrorCore.invalidTypeLambdaParameterNameErrorLocation = path,
            ErrorCore.invalidTypeLambdaParameterNameErrorName = tvName}))))])
      Core.TermVariable v0 -> Logic.ifElse (Logic.or (Maybes.isJust (Maps.lookup v0 (Graph.graphBoundTerms cx))) (Logic.or (Sets.member v0 (Graph.graphLambdaVariables cx)) (Maybes.isJust (Maps.lookup v0 (Graph.graphPrimitives cx))))) Nothing (Just (ErrorCore.InvalidTermErrorUndefinedTermVariable (ErrorCore.UndefinedTermVariableError {
        ErrorCore.undefinedTermVariableErrorLocation = path,
        ErrorCore.undefinedTermVariableErrorName = v0})))
      Core.TermWrap v0 ->
        let tname = Core.wrappedTermTypeName v0
        in (Logic.ifElse (Equality.equal (Core.unName tname) "") (Just (ErrorCore.InvalidTermErrorEmptyTypeNameInTerm (ErrorCore.EmptyTypeNameInTermError {
          ErrorCore.emptyTypeNameInTermErrorLocation = path}))) Nothing)
      _ -> Nothing

-- | Check a type for type variables not bound in the current scope
checkUndefinedTypeVariablesInType :: t0 -> Graph.Graph -> Core.Type -> (Core.Name -> Maybe t1) -> Maybe t1
checkUndefinedTypeVariablesInType path cx typ mkError =

      let freeVars = Variables.freeVariablesInType typ
          undefined = Sets.difference freeVars (Graph.graphTypeVariables cx)
      in (Logic.ifElse (Sets.null undefined) Nothing (
        let firstUndefined = Lists.head (Sets.toList undefined)
        in (mkError firstUndefined)))

-- | Check a type scheme for type variables not bound by the scheme or the current scope
checkUndefinedTypeVariablesInTypeScheme :: t0 -> Graph.Graph -> Core.TypeScheme -> (Core.Name -> Maybe t1) -> Maybe t1
checkUndefinedTypeVariablesInTypeScheme path cx ts mkError =

      let freeVars = Variables.freeVariablesInTypeScheme ts
          undefined = Sets.difference freeVars (Graph.graphTypeVariables cx)
      in (Logic.ifElse (Sets.null undefined) Nothing (
        let firstUndefined = Lists.head (Sets.toList undefined)
        in (mkError firstUndefined)))

-- | Return an error if the given type is TypeVoid
checkVoid :: Core.Type -> Maybe ErrorCore.InvalidTypeError
checkVoid typ =
    case typ of
      Core.TypeVoid -> Just (ErrorCore.InvalidTypeErrorVoidInNonBottomPosition (ErrorCore.VoidInNonBottomPositionError {
        ErrorCore.voidInNonBottomPositionErrorLocation = (Paths.SubtermPath [])}))
      _ -> Nothing

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

-- | Return the first type error from a list of optional errors, or nothing if all are valid
firstTypeError :: [Maybe t0] -> Maybe t0
firstTypeError checks = Lists.foldl (\acc -> \check -> Maybes.cases acc check (\_ -> acc)) Nothing checks

-- | Check whether a name is valid at an introduction site. Currently rejects empty strings.
isValidName :: Core.Name -> Bool
isValidName name = Logic.not (Equality.equal (Core.unName name) "")

-- | Validate a term, returning the first error found or nothing if valid. The 'typed' parameter indicates whether to expect System F (typed) terms; when true, type variable binding checks and UntypedTermVariableError are active.
term :: Bool -> Graph.Graph -> Core.Term -> Maybe ErrorCore.InvalidTermError
term typed g t =
    Rewriting.foldTermWithGraphAndPath (\recurse -> \path -> \cx -> \acc -> \trm -> Maybes.cases acc (
      let checkResult = checkTerm typed (Paths.SubtermPath path) cx trm
      in (Maybes.cases checkResult (recurse Nothing trm) (\err -> Just err))) (\_ -> acc)) g Nothing t

-- | Validate a type, returning the first error found or nothing if valid. The first argument is the set of type variables already in scope.
type_ :: S.Set Core.Name -> Core.Type -> Maybe ErrorCore.InvalidTypeError
type_ boundVars typ =

      let checkResult = validateTypeNode boundVars typ
      in (Maybes.cases checkResult (case typ of
        Core.TypeForall v0 ->
          let newBound = Sets.insert (Core.forallTypeParameter v0) boundVars
          in (type_ newBound (Core.forallTypeBody v0))
        Core.TypeAnnotated v0 -> type_ boundVars (Core.annotatedTypeBody v0)
        Core.TypeApplication v0 -> firstTypeError [
          type_ boundVars (Core.applicationTypeFunction v0),
          (type_ boundVars (Core.applicationTypeArgument v0))]
        Core.TypeEither v0 -> firstTypeError [
          type_ boundVars (Core.eitherTypeLeft v0),
          (type_ boundVars (Core.eitherTypeRight v0))]
        Core.TypeFunction v0 -> firstTypeError [
          type_ boundVars (Core.functionTypeDomain v0),
          (type_ boundVars (Core.functionTypeCodomain v0))]
        Core.TypeList v0 -> type_ boundVars v0
        Core.TypeMap v0 -> firstTypeError [
          type_ boundVars (Core.mapTypeKeys v0),
          (type_ boundVars (Core.mapTypeValues v0))]
        Core.TypeMaybe v0 -> type_ boundVars v0
        Core.TypePair v0 -> firstTypeError [
          type_ boundVars (Core.pairTypeFirst v0),
          (type_ boundVars (Core.pairTypeSecond v0))]
        Core.TypeRecord v0 -> firstTypeError (Lists.map (\f -> type_ boundVars (Core.fieldTypeType f)) v0)
        Core.TypeSet v0 -> type_ boundVars v0
        Core.TypeUnion v0 -> firstTypeError (Lists.map (\f -> type_ boundVars (Core.fieldTypeType f)) v0)
        Core.TypeWrap v0 -> type_ boundVars v0
        _ -> Nothing) (\err -> Just err))

-- | Check a single type node for validation errors
validateTypeNode :: S.Set Core.Name -> Core.Type -> Maybe ErrorCore.InvalidTypeError
validateTypeNode boundVars typ =
    case typ of
      Core.TypeAnnotated v0 ->
        let body = Core.annotatedTypeBody v0
            annMap = Core.annotatedTypeAnnotation v0
        in (firstTypeError [
          Logic.ifElse (Maps.null annMap) (Just (ErrorCore.InvalidTypeErrorEmptyTypeAnnotation (ErrorCore.EmptyTypeAnnotationError {
            ErrorCore.emptyTypeAnnotationErrorLocation = (Paths.SubtermPath [])}))) Nothing,
          case body of
            Core.TypeAnnotated _ -> Just (ErrorCore.InvalidTypeErrorNestedTypeAnnotation (ErrorCore.NestedTypeAnnotationError {
              ErrorCore.nestedTypeAnnotationErrorLocation = (Paths.SubtermPath [])}))
            _ -> Nothing])
      Core.TypeEither v0 -> firstTypeError [
        checkVoid (Core.eitherTypeLeft v0),
        (checkVoid (Core.eitherTypeRight v0))]
      Core.TypeForall v0 ->
        let paramName = Core.forallTypeParameter v0
        in (firstTypeError [
          Logic.ifElse (Sets.member paramName boundVars) (Just (ErrorCore.InvalidTypeErrorTypeVariableShadowingInForall (ErrorCore.TypeVariableShadowingInForallError {
            ErrorCore.typeVariableShadowingInForallErrorLocation = (Paths.SubtermPath []),
            ErrorCore.typeVariableShadowingInForallErrorName = paramName}))) Nothing,
          (Logic.ifElse (isValidName paramName) Nothing (Just (ErrorCore.InvalidTypeErrorInvalidForallParameterName (ErrorCore.InvalidForallParameterNameError {
            ErrorCore.invalidForallParameterNameErrorLocation = (Paths.SubtermPath []),
            ErrorCore.invalidForallParameterNameErrorName = paramName}))))])
      Core.TypeFunction v0 -> checkVoid (Core.functionTypeCodomain v0)
      Core.TypeList v0 -> checkVoid v0
      Core.TypeMap v0 ->
        let keyType = Core.mapTypeKeys v0
        in (firstTypeError [
          case keyType of
            Core.TypeFunction _ -> Just (ErrorCore.InvalidTypeErrorNonComparableMapKeyType (ErrorCore.NonComparableMapKeyTypeError {
              ErrorCore.nonComparableMapKeyTypeErrorLocation = (Paths.SubtermPath []),
              ErrorCore.nonComparableMapKeyTypeErrorKeyType = keyType}))
            _ -> Nothing,
          (checkVoid keyType),
          (checkVoid (Core.mapTypeValues v0))])
      Core.TypePair v0 -> firstTypeError [
        checkVoid (Core.pairTypeFirst v0),
        (checkVoid (Core.pairTypeSecond v0))]
      Core.TypeRecord v0 -> firstTypeError [
        Logic.ifElse (Lists.null v0) (Just (ErrorCore.InvalidTypeErrorEmptyRecordType (ErrorCore.EmptyRecordTypeError {
          ErrorCore.emptyRecordTypeErrorLocation = (Paths.SubtermPath [])}))) Nothing,
        (checkDuplicateFieldTypes v0 (\dupName -> Just (ErrorCore.InvalidTypeErrorDuplicateRecordTypeFieldNames (ErrorCore.DuplicateRecordTypeFieldNamesError {
          ErrorCore.duplicateRecordTypeFieldNamesErrorLocation = (Paths.SubtermPath []),
          ErrorCore.duplicateRecordTypeFieldNamesErrorName = dupName})))),
        (firstTypeError (Lists.map (\f -> checkVoid (Core.fieldTypeType f)) v0))]
      Core.TypeSet v0 -> firstTypeError [
        case v0 of
          Core.TypeFunction _ -> Just (ErrorCore.InvalidTypeErrorNonComparableSetElementType (ErrorCore.NonComparableSetElementTypeError {
            ErrorCore.nonComparableSetElementTypeErrorLocation = (Paths.SubtermPath []),
            ErrorCore.nonComparableSetElementTypeErrorElementType = v0}))
          _ -> Nothing,
        (checkVoid v0)]
      Core.TypeUnion v0 -> firstTypeError [
        Logic.ifElse (Lists.null v0) (Just (ErrorCore.InvalidTypeErrorEmptyUnionType (ErrorCore.EmptyUnionTypeError {
          ErrorCore.emptyUnionTypeErrorLocation = (Paths.SubtermPath [])}))) Nothing,
        (Logic.ifElse (Equality.equal (Lists.length v0) 1) (
          let singleField = Lists.head v0
          in (Just (ErrorCore.InvalidTypeErrorSingleVariantUnion (ErrorCore.SingleVariantUnionError {
            ErrorCore.singleVariantUnionErrorLocation = (Paths.SubtermPath []),
            ErrorCore.singleVariantUnionErrorFieldName = (Core.fieldTypeName singleField)})))) Nothing),
        (checkDuplicateFieldTypes v0 (\dupName -> Just (ErrorCore.InvalidTypeErrorDuplicateUnionTypeFieldNames (ErrorCore.DuplicateUnionTypeFieldNamesError {
          ErrorCore.duplicateUnionTypeFieldNamesErrorLocation = (Paths.SubtermPath []),
          ErrorCore.duplicateUnionTypeFieldNamesErrorName = dupName})))),
        (firstTypeError (Lists.map (\f -> checkVoid (Core.fieldTypeType f)) v0))]
      Core.TypeVariable v0 -> Logic.ifElse (Sets.member v0 boundVars) Nothing (Just (ErrorCore.InvalidTypeErrorUndefinedTypeVariable (ErrorCore.UndefinedTypeVariableError {
        ErrorCore.undefinedTypeVariableErrorLocation = (Paths.SubtermPath []),
        ErrorCore.undefinedTypeVariableErrorName = v0})))
      _ -> Nothing
