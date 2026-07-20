module Hydra.Sources.Gql.OpenGql where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel hiding (parameterName)
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "openGql.grammar"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A GQL model based on the OpenGQL ANTLR grammar, version 15b256b (2024-09-05), available at:"
    ++ " https://github.com/opengql/grammar/blob/main/GQL.g4"))}
  where
    -- Alphabetical order by local type name, per the definition-ordering style guide
    -- (Validate.Packaging.checkDefinitionOrdering has no section-boundary awareness).
    definitions = [
      abbreviatedEdgePattern,
      absoluteCatalogSchemaReference,
      absoluteDirectoryAndSchema,
      absoluteDirectoryPath,
      absoluteValueExpression,
      addSubNumericValueExpression,
      addSubtractExpr,
      addSubtractOperator,
      aggregateFunction,
      aggregatingValueExpression,
      allDifferentPredicate,
      allParametersOrCharacteristics,
      allPathSearch,
      allShortestPathSearch,
      ambientLinearDataModifyingStatement,
      ambientLinearDataModifyingStatementBody,
      ambientLinearQueryStatement,
      ambientLinearQueryStatementSimpleAndPrimitiveResult,
      anyPathSearch,
      anyRecordType,
      anyShortestPathSearch,
      approximateNumericLiteral,
      approximateNumericType,
      arcTypePointingLeft,
      arcTypePointingRight,
      arcTypeUndirected,
      atSchemaClause,
      bigIntType,
      bigIntegerType,
      binaryExactNumericType,
      binarySetFunction,
      binarySetFunctionType,
      binaryType,
      bindingEqualsValue,
      bindingTableExpression,
      bindingTableInitializer,
      bindingTableName,
      bindingTableReference,
      bindingTableReferenceValueType,
      bindingTableType,
      bindingTableVariableDefinition,
      bindingVariable,
      bindingVariableDefinition,
      bindingVariableDefinitionBlock,
      bindingVariableReference,
      bindingVariableReferenceList,
      booleanLiteral,
      booleanType,
      booleanValueExpression,
      byteLengthExpression,
      byteStringLiteral,
      byteStringType,
      byteStringValueExpression,
      bytesType,
      callCatalogModifyingProcedureStatement,
      callDataModifyingProcedureStatement,
      callProcedureStatement,
      callQueryStatement,
      cardinalityArgumentExpression,
      cardinalityExpression,
      case_,
      caseAbbreviation,
      caseExpression,
      caseOperand,
      caseSpecification,
      castOperand,
      castSpecification,
      castTarget,
      catalogGraphParentAndName,
      catalogGraphTypeParentAndName,
      catalogObjectParentReference,
      catalogProcedureParentAndName,
      catalogSchemaParentAndName,
      ceilingFunction,
      charLengthExpression,
      charType,
      characterOrByteStringFunction,
      characterStringLiteral,
      characterStringType,
      characterStringValueExpression,
      closedDynamicUnionTypeAlt1,
      closedDynamicUnionTypeAlt2,
      closedEdgeReferenceValueType,
      closedGraphReferenceValueType,
      closedNodeReferenceValueType,
      commitCommand,
      commonLogarithm,
      compOp,
      comparisonExpr,
      comparisonPredicatePart2,
      compositeQueryExpression,
      compositeQueryExpressionConjunction,
      compositeQueryPrimary,
      compositeQueryStatement,
      concatenationExpr,
      conjunctionLabelExpression,
      conjunctiveExpr,
      connectorPointingRight,
      connectorUndirected,
      copyOfGraphType,
      countedShortestGroupSearch,
      countedShortestPathSearch,
      createGraphOption,
      createGraphStatement,
      createGraphTypeOption,
      createGraphTypeStatement,
      createSchemaStatement,
      currentGraph,
      dateFunction,
      dateFunctionParameters,
      dateLiteral,
      dateString,
      dateType,
      datetimeFunction,
      datetimeFunctionParameters,
      datetimeLiteral,
      datetimeString,
      datetimeSubtraction,
      datetimeSubtractionParameters,
      datetimeType,
      datetimeValueExpression,
      datetimeValueExpression1,
      datetimeValueExpression2,
      datetimeValueFunction,
      decimalExactNumericType,
      deleteItem,
      deleteItemList,
      deleteStatement,
      delimitedBindingTableName,
      delimitedGraphName,
      dependentValueExpression,
      destinationNodeTypeAlias,
      destinationNodeTypeReference,
      destinationPredicate,
      detachOption,
      differentEdgesMatchMode,
      directedPredicate,
      directedPredicatePart2,
      directoryName,
      disjunctionLabelExpression,
      disjunctiveExpr,
      disjunctiveOperator,
      doubleTypeWithPrecision,
      dropGraphStatement,
      dropGraphTypeStatement,
      dropSchemaStatement,
      durationFunction,
      durationFunctionParameters,
      durationLiteral,
      durationString,
      durationValueExpression,
      durationValueFunction,
      dynamicParameterSpecification,
      dynamicPropertyValueType,
      edgeBindingsOrEdges,
      edgeKeyLabelSetWithContent,
      edgeKind,
      edgeKindAndSynonym,
      edgeLabelSetWithProperties,
      edgePattern,
      edgeReference,
      edgeReferenceValueExpression,
      edgeReferenceValueType,
      edgeSynonym,
      edgeTypeFiller,
      edgeTypeImpliedContent,
      edgeTypeKeyLabelSet,
      edgeTypeLabelSet,
      edgeTypeName,
      edgeTypeNameWithFiller,
      edgeTypePattern,
      edgeTypePatternDirected,
      edgeTypePatternPointingLeft,
      edgeTypePatternPointingRight,
      edgeTypePatternType,
      edgeTypePatternUndirected,
      edgeTypePhrase,
      edgeTypePhraseFiller,
      edgeTypePropertyTypes,
      edgeTypeSpecification,
      edgesSynonym,
      elementBindingsOrElements,
      elementIdFunction,
      elementPattern,
      elementPatternFiller,
      elementPatternPredicate,
      elementPatternWhereClause,
      elementPropertySpecification,
      elementTypeList,
      elementTypeSpecification,
      elementVariable,
      elementVariableDeclaration,
      elementVariableReference,
      elementsFunction,
      elseClause,
      emptyType,
      endTransactionCommand,
      endpointPair,
      endpointPairDirected,
      endpointPairPhrase,
      endpointPairPointingLeft,
      endpointPairPointingRight,
      endpointPairUndirected,
      exactNumericLiteral,
      exactNumericType,
      existsPredicate,
      exponentialFunction,
      field,
      fieldList,
      fieldName_,
      fieldType,
      fieldTypeList,
      fieldTypesSpecification,
      fieldsSpecification,
      filterStatement,
      fixedLength,
      fixedQuantifier,
      float128Type,
      float16Type,
      float256Type,
      float32Type,
      float64Type,
      floatTypeWithPrecision,
      floorFunction,
      focusedLinearDataModifyingStatement,
      focusedLinearDataModifyingStatementBody,
      focusedLinearQueryAndPrimitiveResultStatementPart,
      focusedLinearQueryStatement,
      focusedLinearQueryStatementPart,
      focusedLinearQueryStatementPartsAndResult,
      focusedNestedDataModifyingProcedureSpecification,
      focusedNestedQuerySpecification,
      focusedPrimitiveResultStatement,
      foldCharacterString,
      forItem,
      forItemAlias,
      forItemSource,
      forOrdinalityOrOffset,
      forStatement,
      fullEdgeAnyDirection,
      fullEdgeLeftOrRight,
      fullEdgeLeftOrUndirected,
      fullEdgePattern,
      fullEdgePointingLeft,
      fullEdgePointingRight,
      fullEdgeUndirected,
      fullEdgeUndirectedOrRight,
      generalLiteral,
      generalLogarithmArgument,
      generalLogarithmBase,
      generalLogarithmFunction,
      generalQuantifier,
      generalSetFunction,
      generalSetFunctionType,
      generalValueSpecification,
      gqlProgram,
      graphAndNestedQuerySpecification,
      graphExpression,
      graphInitializer,
      graphName,
      graphPattern,
      graphPatternBindingTable,
      graphPatternQuantifier,
      graphPatternWhereClause,
      graphPatternYieldClause,
      graphPatternYieldItem,
      graphPatternYieldItemList,
      graphReference,
      graphReferenceValueType,
      graphSource,
      graphTypeLikeGraph,
      graphTypeName,
      graphTypeOption,
      graphTypeReference,
      graphTypeSource,
      graphTypeSpecificationBody,
      graphVariableDefinition,
      groupByClause,
      groupingElement,
      groupingElementList,
      havingClause,
      homeGraph,
      identifier,
      immaterialValueType,
      implies,
      independentValueExpression,
      inlineProcedureCall,
      insertEdgeAndNode,
      insertEdgePattern,
      insertEdgePointingLeft,
      insertEdgePointingRight,
      insertEdgeUndirected,
      insertElementPatternFiller,
      insertGraphPattern,
      insertNodePattern,
      insertPathPattern,
      insertPathPatternList,
      insertStatement,
      int128Type,
      int16Type,
      int256Type,
      int32Type,
      int64Type,
      int8Type,
      intWithPrecision,
      integer128Type,
      integer16Type,
      integer256Type,
      integer32Type,
      integer64Type,
      integer8Type,
      integerWithPrecision,
      isLabelExpression,
      isLabeledOrColon,
      isNotExpr,
      isOrColon,
      isOrColonWithLabels,
      keepClause,
      labelAndPropertySetSpecification,
      labelExpression,
      labelName,
      labelSetPhrase,
      labelSetSpecification,
      labeledPredicate,
      labeledPredicatePart2,
      lengthExpression,
      letStatement,
      letValueExpression,
      letVariableDefinition,
      letVariableDefinitionList,
      limitClause,
      linearCatalogModifyingStatement,
      linearDataModifyingStatement,
      linearQueryStatement,
      listElement,
      listElementList,
      listLiteral,
      listValueConstructor,
      listValueConstructorByEnumeration,
      listValueExpression,
      listValueFunction,
      listValueTypeAlt1,
      listValueTypeAlt2,
      listValueTypeAlt3,
      listValueTypeName,
      listValueTypeNameSynonym,
      localDatetimeType,
      localDatetimeTypeChoice,
      localNodeTypeAlias,
      localTimeType,
      localTimeTypeChoice,
      localdatetimeFunction,
      localtimeFunction,
      lowerBound,
      matchMode,
      matchStatement,
      matchStatementBlock,
      maxLength,
      minLength,
      modulusExpression,
      mulDivNumericValueExpression,
      multDivExpr,
      multDivOperator,
      namedProcedureCall,
      naturalLogarithm,
      nestedBindingTableQuerySpecification,
      nestedDataModifyingProcedureSpecification,
      nestedGraphTypeSpecification,
      nestedProcedureSpecification,
      nestedQuerySpecification,
      nextStatement,
      nodeKeyLabelSetWithContent,
      nodeLabelSetWithProperties,
      nodePattern,
      nodeReference,
      nodeReferenceValueExpression,
      nodeReferenceValueType,
      nodeSynonym,
      nodeSynonymAndTypeName,
      nodeTypeFiller,
      nodeTypeImpliedContent,
      nodeTypeKeyLabelSet,
      nodeTypeLabelSet,
      nodeTypeName,
      nodeTypeNameWithFiller,
      nodeTypePattern,
      nodeTypePhrase,
      nodeTypePhraseFiller,
      nodeTypePropertyTypes,
      nodeTypeSpecification,
      nonNegativeIntegerSpecification,
      nonParenthesizedPrimaryValueExpression,
      nonParenthesizedPrimaryValueExpressionSpecialCase,
      normalForm,
      normalizeCharacterString,
      normalizedPredicateExpr,
      normalizedPredicatePart2,
      notExpr,
      notNull,
      nullIfAbbreviation,
      nullLiteral,
      nullOrdering,
      nullPredicate,
      nullPredicatePart2,
      nullType,
      numberOfGroups,
      numberOfPaths,
      numericType,
      numericValueExpression,
      numericValueExpressionBase,
      numericValueExpressionDividend,
      numericValueExpressionDivisor,
      numericValueExpressionExponent,
      numericValueFunction,
      objectExpressionPrimary,
      objectName,
      objectNameOrBindingVariable,
      ofGraphType,
      offsetAndOptionalLimit,
      offsetClause,
      offsetSynonym,
      openDynamicUnionType,
      openEdgeReferenceValueType,
      openGraphReferenceValueType,
      openGraphType,
      openNodeReferenceValueType,
      optTypedBindingTableInitializer,
      optTypedGraphInitializer,
      optTypedValueInitializer,
      optionalMatchStatement,
      optionalOperand,
      orderByAndOptionalOffsetAndLimit,
      orderByAndPageStatement,
      orderByClause,
      orderingSpecification,
      ordinalityOrOffsetType,
      parameterName,
      parameterSessionSpecification,
      parametersOrCharacteristics,
      parentAndGraphName,
      parentAndTableName,
      parenthesizedPathPatternExpression,
      parenthesizedPathPatternWhereClause,
      parenthesizedValueExpression,
      pathElementList,
      pathElementListStart,
      pathElementListStep,
      pathFactor,
      pathLengthExpression,
      pathMode,
      pathModePrefix,
      pathOrPaths,
      pathPattern,
      pathPatternExpression,
      pathPatternList,
      pathPatternPrefix,
      pathPrimary,
      pathSearchPrefix,
      pathTerm,
      pathValueConstructor,
      pathValueConstructorByEnumeration,
      pathValueExpression,
      pathValueType,
      pathVariable,
      pathVariableDeclaration,
      pathVariableReference,
      powerFunction,
      precision,
      precisionAndScale,
      predefinedSchemaReference,
      predefinedType,
      predicate,
      primaryValueExpression,
      primitiveCatalogModifyingStatement,
      primitiveDataModifyingStatement,
      primitiveQueryStatement,
      primitiveResultStatement,
      procedureAndMaybeEnd,
      procedureArgument,
      procedureArgumentList,
      procedureBody,
      procedureCall,
      procedureName,
      procedureReference,
      procedureSpecification,
      programActivity,
      propertyExistsPredicate,
      propertyKeyValuePair,
      propertyKeyValuePairList,
      propertyName,
      propertyReference,
      propertyType,
      propertyTypeList,
      propertyTypesSpecification,
      propertyValueType,
      quantifiedPathPrimary,
      queryConjunction,
      questionedPathPrimary,
      realType,
      recordConstructor,
      recordLiteral,
      recordType,
      referenceParameterSpecification,
      referenceValueType,
      regularIdentifier,
      relativeCatalogSchemaReference,
      relativeDirectoryAndSchema,
      relativeDirectoryPath,
      removeItem,
      removeItemList,
      removeLabelItem,
      removePropertyItem,
      removeStatement,
      repeatableElementsMatchMode,
      result,
      resultExpression,
      returnAndOptionalOrderByAndPage,
      returnItem,
      returnItemAlias,
      returnItemList,
      returnItems,
      returnItemsAndGroupBy,
      returnStatement,
      returnStatementBody,
      rollbackCommand,
      samePredicate,
      scale,
      schemaAndObjects,
      schemaName,
      schemaReference,
      searchCondition,
      searchedCase,
      searchedWhenClause,
      selectGraphMatch,
      selectGraphMatchList,
      selectItem,
      selectItemAlias,
      selectItemList,
      selectItems,
      selectQuerySpecification,
      selectStatement,
      selectStatementBody,
      selectStatementBodyAndClauses,
      sessionActivity,
      sessionCloseCommand,
      sessionParameterSpecification,
      sessionResetArguments,
      sessionResetCommand,
      sessionSetAndResetCommands,
      sessionSetBindingTableParameterClause,
      sessionSetCommand,
      sessionSetGraphClause,
      sessionSetGraphParameterClause,
      sessionSetParameterClause,
      sessionSetParameterName,
      sessionSetSchemaClause,
      sessionSetTimeZoneClause,
      sessionSetValueParameterClause,
      setAllPropertiesItem,
      setItem,
      setItemList,
      setLabelItem,
      setOperator,
      setOperatorType,
      setPropertyItem,
      setQuantifier,
      setStatement,
      setTimeZoneValue,
      shortestPathSearch,
      side,
      sign,
      signedBinaryExactNumericType,
      signedExpr,
      signedNumericValueExpression,
      signedVerboseBinaryExactNumericType,
      simpleCase,
      simpleCatalogModifyingStatement,
      simpleDataAccessingStatement,
      simpleDataModifyingStatement,
      simpleDirectoryPath,
      simpleLinearDataAccessingStatement,
      simpleLinearQueryStatement,
      simpleMatchStatement,
      simpleQueryStatement,
      simpleWhenClause,
      simplifiedConcatenation,
      simplifiedConjunction,
      simplifiedContents,
      simplifiedDefaultingAnyDirection,
      simplifiedDefaultingLeft,
      simplifiedDefaultingLeftOrRight,
      simplifiedDefaultingLeftOrUndirected,
      simplifiedDefaultingRight,
      simplifiedDefaultingUndirected,
      simplifiedDefaultingUndirectedOrRight,
      simplifiedDirectionOverride,
      simplifiedFactorHigh,
      simplifiedFactorLow,
      simplifiedMultisetAlternation,
      simplifiedNegation,
      simplifiedOverrideAnyDirection,
      simplifiedOverrideLeft,
      simplifiedOverrideLeftOrRight,
      simplifiedOverrideLeftOrUndirected,
      simplifiedOverrideRight,
      simplifiedOverrideUndirected,
      simplifiedOverrideUndirectedOrRight,
      simplifiedPathPatternExpression,
      simplifiedPathUnion,
      simplifiedPrimary,
      simplifiedQuantified,
      simplifiedQuestioned,
      simplifiedSecondary,
      simplifiedTerm,
      simplifiedTertiary,
      smallIntType,
      smallIntegerType,
      sortKey,
      sortSpecification,
      sortSpecificationList,
      sourceDestinationPredicate,
      sourceNodeTypeAlias,
      sourceNodeTypeReference,
      sourcePredicate,
      specifiedRecordType,
      squareRoot,
      startAndMaybeProcedureAndMaybeEnd,
      startTransactionCommand,
      statement,
      statementBlock,
      stringLength,
      stringType,
      subCharacterOrByteString,
      subpathVariable,
      subpathVariableDeclaration,
      temporalDurationQualifier,
      temporalDurationType,
      temporalInstantType,
      temporalLiteral,
      temporalType,
      timeFunction,
      timeFunctionParameters,
      timeLiteral,
      timeString,
      timeType,
      timeWithTimeZoneType,
      timeWithoutTimeZoneType,
      timeZoneString,
      timestampWithTimeZoneType,
      timestampWithoutTimeZoneType,
      transactionAccessMode,
      transactionActivity,
      transactionCharacteristics,
      transactionMode,
      trigonometricFunction,
      trigonometricFunctionName,
      trimCharacterOrByteString,
      trimCharacterOrByteStringSource,
      trimListFunction,
      trimMultiCharacterCharacterString,
      trimOperands,
      trimSingleCharacterOrByteString,
      trimSpecification,
      trimType,
      truthValue,
      typed,
      typedBindingTableReferenceValueType,
      typedGraphReferenceValueType,
      typedGraphTypeReference,
      typedNestedGraphTypeSpecification,
      typedValueType,
      uBigIntType,
      uSmallIntType,
      uint128Type,
      uint16Type,
      uint256Type,
      uint32Type,
      uint64Type,
      uint8Type,
      uintWithPrecision,
      unsignedBinaryExactNumericType,
      unsignedDecimalInteger,
      unsignedInteger,
      unsignedLiteral,
      unsignedNumericLiteral,
      unsignedValueSpecification,
      upperBound,
      useGraphClause,
      valueExpression,
      valueFunction,
      valueInitializer,
      valueQueryExpression,
      valueType,
      valueTypePredicate,
      valueTypePredicatePart2,
      valueVariableDefinition,
      varbinaryType,
      varcharType,
      variableScopeClause,
      verboseBinaryExactNumericType,
      whenOperand,
      whenOperandList,
      whereClause,
      yieldClause,
      yieldItem,
      yieldItemAlias,
      yieldItemList,
      yieldItemName,
      zonedDatetimeType,
      zonedTimeType]

abbreviatedEdgePattern :: TypeDefinition
abbreviatedEdgePattern = def "AbbreviatedEdgePattern" $
  doc "A GQL abbreviated edge pattern: one of a fixed set of alternatives" $
  T.enum ["leftArrow", "tilde", "rightArrow", "leftArrowTilde", "tildeRightArrow", "leftMinusRight", "minusSign"]

absoluteCatalogSchemaReference :: TypeDefinition
absoluteCatalogSchemaReference = def "AbsoluteCatalogSchemaReference" $
  doc "A GQL absolute catalog schema reference: one of several alternative forms" $
  T.union [
    "root">: T.unit,
    "directoryAndSchema">: gql "AbsoluteDirectoryAndSchema"]

absoluteDirectoryAndSchema :: TypeDefinition
absoluteDirectoryAndSchema = def "AbsoluteDirectoryAndSchema" $
  doc "A GQL absolute directory and schema" $
  T.record [
    "directoryPath">: gql "AbsoluteDirectoryPath",
    "schemaName">: gql "SchemaName"]

absoluteDirectoryPath :: TypeDefinition
absoluteDirectoryPath = def "AbsoluteDirectoryPath" $
  doc "An optional GQL absolute directory path" $
  T.optional $ gql "SimpleDirectoryPath"

absoluteValueExpression :: TypeDefinition
absoluteValueExpression = def "AbsoluteValueExpression" $
  doc "A GQL absolute value expression, equivalent to a value expression" $
  gql "ValueExpression"

addSubNumericValueExpression :: TypeDefinition
addSubNumericValueExpression = def "AddSubNumericValueExpression" $
  doc "A GQL addition subtraction numeric value expression" $
  T.record [
    "left">: gql "NumericValueExpression",
    "operator">: gql "AddSubtractOperator",
    "right">: gql "NumericValueExpression"]

addSubtractExpr :: TypeDefinition
addSubtractExpr = def "AddSubtractExpr" $
  doc "A GQL addition subtract expression" $
  T.record [
    "left">: gql "ValueExpression",
    "operator">: gql "AddSubtractOperator",
    "right">: gql "ValueExpression"]

addSubtractOperator :: TypeDefinition
addSubtractOperator = def "AddSubtractOperator" $
  doc "A GQL addition subtract operator: one of a fixed set of alternatives" $
  T.enum ["add", "subtract"]

aggregateFunction :: TypeDefinition
aggregateFunction = def "AggregateFunction" $
  doc "A GQL aggregate function: one of several alternative forms" $
  T.union [
    "countAll">: T.unit,
    "generalSetFunction">: gql "GeneralSetFunction",
    "binarySetFunction">: gql "BinarySetFunction"]

aggregatingValueExpression :: TypeDefinition
aggregatingValueExpression = def "AggregatingValueExpression" $
  doc "A GQL aggregating value expression, equivalent to a value expression" $
  gql "ValueExpression"

allDifferentPredicate :: TypeDefinition
allDifferentPredicate = def "AllDifferentPredicate" $
  doc "A GQL all different predicate" $
  T.record [
    "references">: nonemptyList $ gql "ElementVariableReference"]

allParametersOrCharacteristics :: TypeDefinition
allParametersOrCharacteristics = def "AllParametersOrCharacteristics" $
  doc "A GQL all parameters or characteristics" $
  T.record [
    "all">: T.boolean,
    "type">: gql "ParametersOrCharacteristics"]

allPathSearch :: TypeDefinition
allPathSearch = def "AllPathSearch" $
  doc "A GQL all path search" $
  T.record [
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

allShortestPathSearch :: TypeDefinition
allShortestPathSearch = def "AllShortestPathSearch" $
  doc "A GQL all shortest path search" $
  T.record [
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

ambientLinearDataModifyingStatement :: TypeDefinition
ambientLinearDataModifyingStatement = def "AmbientLinearDataModifyingStatement" $
  doc "A GQL ambient linear data modifying statement: one of several alternative forms" $
  T.union [
    "simple">: gql "AmbientLinearDataModifyingStatementBody",
    "nested">: gql "NestedDataModifyingProcedureSpecification"]

ambientLinearDataModifyingStatementBody :: TypeDefinition
ambientLinearDataModifyingStatementBody = def "AmbientLinearDataModifyingStatementBody" $
  doc "A GQL ambient linear data modifying statement body" $
  T.record [
    "simpleAccess">: gql "SimpleLinearDataAccessingStatement",
    "primitiveResult">: T.optional $ gql "PrimitiveResultStatement"]

ambientLinearQueryStatement :: TypeDefinition
ambientLinearQueryStatement = def "AmbientLinearQueryStatement" $
  doc "A GQL ambient linear query statement: one of several alternative forms" $
  T.union [
    "simple">: gql "AmbientLinearQueryStatementSimpleAndPrimitiveResult",
    "nested">: gql "NestedQuerySpecification"]

ambientLinearQueryStatementSimpleAndPrimitiveResult :: TypeDefinition
ambientLinearQueryStatementSimpleAndPrimitiveResult = def "AmbientLinearQueryStatementSimpleAndPrimitiveResult" $
  doc "A GQL ambient linear query statement simple and primitive result" $
  T.record [
    "simple">: T.optional $ gql "SimpleLinearQueryStatement",
    "primitiveResult">: gql "PrimitiveResultStatement"]

anyPathSearch :: TypeDefinition
anyPathSearch = def "AnyPathSearch" $
  doc "A GQL any path search" $
  T.record [
    "numberOfPaths">: T.optional $ gql "NumberOfPaths",
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

anyRecordType :: TypeDefinition
anyRecordType = def "AnyRecordType" $
  doc "A GQL any record type" $
  T.record [
    "any">: T.boolean,
    "notNull">: T.boolean]

anyShortestPathSearch :: TypeDefinition
anyShortestPathSearch = def "AnyShortestPathSearch" $
  doc "A GQL any shortest path search" $
  T.record [
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

approximateNumericLiteral :: TypeDefinition
approximateNumericLiteral = def "ApproximateNumericLiteral" $
  doc "A GQL approximate numeric literal: one of several alternative forms" $
  T.union [
    "scientificWithSuffix">: T.string,
    "scientificWithoutSuffix">: T.string,
    "commonWithSuffix">: T.string,
    "integerWithSuffix">: T.string]

approximateNumericType :: TypeDefinition
approximateNumericType = def "ApproximateNumericType" $
  doc "A GQL approximate numeric type: one of several alternative forms" $
  T.union [
    "float16">: gql "Float16Type",
    "float32">: gql "Float32Type",
    "float64">: gql "Float64Type",
    "float128">: gql "Float128Type",
    "float256">: gql "Float256Type",
    "floatWithPrecision">: gql "FloatTypeWithPrecision",
    "real">: gql "RealType",
    "doubleWithPrecision">: gql "DoubleTypeWithPrecision"]

arcTypePointingLeft :: TypeDefinition
arcTypePointingLeft = def "ArcTypePointingLeft" $
  doc "A GQL arc type pointing left, equivalent to a edge type filler" $
  gql "EdgeTypeFiller"

arcTypePointingRight :: TypeDefinition
arcTypePointingRight = def "ArcTypePointingRight" $
  doc "A GQL arc type pointing right, equivalent to a edge type filler" $
  gql "EdgeTypeFiller"

arcTypeUndirected :: TypeDefinition
arcTypeUndirected = def "ArcTypeUndirected" $
  doc "A GQL arc type undirected, equivalent to a edge type filler" $
  gql "EdgeTypeFiller"

atSchemaClause :: TypeDefinition
atSchemaClause = def "AtSchemaClause" $
  doc "A GQL at schema clause, equivalent to a schema reference" $
  gql "SchemaReference"

bigIntType :: TypeDefinition
bigIntType = def "BigIntType" $
  doc "A GQL big int type" $
  T.record ["notNull">: T.boolean]

bigIntegerType :: TypeDefinition
bigIntegerType = def "BigIntegerType" $
  doc "A GQL big integer type" $
  T.record ["notNull">: T.boolean]

binaryExactNumericType :: TypeDefinition
binaryExactNumericType = def "BinaryExactNumericType" $
  doc "A GQL binary exact numeric type: one of several alternative forms" $
  T.union [
    "signed">: gql "SignedBinaryExactNumericType",
    "unsigned">: gql "UnsignedBinaryExactNumericType"]

binarySetFunction :: TypeDefinition
binarySetFunction = def "BinarySetFunction" $
  doc "A GQL binary set function" $
  T.record [
    "functionType">: gql "BinarySetFunctionType",
    "dependentValue">: gql "DependentValueExpression",
    "independentValue">: gql "IndependentValueExpression"]

binarySetFunctionType :: TypeDefinition
binarySetFunctionType = def "BinarySetFunctionType" $
  doc "A GQL binary set function type: one of a fixed set of alternatives" $
  T.enum ["percentileCont", "percentileDisc"]

binaryType :: TypeDefinition
binaryType = def "BinaryType" $
  doc "A GQL binary type" $
  T.record [
    "fixedLength">: T.optional $ gql "FixedLength",
    "notNull">: T.boolean]

bindingEqualsValue :: TypeDefinition
bindingEqualsValue = def "BindingEqualsValue" $
  doc "A GQL binding equals value" $
  T.record [
    "binding">: gql "BindingVariable",
    "value">: gql "ValueExpression"]

bindingTableExpression :: TypeDefinition
bindingTableExpression = def "BindingTableExpression" $
  doc "A GQL binding table expression: one of several alternative forms" $
  T.union [
    "nested">: gql "NestedBindingTableQuerySpecification",
    "object">: gql "ObjectExpressionPrimary",
    "table">: gql "BindingTableReference",
    "name">: gql "ObjectNameOrBindingVariable"]

bindingTableInitializer :: TypeDefinition
bindingTableInitializer = def "BindingTableInitializer" $
  doc "A GQL binding table initializer" $
  T.unit

bindingTableName :: TypeDefinition
bindingTableName = def "BindingTableName" $
  doc "A GQL binding table name: one of several alternative forms" $
  T.union [
    "regularIdentifier">: T.string,
    "delimitedBindingTableName">: gql "DelimitedBindingTableName"]

bindingTableReference :: TypeDefinition
bindingTableReference = def "BindingTableReference" $
  doc "A GQL binding table reference: one of several alternative forms" $
  T.union [
    "parentAndTableName">: gql "ParentAndTableName",
    "delimitedBindingTableName">: gql "DelimitedBindingTableName",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

bindingTableReferenceValueType :: TypeDefinition
bindingTableReferenceValueType = def "BindingTableReferenceValueType" $
  doc "A GQL binding table reference value type" $
  T.record [
    "bindingTableType">: gql "BindingTableType",
    "notNull">: T.boolean]

bindingTableType :: TypeDefinition
bindingTableType = def "BindingTableType" $
  doc "A GQL binding table type" $
  T.record [
    "binding">: T.boolean,
    "fieldTypes">: gql "FieldTypesSpecification"]

bindingTableVariableDefinition :: TypeDefinition
bindingTableVariableDefinition = def "BindingTableVariableDefinition" $
  doc "A GQL binding table variable definition" $
  T.record [
    "binding">: T.boolean,
    "variable">: gql "BindingVariable",
    "initializer">: gql "OptTypedBindingTableInitializer"]

bindingVariable :: TypeDefinition
bindingVariable = def "BindingVariable" $
  doc "A GQL binding variable" $
  T.string

bindingVariableDefinition :: TypeDefinition
bindingVariableDefinition = def "BindingVariableDefinition" $
  doc "A GQL binding variable definition: one of several alternative forms" $
  T.union [
    "graph">: gql "GraphVariableDefinition",
    "table">: gql "BindingTableVariableDefinition",
    "value">: gql "ValueVariableDefinition"]

bindingVariableDefinitionBlock :: TypeDefinition
bindingVariableDefinitionBlock = def "BindingVariableDefinitionBlock" $
  doc "A list of GQL binding variable definition block elements" $
  nonemptyList $ gql "BindingVariableDefinition"

bindingVariableReference :: TypeDefinition
bindingVariableReference = def "BindingVariableReference" $
  doc "A GQL binding variable reference, equivalent to a binding variable" $
  gql "BindingVariable"

bindingVariableReferenceList :: TypeDefinition
bindingVariableReferenceList = def "BindingVariableReferenceList" $
  doc "A list of GQL binding variable reference list elements" $
  nonemptyList $ gql "BindingVariableReference"

booleanLiteral :: TypeDefinition
booleanLiteral = def "BooleanLiteral" $
  doc "A GQL boolean literal: one of a fixed set of alternatives" $
  T.enum ["true", "false", "unknown"]

booleanType :: TypeDefinition
booleanType = def "BooleanType" $
  doc "A GQL boolean type" $
  T.record [
    "notNull">: T.boolean]

booleanValueExpression :: TypeDefinition
booleanValueExpression = def "BooleanValueExpression" $
  doc "A GQL boolean value expression, equivalent to a value expression" $
  gql "ValueExpression"

byteLengthExpression :: TypeDefinition
byteLengthExpression = def "ByteLengthExpression" $
  doc "A GQL byte length expression, equivalent to a byte string value expression" $
  gql "ByteStringValueExpression"

byteStringLiteral :: TypeDefinition
byteStringLiteral = def "ByteStringLiteral" $
  doc "A GQL byte string literal" $
    T.string


-- UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITH_EXACT_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION EXACT_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITHOUT_SUFFIX
--     : UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION
--     ;
--
-- UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITH_APPROXIMATE_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION APPROXIMATE_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITH_EXACT_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_IN_COMMON_NOTATION EXACT_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITHOUT_SUFFIX
--     : UNSIGNED_DECIMAL_IN_COMMON_NOTATION
--     ;
--
-- UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITH_APPROXIMATE_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_IN_COMMON_NOTATION APPROXIMATE_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_INTEGER_WITH_EXACT_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_INTEGER EXACT_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_INTEGER_WITH_APPROXIMATE_NUMBER_SUFFIX
--     : UNSIGNED_DECIMAL_INTEGER APPROXIMATE_NUMBER_SUFFIX
--     ;
--
-- UNSIGNED_DECIMAL_INTEGER
--     : DIGIT (UNDERSCORE? DIGIT)*
--     ;
--
-- fragment EXACT_NUMBER_SUFFIX
--     : 'M'
--     ;
--
-- fragment UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION
--     : MANTISSA 'E' EXPONENT
--     ;
--
-- fragment MANTISSA
--     : UNSIGNED_DECIMAL_IN_COMMON_NOTATION
--     | UNSIGNED_DECIMAL_INTEGER
--     ;
--
-- fragment EXPONENT
--     : SIGNED_DECIMAL_INTEGER
--     ;
--
-- fragment UNSIGNED_DECIMAL_IN_COMMON_NOTATION
--     : UNSIGNED_DECIMAL_INTEGER (PERIOD UNSIGNED_DECIMAL_INTEGER?)
--     | PERIOD UNSIGNED_DECIMAL_INTEGER
--     ;
--
-- fragment SIGNED_DECIMAL_INTEGER
--     : (PLUS_SIGN | MINUS_SIGN)? UNSIGNED_DECIMAL_INTEGER
--     ;
--
-- UNSIGNED_HEXADECIMAL_INTEGER
--     : '0x' ('_'? HEX_DIGIT)+
--     ;
--
-- UNSIGNED_OCTAL_INTEGER
--     : '0o' ('_'? OCTAL_DIGIT)+
--     ;
--
-- UNSIGNED_BINARY_INTEGER
--     : '0b' ('_'? BINARY_DIGIT)+
--     ;
--
-- fragment APPROXIMATE_NUMBER_SUFFIX
--     : 'F'
--     | 'D'
--     ;
--
-- // 21.3 <token>, <separator>, and <identifier>
--
-- // Reserved words
-- ABS: 'ABS';
-- ACOS: 'ACOS';
-- ALL: 'ALL';
-- ALL_DIFFERENT: 'ALL_DIFFERENT';
-- AND: 'AND';
-- ANY: 'ANY';
-- ARRAY: 'ARRAY';
-- AS: 'AS';
-- ASC: 'ASC';
-- ASCENDING: 'ASCENDING';
-- ASIN: 'ASIN';
-- AT: 'AT';
-- ATAN: 'ATAN';
-- AVG: 'AVG';
-- BIG: 'BIG';
-- BIGINT: 'BIGINT';
-- BINARY: 'BINARY';
-- BOOL: 'BOOL';
-- BOOLEAN: 'BOOLEAN';
-- BOTH: 'BOTH';
-- BTRIM: 'BTRIM';
-- BY: 'BY';
-- BYTE_LENGTH: 'BYTE_LENGTH';
-- BYTES: 'BYTES';
-- CALL: 'CALL';
-- CARDINALITY: 'CARDINALITY';
-- CASE: 'CASE';
-- CAST: 'CAST';
-- CEIL: 'CEIL';
-- CEILING: 'CEILING';
-- CHAR: 'CHAR';
-- CHAR_LENGTH: 'CHAR_LENGTH';
-- CHARACTER_LENGTH: 'CHARACTER_LENGTH';
-- CHARACTERISTICS: 'CHARACTERISTICS';
-- CLOSE: 'CLOSE';
-- COALESCE: 'COALESCE';
-- COLLECT_LIST: 'COLLECT_LIST';
-- COMMIT: 'COMMIT';
-- COPY: 'COPY';
-- COS: 'COS';
-- COSH: 'COSH';
-- COT: 'COT';
-- COUNT: 'COUNT';
-- CREATE: 'CREATE';
-- CURRENT_DATE: 'CURRENT_DATE';
-- CURRENT_GRAPH: 'CURRENT_GRAPH';
-- CURRENT_PROPERTY_GRAPH: 'CURRENT_PROPERTY_GRAPH';
-- CURRENT_SCHEMA: 'CURRENT_SCHEMA';
-- CURRENT_TIME: 'CURRENT_TIME';
-- CURRENT_TIMESTAMP: 'CURRENT_TIMESTAMP';
-- DATE: 'DATE';
-- DATETIME: 'DATETIME';
-- DAY: 'DAY';
-- DEC: 'DEC';
-- DECIMAL: 'DECIMAL';
-- DEGREES: 'DEGREES';
-- DELETE: 'DELETE';
-- DESC: 'DESC';
-- DESCENDING: 'DESCENDING';
-- DETACH: 'DETACH';
-- DISTINCT: 'DISTINCT';
-- DOUBLE: 'DOUBLE';
-- DROP: 'DROP';
-- DURATION: 'DURATION';
-- DURATION_BETWEEN: 'DURATION_BETWEEN';
-- ELEMENT_ID: 'ELEMENT_ID';
-- ELSE: 'ELSE';
-- END: 'END';
-- EXCEPT: 'EXCEPT';
-- EXISTS: 'EXISTS';
-- EXP: 'EXP';
-- FILTER: 'FILTER';
-- FINISH: 'FINISH';
-- FLOAT: 'FLOAT';
-- FLOAT16: 'FLOAT16';
-- FLOAT32: 'FLOAT32';
-- FLOAT64: 'FLOAT64';
-- FLOAT128: 'FLOAT128';
-- FLOAT256: 'FLOAT256';
-- FLOOR: 'FLOOR';
-- FOR: 'FOR';
-- FROM: 'FROM';
-- GROUP: 'GROUP';
-- HAVING: 'HAVING';
-- HOME_GRAPH: 'HOME_GRAPH';
-- HOME_PROPERTY_GRAPH: 'HOME_PROPERTY_GRAPH';
-- HOME_SCHEMA: 'HOME_SCHEMA';
-- HOUR: 'HOUR';
-- IF: 'IF';
-- IN: 'IN';
-- INSERT: 'INSERT';
-- INT: 'INT';
-- INTEGER: 'INTEGER';
-- INT8: 'INT8';
-- INTEGER8: 'INTEGER8';
-- INT16: 'INT16';
-- INTEGER16: 'INTEGER16';
-- INT32: 'INT32';
-- INTEGER32: 'INTEGER32';
-- INT64: 'INT64';
-- INTEGER64: 'INTEGER64';
-- INT128: 'INT128';
-- INTEGER128: 'INTEGER128';
-- INT256: 'INT256';
-- INTEGER256: 'INTEGER256';
-- INTERSECT: 'INTERSECT';
-- INTERVAL: 'INTERVAL';
-- IS: 'IS';
-- LEADING: 'LEADING';
-- LEFT: 'LEFT';
-- LET: 'LET';
-- LIKE: 'LIKE';
-- LIMIT: 'LIMIT';
-- LIST: 'LIST';
-- LN: 'LN';
-- LOCAL: 'LOCAL';
-- LOCAL_DATETIME: 'LOCAL_DATETIME';
-- LOCAL_TIME: 'LOCAL_TIME';
-- LOCAL_TIMESTAMP: 'LOCAL_TIMESTAMP';
-- LOG: 'LOG';
-- LOG10: 'LOG10';
-- LOWER: 'LOWER';
-- LTRIM: 'LTRIM';
-- MATCH: 'MATCH';
-- MAX: 'MAX';
-- MIN: 'MIN';
-- MINUTE: 'MINUTE';
-- MOD: 'MOD';
-- MONTH: 'MONTH';
-- NEXT: 'NEXT';
-- NODETACH: 'NODETACH';
-- NORMALIZE: 'NORMALIZE';
-- NOT: 'NOT';
-- NOTHING: 'NOTHING';
-- NULL_KW: 'NULL';            // NULL is a commonly used macro in C++.
-- NULLS: 'NULLS';
-- NULLIF: 'NULLIF';
-- OCTET_LENGTH: 'OCTET_LENGTH';
-- OF: 'OF';
-- OFFSET: 'OFFSET';
-- OPTIONAL: 'OPTIONAL';
-- OR: 'OR';
-- ORDER: 'ORDER';
-- OTHERWISE: 'OTHERWISE';
-- PARAMETER: 'PARAMETER';
-- PARAMETERS: 'PARAMETERS';
-- PATH: 'PATH';
-- PATH_LENGTH: 'PATH_LENGTH';
-- PATHS: 'PATHS';
-- PERCENTILE_CONT: 'PERCENTILE_CONT';
-- PERCENTILE_DISC: 'PERCENTILE_DISC';
-- POWER: 'POWER';
-- PRECISION: 'PRECISION';
-- PROPERTY_EXISTS: 'PROPERTY_EXISTS';
-- RADIANS: 'RADIANS';
-- REAL: 'REAL';
-- RECORD: 'RECORD';
-- REMOVE: 'REMOVE';
-- REPLACE: 'REPLACE';
-- RESET: 'RESET';
-- RETURN: 'RETURN';
-- RIGHT: 'RIGHT';
-- ROLLBACK: 'ROLLBACK';
-- RTRIM: 'RTRIM';
-- SAME: 'SAME';
-- SCHEMA: 'SCHEMA';
-- SECOND: 'SECOND';
-- SELECT: 'SELECT';
-- SESSION: 'SESSION';
-- SESSION_USER: 'SESSION_USER';
-- SET: 'SET';
-- SIGNED: 'SIGNED';
-- SIN: 'SIN';
-- SINH: 'SINH';
-- SIZE: 'SIZE';
-- SKIP_RESERVED_WORD: 'SKIP';
-- SMALL: 'SMALL';
-- SMALLINT: 'SMALLINT';
-- SQRT: 'SQRT';
-- START: 'START';
-- STDDEV_POP: 'STDDEV_POP';
-- STDDEV_SAMP: 'STDDEV_SAMP';
-- STRING: 'STRING';
-- SUM: 'SUM';
-- TAN: 'TAN';
-- TANH: 'TANH';
-- THEN: 'THEN';
-- TIME: 'TIME';
-- TIMESTAMP: 'TIMESTAMP';
-- TRAILING: 'TRAILING';
-- TRIM: 'TRIM';
-- TYPED: 'TYPED';
-- UBIGINT: 'UBIGINT';
-- UINT: 'UINT';
-- UINT8: 'UINT8';
-- UINT16: 'UINT16';
-- UINT32: 'UINT32';
-- UINT64: 'UINT64';
-- UINT128: 'UINT128';
-- UINT256: 'UINT256';
-- UNION: 'UNION';
-- UNSIGNED: 'UNSIGNED';
-- UPPER: 'UPPER';
-- USE: 'USE';
-- USMALLINT: 'USMALLINT';
-- VALUE: 'VALUE';
-- VARBINARY: 'VARBINARY';
-- VARCHAR: 'VARCHAR';
-- VARIABLE: 'VARIABLE';
-- WHEN: 'WHEN';
-- WHERE: 'WHERE';
-- WITH: 'WITH';
-- XOR: 'XOR';
-- YEAR: 'YEAR';
-- YIELD: 'YIELD';
-- ZONED: 'ZONED';
-- ZONED_DATETIME: 'ZONED_DATETIME';
-- ZONED_TIME: 'ZONED_TIME';
--
-- // Prereserved words
-- ABSTRACT: 'ABSTRACT';
-- AGGREGATE: 'AGGREGATE';
-- AGGREGATES: 'AGGREGATES';
-- ALTER: 'ALTER';
-- CATALOG: 'CATALOG';
-- CLEAR: 'CLEAR';
-- CLONE: 'CLONE';
-- CONSTRAINT: 'CONSTRAINT';
-- CURRENT_ROLE: 'CURRENT_ROLE';
-- CURRENT_USER: 'CURRENT_USER';
-- DATA: 'DATA';
-- DIRECTORY: 'DIRECTORY';
-- DRYRUN: 'DRYRUN';
-- EXACT: 'EXACT';
-- EXISTING: 'EXISTING';
-- FUNCTION: 'FUNCTION';
-- GQLSTATUS: 'GQLSTATUS';
-- GRANT: 'GRANT';
-- INSTANT: 'INSTANT';
-- INFINITY_KW: 'INFINITY';            // INFINITY is a commonly used macro in C++
-- NUMBER: 'NUMBER';
-- NUMERIC: 'NUMERIC';
-- ON: 'ON';
-- OPEN: 'OPEN';
-- PARTITION: 'PARTITION';
-- PROCEDURE: 'PROCEDURE';
-- PRODUCT: 'PRODUCT';
-- PROJECT: 'PROJECT';
-- QUERY: 'QUERY';
-- RECORDS: 'RECORDS';
-- REFERENCE: 'REFERENCE';
-- RENAME: 'RENAME';
-- REVOKE: 'REVOKE';
-- SUBSTRING: 'SUBSTRING';
-- SYSTEM_USER: 'SYSTEM_USER';
-- TEMPORAL: 'TEMPORAL';
-- UNIQUE: 'UNIQUE';
-- UNIT: 'UNIT';
-- VALUES: 'VALUES';
--
-- // Nonreserved words
-- ACYCLIC: 'ACYCLIC';
-- BINDING: 'BINDING';
-- BINDINGS: 'BINDINGS';
-- CONNECTING: 'CONNECTING';
-- DESTINATION: 'DESTINATION';
-- DIFFERENT: 'DIFFERENT';
-- DIRECTED: 'DIRECTED';
-- EDGE: 'EDGE';
-- EDGES: 'EDGES';
-- ELEMENT: 'ELEMENT';
-- ELEMENTS: 'ELEMENTS';
-- FIRST: 'FIRST';
-- GRAPH: 'GRAPH';
-- GROUPS: 'GROUPS';
-- KEEP: 'KEEP';
-- LABEL: 'LABEL';
-- LABELED: 'LABELED';
-- LABELS: 'LABELS';
-- LAST: 'LAST';
-- NFC: 'NFC';
-- NFD: 'NFD';
-- NFKC: 'NFKC';
-- NFKD: 'NFKD';
-- NO: 'NO';
-- NODE: 'NODE';
-- NORMALIZED: 'NORMALIZED';
-- ONLY: 'ONLY';
-- ORDINALITY: 'ORDINALITY';
-- PROPERTY: 'PROPERTY';
-- READ: 'READ';
-- RELATIONSHIP: 'RELATIONSHIP';
-- RELATIONSHIPS: 'RELATIONSHIPS';
-- REPEATABLE: 'REPEATABLE';
-- SHORTEST: 'SHORTEST';
-- SIMPLE: 'SIMPLE';
-- SOURCE: 'SOURCE';
-- TABLE: 'TABLE';
-- TEMP: 'TEMP';
-- TO: 'TO';
-- TRAIL: 'TRAIL';
-- TRANSACTION: 'TRANSACTION';
-- TYPE: 'TYPE';
-- UNDIRECTED: 'UNDIRECTED';
-- VERTEX: 'VERTEX';
-- WALK: 'WALK';
-- WITHOUT: 'WITHOUT';
-- WRITE: 'WRITE';
-- ZONE: 'ZONE';
--
-- fragment SEPARATED_IDENTIFIER
--     : DELIMITED_IDENTIFIER
--     | EXTENDED_IDENTIFIER
--     ;
--
-- REGULAR_IDENTIFIER
--     : IDENTIFIER_START IDENTIFIER_EXTEND*
--     ;
--
-- fragment EXTENDED_IDENTIFIER
--     : IDENTIFIER_EXTEND+
--     ;
--
-- fragment DELIMITED_IDENTIFIER
--     : DOUBLE_QUOTED_CHARACTER_SEQUENCE
--     | ACCENT_QUOTED_CHARACTER_SEQUENCE
--     ;
--
-- SUBSTITUTED_PARAMETER_REFERENCE
--     : DOUBLE_DOLLAR_SIGN PARAMETER_NAME
--     ;
--
-- GENERAL_PARAMETER_REFERENCE
--     : DOLLAR_SIGN PARAMETER_NAME
--     ;
--
-- fragment IDENTIFIER_START
--     : ID_Start
--     | Pc
--     ;
--
-- fragment IDENTIFIER_EXTEND
--     : ID_Continue
--     ;
--
-- fragment ID_Start
--     : [\p{ID_Start}]
--     ;
--
-- fragment ID_Continue
--     : [\p{ID_Continue}]
--     ;
--
-- MULTISET_ALTERNATION_OPERATOR: '|+|';
--
-- BRACKET_RIGHT_ARROW: ']->';
-- BRACKET_TILDE_RIGHT_ARROW: ']~>';
-- CONCATENATION_OPERATOR: '||';
-- DOUBLE_COLON: '::';
-- DOUBLE_DOLLAR_SIGN: '$$';
-- DOUBLE_PERIOD: '..';
-- GREATER_THAN_OR_EQUALS_OPERATOR: '>=';
-- LEFT_ARROW: '<-';
-- LEFT_ARROW_TILDE: '<~';
-- LEFT_ARROW_BRACKET: '<-[';
-- LEFT_ARROW_TILDE_BRACKET: '<~[';
-- LEFT_MINUS_RIGHT: '<->';
-- LEFT_MINUS_SLASH: '<-/';
-- LEFT_TILDE_SLASH: '<~/';
-- LESS_THAN_OR_EQUALS_OPERATOR: '<=';
-- MINUS_LEFT_BRACKET: '-[';
-- MINUS_SLASH: '-/';
-- NOT_EQUALS_OPERATOR: '<>';
-- RIGHT_ARROW: '->';
-- RIGHT_BRACKET_MINUS: ']-';
-- RIGHT_BRACKET_TILDE: ']~';
-- RIGHT_DOUBLE_ARROW: '=>';
-- SLASH_MINUS: '/-';
-- SLASH_MINUS_RIGHT: '/->';
-- SLASH_TILDE: '/~';
-- SLASH_TILDE_RIGHT: '/~>';
-- TILDE_LEFT_BRACKET: '~[';
-- TILDE_RIGHT_ARROW: '~>';
-- TILDE_SLASH: '~/';
--
-- // 21.4 GQL terminal characters
--
-- AMPERSAND: '&';
-- ASTERISK: '*';
-- COLON: ':';
-- COMMA: ',';
-- COMMERCIAL_AT: '@';
-- DOLLAR_SIGN: '$';
-- DOUBLE_QUOTE: '"';
-- EQUALS_OPERATOR: '=';
-- EXCLAMATION_MARK: '!';
-- RIGHT_ANGLE_BRACKET: '>';
-- GRAVE_ACCENT: '`';
-- LEFT_BRACE: '{';
-- LEFT_BRACKET: '[';
-- LEFT_PAREN: '(';
-- LEFT_ANGLE_BRACKET: '<';
-- MINUS_SIGN: '-';
-- PERCENT: '%';
-- PERIOD: '.';
-- PLUS_SIGN: '+';
-- QUESTION_MARK: '?';
-- QUOTE: '\'';
-- REVERSE_SOLIDUS: '\\';
-- RIGHT_BRACE: '}';
-- RIGHT_BRACKET: ']';
-- RIGHT_PAREN: ')';
-- SOLIDUS: '/';
-- TILDE: '~';
-- UNDERSCORE: '_';
-- VERTICAL_BAR: '|';
--
-- fragment HEX_DIGIT
--     : [0-9a-f]
--     ;
--
-- fragment DIGIT
--     : [0-9]
--     ;
--
-- fragment OCTAL_DIGIT
--     : [0-7]
--     ;
--
-- fragment BINARY_DIGIT
--     : [0-1]
--     ;
--
-- SP
--   : (WHITESPACE)+
--   -> channel(HIDDEN)
--   ;
--
-- WHITESPACE
--     : SPACE
--     | TAB
--     | LF
--     | VT
--     | FF
--     | CR
--     | FS
--     | GS
--     | RS
--     | US
--     | '\u1680'
--     | '\u180e'
--     | '\u2000'
--     | '\u2001'
--     | '\u2002'
--     | '\u2003'
--     | '\u2004'
--     | '\u2005'
--     | '\u2006'
--     | '\u2008'
--     | '\u2009'
--     | '\u200a'
--     | '\u2028'
--     | '\u2029'
--     | '\u205f'
--     | '\u3000'
--     | '\u00a0'
--     | '\u2007'
--     | '\u202f'
--     ;
--
-- BRACKETED_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);
--
-- SIMPLE_COMMENT_SOLIDUS: '//' ~[\r\n]* -> channel(HIDDEN);
--
-- SIMPLE_COMMENT_MINUS: '--' ~[\r\n]* -> channel(HIDDEN);
--
-- fragment GS : [\u001D];
--
-- fragment FS : [\u001C];
--
-- fragment CR : [\r];
--
-- fragment Sc : [\p{Sc}];
--
-- fragment SPACE : [ ];
--
-- fragment Pc : [\p{Pc}];
--
-- fragment TAB : [\t];
--
-- fragment LF : [\n];
--
-- fragment VT : [\u000B];
--
-- fragment US : [\u001F];
--
-- fragment FF: [\f];
--
-- fragment RS: [\u001E];

byteStringType :: TypeDefinition
byteStringType = def "ByteStringType" $
  doc "A GQL byte string type: one of several alternative forms" $
  T.union [
    "bytesType">: gql "BytesType",
    "binaryType">: gql "BinaryType",
    "varbinaryType">: gql "VarbinaryType"]

byteStringValueExpression :: TypeDefinition
byteStringValueExpression = def "ByteStringValueExpression" $
  doc "A GQL byte string value expression, equivalent to a value expression" $
  gql "ValueExpression"

bytesType :: TypeDefinition
bytesType = def "BytesType" $
  doc "A GQL bytes type" $
  T.record [
    "minLength">: T.optional $ gql "MinLength",
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

callCatalogModifyingProcedureStatement :: TypeDefinition
callCatalogModifyingProcedureStatement = def "CallCatalogModifyingProcedureStatement" $
  doc "A GQL call catalog modifying procedure statement, equivalent to a call procedure statement" $
  gql "CallProcedureStatement"

callDataModifyingProcedureStatement :: TypeDefinition
callDataModifyingProcedureStatement = def "CallDataModifyingProcedureStatement" $
  doc "A GQL call data modifying procedure statement, equivalent to a call procedure statement" $
  gql "CallProcedureStatement"

callProcedureStatement :: TypeDefinition
callProcedureStatement = def "CallProcedureStatement" $
  doc "A GQL call procedure statement" $
  T.record [
    "optional">: T.boolean,
    "call">: gql "ProcedureCall"]

callQueryStatement :: TypeDefinition
callQueryStatement = def "CallQueryStatement" $
  doc "A GQL call query statement, equivalent to a call procedure statement" $
  gql "CallProcedureStatement"

cardinalityArgumentExpression :: TypeDefinition
cardinalityArgumentExpression = def "CardinalityArgumentExpression" $
  doc "A GQL cardinality argument expression, equivalent to a value expression" $
  gql "ValueExpression"

cardinalityExpression :: TypeDefinition
cardinalityExpression = def "CardinalityExpression" $
  doc "A GQL cardinality expression: one of several alternative forms" $
  T.union [
    "cardinality">: gql "CardinalityArgumentExpression",
    "size">: gql "ListValueExpression"]

caseAbbreviation :: TypeDefinition
caseAbbreviation = def "CaseAbbreviation" $
  doc "A GQL case abbreviation: one of several alternative forms" $
  T.union [
    "nullIf">: gql "NullIfAbbreviation",
    "coalesce">: nonemptyList $ gql "ValueExpression"]

caseExpression :: TypeDefinition
caseExpression = def "CaseExpression" $
  doc "A GQL case expression: one of several alternative forms" $
  T.union [
    "abbreviation">: gql "CaseAbbreviation",
    "specification">: gql "CaseSpecification"]

caseOperand :: TypeDefinition
caseOperand = def "CaseOperand" $
  doc "A GQL case operand: one of several alternative forms" $
  T.union [
    "valueExpression">: gql "NonParenthesizedPrimaryValueExpression",
    "elementReference">: gql "ElementVariableReference"]

caseSpecification :: TypeDefinition
caseSpecification = def "CaseSpecification" $
  doc "A GQL case specification: one of several alternative forms" $
  T.union [
    "simple">: gql "SimpleCase",
    "searched">: gql "SearchedCase"]

case_ :: TypeDefinition
case_ = def "Case" $
  doc "A GQL case: one of a fixed set of alternatives" $
  T.enum ["upper", "lower"]

castOperand :: TypeDefinition
castOperand = def "CastOperand" $
  doc "A GQL cast operand: one of several alternative forms" $
  T.union [
    "valueExpression">: gql "ValueExpression",
    "nullLiteral">: T.unit]

castSpecification :: TypeDefinition
castSpecification = def "CastSpecification" $
  doc "A GQL cast specification" $
  T.record [
    "operand">: gql "CastOperand",
    "target">: gql "CastTarget"]

castTarget :: TypeDefinition
castTarget = def "CastTarget" $
  doc "A GQL cast target, equivalent to a value type" $
  gql "ValueType"

catalogGraphParentAndName :: TypeDefinition
catalogGraphParentAndName = def "CatalogGraphParentAndName" $
  doc "A GQL catalog graph parent and name" $
  T.record [
    "parentReference">: T.optional $ gql "CatalogObjectParentReference",
    "graphName">: gql "GraphName"]

catalogGraphTypeParentAndName :: TypeDefinition
catalogGraphTypeParentAndName = def "CatalogGraphTypeParentAndName" $
  doc "A GQL catalog graph type parent and name" $
  T.record [
    "parentReference">: T.optional $ gql "CatalogObjectParentReference",
    "graphTypeName">: gql "GraphTypeName"]

catalogObjectParentReference :: TypeDefinition
catalogObjectParentReference = def "CatalogObjectParentReference" $
  doc "A GQL catalog object parent reference: one of several alternative forms" $
  T.union [
    "schemaAndObjects">: gql "SchemaAndObjects",
    "objectsOnly">: nonemptyList $ gql "ObjectName"]

catalogProcedureParentAndName :: TypeDefinition
catalogProcedureParentAndName = def "CatalogProcedureParentAndName" $
  doc "A GQL catalog procedure parent and name" $
  T.record [
    "parentReference">: T.optional $ gql "CatalogObjectParentReference",
    "procedureName">: gql "ProcedureName"]

catalogSchemaParentAndName :: TypeDefinition
catalogSchemaParentAndName = def "CatalogSchemaParentAndName" $
  doc "A GQL catalog schema parent and name, equivalent to a absolute directory and schema" $
  gql "AbsoluteDirectoryAndSchema"

ceilingFunction :: TypeDefinition
ceilingFunction = def "CeilingFunction" $
  doc "A GQL ceiling function, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

charLengthExpression :: TypeDefinition
charLengthExpression = def "CharLengthExpression" $
  doc "A GQL char length expression, equivalent to a character string value expression" $
  gql "CharacterStringValueExpression"

charType :: TypeDefinition
charType = def "CharType" $
  doc "A GQL char type" $
  T.record [
    "fixedLength">: T.optional $ gql "FixedLength",
    "notNull">: T.boolean]

characterOrByteStringFunction :: TypeDefinition
characterOrByteStringFunction = def "CharacterOrByteStringFunction" $
  doc "A GQL character or byte string function: one of several alternative forms" $
  T.union [
    "sub">: gql "SubCharacterOrByteString",
    "trimSingle">: gql "TrimSingleCharacterOrByteString",
    "fold">: gql "FoldCharacterString",
    "trimMultiCharacter">: gql "TrimMultiCharacterCharacterString",
    "normalize">: gql "NormalizeCharacterString"]

characterStringLiteral :: TypeDefinition
characterStringLiteral = def "CharacterStringLiteral" $
  doc "A GQL character string literal" $
  T.string

characterStringType :: TypeDefinition
characterStringType = def "CharacterStringType" $
  doc "A GQL character string type: one of several alternative forms" $
  T.union [
    "stringType">: gql "StringType",
    "charType">: gql "CharType",
    "varcharType">: gql "VarcharType"]

characterStringValueExpression :: TypeDefinition
characterStringValueExpression = def "CharacterStringValueExpression" $
  doc "A GQL character string value expression, equivalent to a value expression" $
  gql "ValueExpression"

closedDynamicUnionTypeAlt1 :: TypeDefinition
closedDynamicUnionTypeAlt1 = def "ClosedDynamicUnionTypeAlt1" $
  doc "A GQL closed dynamic union type alt1" $
  T.record [
    "anyValue">: T.optional T.boolean,
    "valueTypes">: nonemptyList $ gql "ValueType"]

closedDynamicUnionTypeAlt2 :: TypeDefinition
closedDynamicUnionTypeAlt2 = def "ClosedDynamicUnionTypeAlt2" $
  doc "A GQL closed dynamic union type alt2" $
  T.record [
    "valueTypes">: nonemptyList $ gql "ValueType"]

closedEdgeReferenceValueType :: TypeDefinition
closedEdgeReferenceValueType = def "ClosedEdgeReferenceValueType" $
  doc "A GQL closed edge reference value type" $
  T.record [
    "edgeTypeSpec">: gql "EdgeTypeSpecification",
    "notNull">: T.boolean]

closedGraphReferenceValueType :: TypeDefinition
closedGraphReferenceValueType = def "ClosedGraphReferenceValueType" $
  doc "A GQL closed graph reference value type" $
  T.record [
    "property">: T.boolean,
    "nestedSpec">: gql "NestedGraphTypeSpecification",
    "notNull">: T.boolean]

closedNodeReferenceValueType :: TypeDefinition
closedNodeReferenceValueType = def "ClosedNodeReferenceValueType" $
  doc "A GQL closed node reference value type" $
  T.record [
    "nodeTypeSpec">: gql "NodeTypeSpecification",
    "notNull">: T.boolean]

commitCommand :: TypeDefinition
commitCommand = def "CommitCommand" $
  doc "A GQL commit command" $
  T.unit

commonLogarithm :: TypeDefinition
commonLogarithm = def "CommonLogarithm" $
  doc "A GQL common logarithm, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

compOp :: TypeDefinition
compOp = def "CompOp" $
  doc "A GQL comp op: one of a fixed set of alternatives" $
  T.enum [
    "equals",
    "notEquals",
    "lessThan",
    "greaterThan",
    "lessThanOrEquals",
    "greaterThanOrEquals"]

comparisonExpr :: TypeDefinition
comparisonExpr = def "ComparisonExpr" $
  doc "A GQL comparison expression" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "comparison">: gql "ComparisonPredicatePart2"]

comparisonPredicatePart2 :: TypeDefinition
comparisonPredicatePart2 = def "ComparisonPredicatePart2" $
  doc "A GQL comparison predicate part2" $
  T.record [
    "compOp">: gql "CompOp",
    "valueExpression">: gql "ValueExpression"]

compositeQueryExpression :: TypeDefinition
compositeQueryExpression = def "CompositeQueryExpression" $
  doc "A GQL composite query expression: one of several alternative forms" $
  T.union [
    "simple">: gql "CompositeQueryExpressionConjunction",
    "primary">: gql "CompositeQueryPrimary"]

compositeQueryExpressionConjunction :: TypeDefinition
compositeQueryExpressionConjunction = def "CompositeQueryExpressionConjunction" $
  doc "A GQL composite query expression conjunction" $
  T.record [
    "left">: gql "CompositeQueryExpression",
    "conjunction">: gql "QueryConjunction",
    "right">: gql "CompositeQueryPrimary"]

compositeQueryPrimary :: TypeDefinition
compositeQueryPrimary = def "CompositeQueryPrimary" $
  doc "A GQL composite query primary, equivalent to a linear query statement" $
  gql "LinearQueryStatement"

compositeQueryStatement :: TypeDefinition
compositeQueryStatement = def "CompositeQueryStatement" $
  doc "A GQL composite query statement, equivalent to a composite query expression" $
  gql "CompositeQueryExpression"

concatenationExpr :: TypeDefinition
concatenationExpr = def "ConcatenationExpr" $
  doc "A GQL concatenation expression" $
  T.record [
    "left">: gql "ValueExpression",
    "right">: gql "ValueExpression"]

conjunctionLabelExpression :: TypeDefinition
conjunctionLabelExpression = def "ConjunctionLabelExpression" $
  doc "A GQL conjunction label expression" $
  T.record [
    "left">: gql "LabelExpression",
    "right">: gql "LabelExpression"]

conjunctiveExpr :: TypeDefinition
conjunctiveExpr = def "ConjunctiveExpr" $
  doc "A GQL conjunctive expression" $
  T.record [
    "left">: gql "ValueExpression",
    "right">: gql "ValueExpression"]

connectorPointingRight :: TypeDefinition
connectorPointingRight = def "ConnectorPointingRight" $
  doc "A GQL connector pointing right: one of a fixed set of alternatives" $
  T.enum ["to", "rightArrow"]

connectorUndirected :: TypeDefinition
connectorUndirected = def "ConnectorUndirected" $
  doc "A GQL connector undirected: one of a fixed set of alternatives" $
  T.enum ["to", "tilde"]

copyOfGraphType :: TypeDefinition
copyOfGraphType = def "CopyOfGraphType" $
  doc "A GQL copy of graph type, equivalent to a graph type reference" $
  gql "GraphTypeReference"

countedShortestGroupSearch :: TypeDefinition
countedShortestGroupSearch = def "CountedShortestGroupSearch" $
  doc "A GQL counted shortest group search" $
  T.record [
    "numberOfGroups">: T.optional $ gql "NumberOfGroups",
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths",
    "groups">: T.boolean]

countedShortestPathSearch :: TypeDefinition
countedShortestPathSearch = def "CountedShortestPathSearch" $
  doc "A GQL counted shortest path search" $
  T.record [
    "numberOfPaths">: gql "NumberOfPaths",
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

createGraphOption :: TypeDefinition
createGraphOption = def "CreateGraphOption" $
  doc "A GQL create graph option: one of several alternative forms" $
  T.union [
    "graphIfNotExists">: T.boolean,
    "orReplace">: T.unit]

createGraphStatement :: TypeDefinition
createGraphStatement = def "CreateGraphStatement" $
  doc "A GQL create graph statement" $
  T.record [
    "createOption">: gql "CreateGraphOption",
    "parentAndName">: gql "CatalogGraphParentAndName",
    "type">: gql "GraphTypeOption",
    "source">: T.optional $ gql "GraphSource"]

createGraphTypeOption :: TypeDefinition
createGraphTypeOption = def "CreateGraphTypeOption" $
  doc "A GQL create graph type option: one of several alternative forms" $
  T.union [
    "typeIfNotExists">: T.boolean,
    "orReplace">: T.unit]

createGraphTypeStatement :: TypeDefinition
createGraphTypeStatement = def "CreateGraphTypeStatement" $
  doc "A GQL create graph type statement" $
  T.record [
    "createOption">: gql "CreateGraphTypeOption",
    "parentAndName">: gql "CatalogGraphTypeParentAndName",
    "source">: gql "GraphTypeSource"]

createSchemaStatement :: TypeDefinition
createSchemaStatement = def "CreateSchemaStatement" $
  doc "A GQL create schema statement" $
  T.record [
    "ifNotExists">: T.boolean,
    "parentAndName">: gql "CatalogSchemaParentAndName"]

currentGraph :: TypeDefinition
currentGraph = def "CurrentGraph" $
  doc "A GQL current graph: one of a fixed set of alternatives" $
  T.enum ["graph", "propertyGraph"]

dateFunction :: TypeDefinition
dateFunction = def "DateFunction" $
  doc "A GQL date function: one of several alternative forms" $
  T.union [
    "currentDate">: T.unit,
    "dateWithParams">: T.optional $ gql "DateFunctionParameters"]

dateFunctionParameters :: TypeDefinition
dateFunctionParameters = def "DateFunctionParameters" $
  doc "A GQL date function parameters: one of several alternative forms" $
  T.union [
    "dateString">: gql "DateString",
    "recordConstructor">: gql "RecordConstructor"]

dateLiteral :: TypeDefinition
dateLiteral = def "DateLiteral" $
  doc "A GQL date literal, equivalent to a date string" $
  gql "DateString"

dateString :: TypeDefinition
dateString = def "DateString" $
  doc "A GQL date string, equivalent to a character string literal" $
  gql "CharacterStringLiteral"

dateType :: TypeDefinition
dateType = def "DateType" $
  doc "A GQL date type" $
  T.record [
    "notNull">: T.boolean]

datetimeFunction :: TypeDefinition
datetimeFunction = def "DatetimeFunction" $
  doc "A GQL datetime function: one of several alternative forms" $
  T.union [
    "currentTimestamp">: T.unit,
    "zonedDatetimeWithParams">: T.optional $ gql "DatetimeFunctionParameters"]

datetimeFunctionParameters :: TypeDefinition
datetimeFunctionParameters = def "DatetimeFunctionParameters" $
  doc "A GQL datetime function parameters: one of several alternative forms" $
  T.union [
    "datetimeString">: gql "DatetimeString",
    "recordConstructor">: gql "RecordConstructor"]

datetimeLiteral :: TypeDefinition
datetimeLiteral = def "DatetimeLiteral" $
  doc "A GQL datetime literal, equivalent to a datetime string" $
  gql "DatetimeString"

datetimeString :: TypeDefinition
datetimeString = def "DatetimeString" $
  doc "A GQL datetime string, equivalent to a character string literal" $
  gql "CharacterStringLiteral"

datetimeSubtraction :: TypeDefinition
datetimeSubtraction = def "DatetimeSubtraction" $
  doc "A GQL datetime subtraction" $
  T.record [
    "parameters">: gql "DatetimeSubtractionParameters",
    "temporalDurationQualifier">: T.optional $ gql "TemporalDurationQualifier"]

datetimeSubtractionParameters :: TypeDefinition
datetimeSubtractionParameters = def "DatetimeSubtractionParameters" $
  doc "A GQL datetime subtraction parameters" $
  T.record [
    "expression1">: gql "DatetimeValueExpression1",
    "expression2">: gql "DatetimeValueExpression2"]

datetimeType :: TypeDefinition
datetimeType = def "DatetimeType" $
  doc "A GQL datetime type: one of several alternative forms" $
  T.union [
    "zonedDatetime">: gql "ZonedDatetimeType",
    "timestampWithTimeZone">: gql "TimestampWithTimeZoneType"]

datetimeValueExpression :: TypeDefinition
datetimeValueExpression = def "DatetimeValueExpression" $
  doc "A GQL datetime value expression, equivalent to a value expression" $
  gql "ValueExpression"

datetimeValueExpression1 :: TypeDefinition
datetimeValueExpression1 = def "DatetimeValueExpression1" $
  doc "A GQL datetime value expression1, equivalent to a datetime value expression" $
  gql "DatetimeValueExpression"

datetimeValueExpression2 :: TypeDefinition
datetimeValueExpression2 = def "DatetimeValueExpression2" $
  doc "A GQL datetime value expression2, equivalent to a datetime value expression" $
  gql "DatetimeValueExpression"

datetimeValueFunction :: TypeDefinition
datetimeValueFunction = def "DatetimeValueFunction" $
  doc "A GQL datetime value function: one of several alternative forms" $
  T.union [
    "dateFunction">: gql "DateFunction",
    "timeFunction">: gql "TimeFunction",
    "datetimeFunction">: gql "DatetimeFunction",
    "localtimeFunction">: gql "LocaltimeFunction",
    "localdatetimeFunction">: gql "LocaldatetimeFunction"]

decimalExactNumericType :: TypeDefinition
decimalExactNumericType = def "DecimalExactNumericType" $
  doc "An optional GQL decimal exact numeric type" $
  T.optional $ gql "PrecisionAndScale"

def :: String -> Type -> TypeDefinition
def = datatype ns

deleteItem :: TypeDefinition
deleteItem = def "DeleteItem" $
  doc "A GQL delete item, equivalent to a value expression" $
  gql "ValueExpression"

deleteItemList :: TypeDefinition
deleteItemList = def "DeleteItemList" $
  doc "A list of GQL delete item list elements" $
  nonemptyList $ gql "DeleteItem"

deleteStatement :: TypeDefinition
deleteStatement = def "DeleteStatement" $
  doc "A GQL delete statement" $
  T.record [
    "detach">: T.optional $ gql "DetachOption",
    "items">: gql "DeleteItemList"]

delimitedBindingTableName :: TypeDefinition
delimitedBindingTableName = def "DelimitedBindingTableName" $
  doc "A GQL delimited binding table name" $
  T.string

delimitedGraphName :: TypeDefinition
delimitedGraphName = def "DelimitedGraphName" $
  doc "A GQL delimited graph name" $
  T.string

dependentValueExpression :: TypeDefinition
dependentValueExpression = def "DependentValueExpression" $
  doc "A GQL dependent value expression" $
  T.record [
    "setQuantifier">: T.optional $ gql "SetQuantifier",
    "numericValue">: gql "NumericValueExpression"]

destinationNodeTypeAlias :: TypeDefinition
destinationNodeTypeAlias = def "DestinationNodeTypeAlias" $
  doc "A GQL destination node type alias" $
  T.string

destinationNodeTypeReference :: TypeDefinition
destinationNodeTypeReference = def "DestinationNodeTypeReference" $
  doc "A GQL destination node type reference: one of several alternative forms" $
  T.union [
    "alias">: gql "DestinationNodeTypeAlias",
    "filler">: T.optional $ gql "NodeTypeFiller"]

destinationPredicate :: TypeDefinition
destinationPredicate = def "DestinationPredicate" $
  doc "A GQL destination predicate" $
  T.record [
    "nodeReference">: gql "NodeReference",
    "not">: T.boolean,
    "destinationOf">: gql "EdgeReference"]

detachOption :: TypeDefinition
detachOption = def "DetachOption" $
  doc "A GQL detach option: one of a fixed set of alternatives" $
  T.enum [
    "detach",
    "noDetach"]

differentEdgesMatchMode :: TypeDefinition
differentEdgesMatchMode = def "DifferentEdgesMatchMode" $
  doc "A GQL different edges match mode, equivalent to a edge bindings or edges" $
  gql "EdgeBindingsOrEdges"

directedPredicate :: TypeDefinition
directedPredicate = def "DirectedPredicate" $
  doc "A GQL directed predicate" $
  T.record [
    "elementVariableReference">: gql "ElementVariableReference",
    "directedPart">: gql "DirectedPredicatePart2"]

directedPredicatePart2 :: TypeDefinition
directedPredicatePart2 = def "DirectedPredicatePart2" $
  doc "A GQL directed predicate part2" $
  T.record [
    "not">: T.boolean]

directoryName :: TypeDefinition
directoryName = def "DirectoryName" $
  doc "A GQL directory name" $
  T.string

disjunctionLabelExpression :: TypeDefinition
disjunctionLabelExpression = def "DisjunctionLabelExpression" $
  doc "A GQL disjunction label expression" $
  T.record [
    "left">: gql "LabelExpression",
    "right">: gql "LabelExpression"]

disjunctiveExpr :: TypeDefinition
disjunctiveExpr = def "DisjunctiveExpr" $
  doc "A GQL disjunctive expression" $
  T.record [
    "left">: gql "ValueExpression",
    "operator">: gql "DisjunctiveOperator",
    "right">: gql "ValueExpression"]

disjunctiveOperator :: TypeDefinition
disjunctiveOperator = def "DisjunctiveOperator" $
  doc "A GQL disjunctive operator: one of a fixed set of alternatives" $
  T.enum ["or", "xor"]

doubleTypeWithPrecision :: TypeDefinition
doubleTypeWithPrecision = def "DoubleTypeWithPrecision" $
  doc "A GQL double type with precision" $
  T.record [
    "precision">: T.boolean,
    "notNull">: T.boolean]

dropGraphStatement :: TypeDefinition
dropGraphStatement = def "DropGraphStatement" $
  doc "A GQL drop graph statement" $
  T.record [
    "ifExists">: T.boolean,
    "parentAndName">: gql "CatalogGraphParentAndName"]

dropGraphTypeStatement :: TypeDefinition
dropGraphTypeStatement = def "DropGraphTypeStatement" $
  doc "A GQL drop graph type statement" $
  T.record [
    "ifExists">: T.boolean,
    "parentAndName">: gql "CatalogGraphTypeParentAndName"]

dropSchemaStatement :: TypeDefinition
dropSchemaStatement = def "DropSchemaStatement" $
  doc "A GQL drop schema statement" $
  T.record [
    "ifExists">: T.boolean,
    "parentAndName">: gql "CatalogSchemaParentAndName"]

durationFunction :: TypeDefinition
durationFunction = def "DurationFunction" $
  doc "A GQL duration function, equivalent to a duration function parameters" $
  gql "DurationFunctionParameters"

durationFunctionParameters :: TypeDefinition
durationFunctionParameters = def "DurationFunctionParameters" $
  doc "A GQL duration function parameters: one of several alternative forms" $
  T.union [
    "durationString">: gql "DurationString",
    "recordConstructor">: gql "RecordConstructor"]

durationLiteral :: TypeDefinition
durationLiteral = def "DurationLiteral" $
  doc "A GQL duration literal, equivalent to a duration string" $
  gql "DurationString"

durationString :: TypeDefinition
durationString = def "DurationString" $
  doc "A GQL duration string, equivalent to a character string literal" $
  gql "CharacterStringLiteral"

durationValueExpression :: TypeDefinition
durationValueExpression = def "DurationValueExpression" $
  doc "A GQL duration value expression, equivalent to a value expression" $
  gql "ValueExpression"

durationValueFunction :: TypeDefinition
durationValueFunction = def "DurationValueFunction" $
  doc "A GQL duration value function: one of several alternative forms" $
  T.union [
    "durationFunction">: gql "DurationFunction",
    "absoluteValue">: gql "AbsoluteValueExpression"]

dynamicParameterSpecification :: TypeDefinition
dynamicParameterSpecification = def "DynamicParameterSpecification" $
  doc "A GQL dynamic parameter specification, equivalent to a parameter name" $
  gql "ParameterName"

dynamicPropertyValueType :: TypeDefinition
dynamicPropertyValueType = def "DynamicPropertyValueType" $
  doc "A GQL dynamic property value type" $
  T.record [
    "any">: T.optional T.boolean,
    "property">: T.unit,
    "value">: T.unit,
    "notNull">: T.boolean]

edgeBindingsOrEdges :: TypeDefinition
edgeBindingsOrEdges = def "EdgeBindingsOrEdges" $
  doc "A GQL edge bindings or edges: one of several alternative forms" $
  T.union [
    "edgeBindings">: T.boolean,
    "edges">: T.unit]

edgeKeyLabelSetWithContent :: TypeDefinition
edgeKeyLabelSetWithContent = def "EdgeKeyLabelSetWithContent" $
  doc "A GQL edge key label set with content" $
  T.record [
    "keyLabelSet">: gql "EdgeTypeKeyLabelSet",
    "impliedContent">: T.optional $ gql "EdgeTypeImpliedContent"]

edgeKind :: TypeDefinition
edgeKind = def "EdgeKind" $
  doc "A GQL edge kind: one of a fixed set of alternatives" $
  T.enum ["directed", "undirected"]

edgeKindAndSynonym :: TypeDefinition
edgeKindAndSynonym = def "EdgeKindAndSynonym" $
  doc "A GQL edge kind and synonym" $
  T.record [
    "kind">: T.optional $ gql "EdgeKind",
    "synonym">: gql "EdgeSynonym",
    "typeName">: T.optional $ gql "EdgeTypeName"]

edgeLabelSetWithProperties :: TypeDefinition
edgeLabelSetWithProperties = def "EdgeLabelSetWithProperties" $
  doc "A GQL edge label set with properties" $
  T.record [
    "labelSet">: gql "EdgeTypeLabelSet",
    "propertyTypes">: gql "EdgeTypePropertyTypes"]

edgePattern :: TypeDefinition
edgePattern = def "EdgePattern" $
  doc "A GQL edge pattern: one of several alternative forms" $
  T.union [
    "fullEdge">: gql "FullEdgePattern",
    "abbreviatedEdge">: gql "AbbreviatedEdgePattern"]

edgeReference :: TypeDefinition
edgeReference = def "EdgeReference" $
  doc "A GQL edge reference, equivalent to a element variable reference" $
  gql "ElementVariableReference"

edgeReferenceValueExpression :: TypeDefinition
edgeReferenceValueExpression = def "EdgeReferenceValueExpression" $
  doc "A GQL edge reference value expression, equivalent to a primary value expression" $
  gql "PrimaryValueExpression"

edgeReferenceValueType :: TypeDefinition
edgeReferenceValueType = def "EdgeReferenceValueType" $
  doc "A GQL edge reference value type: one of several alternative forms" $
  T.union [
    "open">: gql "OpenEdgeReferenceValueType",
    "closed">: gql "ClosedEdgeReferenceValueType"]

edgeSynonym :: TypeDefinition
edgeSynonym = def "EdgeSynonym" $
  doc "A GQL edge synonym: one of a fixed set of alternatives" $
  T.enum ["edge", "relationship"]

edgeTypeFiller :: TypeDefinition
edgeTypeFiller = def "EdgeTypeFiller" $
  doc "A GQL edge type filler: one of several alternative forms" $
  T.union [
    "keyLabelSetWithContent">: gql "EdgeKeyLabelSetWithContent",
    "impliedContent">: gql "EdgeTypeImpliedContent"]

edgeTypeImpliedContent :: TypeDefinition
edgeTypeImpliedContent = def "EdgeTypeImpliedContent" $
  doc "A GQL edge type implied content: one of several alternative forms" $
  T.union [
    "labelSet">: gql "EdgeTypeLabelSet",
    "propertyTypes">: gql "EdgeTypePropertyTypes",
    "labelSetWithProperties">: gql "EdgeLabelSetWithProperties"]

edgeTypeKeyLabelSet :: TypeDefinition
edgeTypeKeyLabelSet = def "EdgeTypeKeyLabelSet" $
  doc "An optional GQL edge type key label set" $
  T.optional $ gql "LabelSetPhrase"

edgeTypeLabelSet :: TypeDefinition
edgeTypeLabelSet = def "EdgeTypeLabelSet" $
  doc "A GQL edge type label set, equivalent to a label set phrase" $
  gql "LabelSetPhrase"

edgeTypeName :: TypeDefinition
edgeTypeName = def "EdgeTypeName" $
  doc "A GQL edge type name" $
  T.string

edgeTypeNameWithFiller :: TypeDefinition
edgeTypeNameWithFiller = def "EdgeTypeNameWithFiller" $
  doc "A GQL edge type name with filler" $
  T.record [
    "typeName">: gql "EdgeTypeName",
    "filler">: T.optional $ gql "EdgeTypeFiller"]

edgeTypePattern :: TypeDefinition
edgeTypePattern = def "EdgeTypePattern" $
  doc "A GQL edge type pattern" $
  T.record [
    "kindAndSynonym">: T.optional $ gql "EdgeKindAndSynonym",
    "patternType">: gql "EdgeTypePatternType"]

edgeTypePatternDirected :: TypeDefinition
edgeTypePatternDirected = def "EdgeTypePatternDirected" $
  doc "A GQL edge type pattern directed: one of several alternative forms" $
  T.union [
    "pointingRight">: gql "EdgeTypePatternPointingRight",
    "pointingLeft">: gql "EdgeTypePatternPointingLeft"]

edgeTypePatternPointingLeft :: TypeDefinition
edgeTypePatternPointingLeft = def "EdgeTypePatternPointingLeft" $
  doc "A GQL edge type pattern pointing left" $
  T.record [
    "destination">: gql "DestinationNodeTypeReference",
    "arc">: gql "ArcTypePointingLeft",
    "source">: gql "SourceNodeTypeReference"]

edgeTypePatternPointingRight :: TypeDefinition
edgeTypePatternPointingRight = def "EdgeTypePatternPointingRight" $
  doc "A GQL edge type pattern pointing right" $
  T.record [
    "source">: gql "SourceNodeTypeReference",
    "arc">: gql "ArcTypePointingRight",
    "destination">: gql "DestinationNodeTypeReference"]

edgeTypePatternType :: TypeDefinition
edgeTypePatternType = def "EdgeTypePatternType" $
  doc "A GQL edge type pattern type: one of several alternative forms" $
  T.union [
    "directed">: gql "EdgeTypePatternDirected",
    "undirected">: gql "EdgeTypePatternUndirected"]

edgeTypePatternUndirected :: TypeDefinition
edgeTypePatternUndirected = def "EdgeTypePatternUndirected" $
  doc "A GQL edge type pattern undirected" $
  T.record [
    "source">: gql "SourceNodeTypeReference",
    "arc">: gql "ArcTypeUndirected",
    "destination">: gql "DestinationNodeTypeReference"]

edgeTypePhrase :: TypeDefinition
edgeTypePhrase = def "EdgeTypePhrase" $
  doc "A GQL edge type phrase" $
  T.record [
    "kind">: gql "EdgeKind",
    "synonym">: gql "EdgeSynonym",
    "typeNameAndFiller">: gql "EdgeTypePhraseFiller",
    "endpointPair">: gql "EndpointPairPhrase"]

edgeTypePhraseFiller :: TypeDefinition
edgeTypePhraseFiller = def "EdgeTypePhraseFiller" $
  doc "A GQL edge type phrase filler: one of several alternative forms" $
  T.union [
    "typeNameWithFiller">: gql "EdgeTypeNameWithFiller",
    "fillerOnly">: gql "EdgeTypeFiller"]

edgeTypePropertyTypes :: TypeDefinition
edgeTypePropertyTypes = def "EdgeTypePropertyTypes" $
  doc "A GQL edge type property types, equivalent to a property types specification" $
  gql "PropertyTypesSpecification"

edgeTypeSpecification :: TypeDefinition
edgeTypeSpecification = def "EdgeTypeSpecification" $
  doc "A GQL edge type specification: one of several alternative forms" $
  T.union [
    "pattern">: gql "EdgeTypePattern",
    "phrase">: gql "EdgeTypePhrase"]

edgesSynonym :: TypeDefinition
edgesSynonym = def "EdgesSynonym" $
  doc "A GQL edges synonym: one of a fixed set of alternatives" $
  T.enum ["edges", "relationships"]

elementBindingsOrElements :: TypeDefinition
elementBindingsOrElements = def "ElementBindingsOrElements" $
  doc "A GQL element bindings or elements: one of several alternative forms" $
  T.union [
    "elementBindings">: T.boolean,
    "elements">: T.unit]

elementIdFunction :: TypeDefinition
elementIdFunction = def "ElementIdFunction" $
  doc "A GQL element id function, equivalent to a element variable reference" $
  gql "ElementVariableReference"

elementPattern :: TypeDefinition
elementPattern = def "ElementPattern" $
  doc "A GQL element pattern: one of several alternative forms" $
  T.union [
    "node">: gql "NodePattern",
    "edge">: gql "EdgePattern"]

elementPatternFiller :: TypeDefinition
elementPatternFiller = def "ElementPatternFiller" $
  doc "A GQL element pattern filler" $
  T.record [
    "variableDeclaration">: T.optional $ gql "ElementVariableDeclaration",
    "isLabelExpression">: T.optional $ gql "IsLabelExpression",
    "predicate">: T.optional $ gql "ElementPatternPredicate"]

elementPatternPredicate :: TypeDefinition
elementPatternPredicate = def "ElementPatternPredicate" $
  doc "A GQL element pattern predicate: one of several alternative forms" $
  T.union [
    "whereClause">: gql "ElementPatternWhereClause",
    "propertySpecification">: gql "ElementPropertySpecification"]

elementPatternWhereClause :: TypeDefinition
elementPatternWhereClause = def "ElementPatternWhereClause" $
  doc "A GQL element pattern where clause, equivalent to a search condition" $
  gql "SearchCondition"

elementPropertySpecification :: TypeDefinition
elementPropertySpecification = def "ElementPropertySpecification" $
  doc "A GQL element property specification, equivalent to a property key value pair list" $
  gql "PropertyKeyValuePairList"

elementTypeList :: TypeDefinition
elementTypeList = def "ElementTypeList" $
  doc "A list of GQL element type list elements" $
  nonemptyList $ gql "ElementTypeSpecification"

elementTypeSpecification :: TypeDefinition
elementTypeSpecification = def "ElementTypeSpecification" $
  doc "A GQL element type specification: one of several alternative forms" $
  T.union [
    "nodeType">: gql "NodeTypeSpecification",
    "edgeType">: gql "EdgeTypeSpecification"]

elementVariable :: TypeDefinition
elementVariable = def "ElementVariable" $
  doc "A GQL element variable, equivalent to a binding variable" $
  gql "BindingVariable"

elementVariableDeclaration :: TypeDefinition
elementVariableDeclaration = def "ElementVariableDeclaration" $
  doc "A GQL element variable declaration" $
  T.record [
    "temp">: T.optional T.boolean,
    "variable">: gql "ElementVariable"]

elementVariableReference :: TypeDefinition
elementVariableReference = def "ElementVariableReference" $
  doc "A GQL element variable reference, equivalent to a binding variable reference" $
  gql "BindingVariableReference"

elementsFunction :: TypeDefinition
elementsFunction = def "ElementsFunction" $
  doc "A GQL elements function, equivalent to a path value expression" $
  gql "PathValueExpression"

elseClause :: TypeDefinition
elseClause = def "ElseClause" $
  doc "A GQL else clause, equivalent to a result" $
  gql "Result"

emptyType :: TypeDefinition
emptyType = def "EmptyType" $
  doc "A GQL empty type" $
  T.unit

endTransactionCommand :: TypeDefinition
endTransactionCommand = def "EndTransactionCommand" $
  doc "A GQL end transaction command: one of several alternative forms" $
  T.union [
    "rollback">: gql "RollbackCommand",
    "commit">: gql "CommitCommand"]

endpointPair :: TypeDefinition
endpointPair = def "EndpointPair" $
  doc "A GQL endpoint pair: one of several alternative forms" $
  T.union [
    "directedPair">: gql "EndpointPairDirected",
    "undirectedPair">: gql "EndpointPairUndirected"]

endpointPairDirected :: TypeDefinition
endpointPairDirected = def "EndpointPairDirected" $
  doc "A GQL endpoint pair directed: one of several alternative forms" $
  T.union [
    "pointingRight">: gql "EndpointPairPointingRight",
    "pointingLeft">: gql "EndpointPairPointingLeft"]

endpointPairPhrase :: TypeDefinition
endpointPairPhrase = def "EndpointPairPhrase" $
  doc "A GQL endpoint pair phrase, equivalent to a endpoint pair" $
  gql "EndpointPair"

endpointPairPointingLeft :: TypeDefinition
endpointPairPointingLeft = def "EndpointPairPointingLeft" $
  doc "A GQL endpoint pair pointing left" $
  T.record [
    "destinationAlias">: gql "DestinationNodeTypeAlias",
    "sourceAlias">: gql "SourceNodeTypeAlias"]

endpointPairPointingRight :: TypeDefinition
endpointPairPointingRight = def "EndpointPairPointingRight" $
  doc "A GQL endpoint pair pointing right" $
  T.record [
    "sourceAlias">: gql "SourceNodeTypeAlias",
    "connector">: gql "ConnectorPointingRight",
    "destinationAlias">: gql "DestinationNodeTypeAlias"]

endpointPairUndirected :: TypeDefinition
endpointPairUndirected = def "EndpointPairUndirected" $
  doc "A GQL endpoint pair undirected" $
  T.record [
    "sourceAlias">: gql "SourceNodeTypeAlias",
    "connector">: gql "ConnectorUndirected",
    "destinationAlias">: gql "DestinationNodeTypeAlias"]

exactNumericLiteral :: TypeDefinition
exactNumericLiteral = def "ExactNumericLiteral" $
  doc "A GQL exact numeric literal: one of several alternative forms" $
  T.union [
    "scientificWithSuffix">: T.string,
    "commonWithSuffix">: T.string,
    "commonWithoutSuffix">: T.string,
    "integerWithSuffix">: T.string,
    "unsignedInteger">: gql "UnsignedInteger"]

exactNumericType :: TypeDefinition
exactNumericType = def "ExactNumericType" $
  doc "A GQL exact numeric type: one of several alternative forms" $
  T.union [
    "binary">: gql "BinaryExactNumericType",
    "decimal">: gql "DecimalExactNumericType"]

existsPredicate :: TypeDefinition
existsPredicate = def "ExistsPredicate" $
  doc "A GQL exists predicate: one of several alternative forms" $
  T.union [
    "graphPatternBrace">: gql "GraphPattern",
    "graphPatternParen">: gql "GraphPattern",
    "matchBlockBrace">: gql "MatchStatementBlock",
    "matchBlockParen">: gql "MatchStatementBlock",
    "nestedQuery">: gql "NestedQuerySpecification"]

exponentialFunction :: TypeDefinition
exponentialFunction = def "ExponentialFunction" $
  doc "A GQL exponential function, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

field :: TypeDefinition
field = def "Field" $
  doc "A GQL field" $
  T.record [
    "name">: gql "FieldName",
    "value">: gql "ValueExpression"]

fieldList :: TypeDefinition
fieldList = def "FieldList" $
  doc "A list of GQL field list elements" $
  nonemptyList $ gql "Field"

fieldName_ :: TypeDefinition
fieldName_ = def "FieldName" $
  doc "A GQL field name" $
  T.string

fieldType :: TypeDefinition
fieldType = def "FieldType" $
  doc "A GQL field type" $
  T.record [
    "fieldName">: gql "FieldName",
    "typed">: T.optional $ gql "Typed",
    "valueType">: gql "ValueType"]

fieldTypeList :: TypeDefinition
fieldTypeList = def "FieldTypeList" $
  doc "A list of GQL field type list elements" $
  nonemptyList $ gql "FieldType"

fieldTypesSpecification :: TypeDefinition
fieldTypesSpecification = def "FieldTypesSpecification" $
  doc "An optional GQL field types specification" $
  T.optional $ gql "FieldTypeList"

fieldsSpecification :: TypeDefinition
fieldsSpecification = def "FieldsSpecification" $
  doc "An optional GQL fields specification" $
  T.optional $ gql "FieldList"

filterStatement :: TypeDefinition
filterStatement = def "FilterStatement" $
  doc "A GQL filter statement: one of several alternative forms" $
  T.union [
    "whereClause">: gql "WhereClause",
    "searchCondition">: gql "SearchCondition"]

fixedLength :: TypeDefinition
fixedLength = def "FixedLength" $
  doc "A GQL fixed length, equivalent to a unsigned integer" $
  gql "UnsignedInteger"

fixedQuantifier :: TypeDefinition
fixedQuantifier = def "FixedQuantifier" $
  doc "A GQL fixed quantifier, equivalent to a unsigned integer" $
  gql "UnsignedInteger"

float128Type :: TypeDefinition
float128Type = def "Float128Type" $
  doc "A GQL float128 type" $
  T.record [
    "notNull">: T.boolean]

float16Type :: TypeDefinition
float16Type = def "Float16Type" $
  doc "A GQL float16 type" $
  T.record [
    "notNull">: T.boolean]

float256Type :: TypeDefinition
float256Type = def "Float256Type" $
  doc "A GQL float256 type" $
  T.record [
    "notNull">: T.boolean]

float32Type :: TypeDefinition
float32Type = def "Float32Type" $
  doc "A GQL float32 type" $
  T.record [
    "notNull">: T.boolean]

float64Type :: TypeDefinition
float64Type = def "Float64Type" $
  doc "A GQL float64 type" $
  T.record [
    "notNull">: T.boolean]

floatTypeWithPrecision :: TypeDefinition
floatTypeWithPrecision = def "FloatTypeWithPrecision" $
  doc "A GQL float type with precision" $
  T.record [
    "precisionAndScale">: T.optional $ gql "PrecisionAndScale",
    "notNull">: T.boolean]

floorFunction :: TypeDefinition
floorFunction = def "FloorFunction" $
  doc "A GQL floor function, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

focusedLinearDataModifyingStatement :: TypeDefinition
focusedLinearDataModifyingStatement = def "FocusedLinearDataModifyingStatement" $
  doc "A GQL focused linear data modifying statement: one of several alternative forms" $
  T.union [
    "simple">: gql "FocusedLinearDataModifyingStatementBody",
    "nested">: gql "FocusedNestedDataModifyingProcedureSpecification"]

focusedLinearDataModifyingStatementBody :: TypeDefinition
focusedLinearDataModifyingStatementBody = def "FocusedLinearDataModifyingStatementBody" $
  doc "A GQL focused linear data modifying statement body" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "simpleAccess">: gql "SimpleLinearDataAccessingStatement",
    "primitiveResult">: T.optional $ gql "PrimitiveResultStatement"]

focusedLinearQueryAndPrimitiveResultStatementPart :: TypeDefinition
focusedLinearQueryAndPrimitiveResultStatementPart = def "FocusedLinearQueryAndPrimitiveResultStatementPart" $
  doc "A GQL focused linear query and primitive result statement part" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "simple">: gql "SimpleLinearQueryStatement",
    "primitiveResult">: gql "PrimitiveResultStatement"]

focusedLinearQueryStatement :: TypeDefinition
focusedLinearQueryStatement = def "FocusedLinearQueryStatement" $
  doc "A GQL focused linear query statement: one of several alternative forms" $
  T.union [
    "parts">: gql "FocusedLinearQueryStatementPartsAndResult",
    "primitive">: gql "FocusedPrimitiveResultStatement",
    "nested">: gql "FocusedNestedQuerySpecification",
    "select">: gql "SelectStatement"]

focusedLinearQueryStatementPart :: TypeDefinition
focusedLinearQueryStatementPart = def "FocusedLinearQueryStatementPart" $
  doc "A GQL focused linear query statement part" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "simple">: gql "SimpleLinearQueryStatement"]

focusedLinearQueryStatementPartsAndResult :: TypeDefinition
focusedLinearQueryStatementPartsAndResult = def "FocusedLinearQueryStatementPartsAndResult" $
  doc "A GQL focused linear query statement parts and result" $
  T.record [
    "parts">: T.list $ gql "FocusedLinearQueryStatementPart",
    "result">: gql "FocusedLinearQueryAndPrimitiveResultStatementPart"]

focusedNestedDataModifyingProcedureSpecification :: TypeDefinition
focusedNestedDataModifyingProcedureSpecification = def "FocusedNestedDataModifyingProcedureSpecification" $
  doc "A GQL focused nested data modifying procedure specification" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "nestedSpec">: gql "NestedDataModifyingProcedureSpecification"]

focusedNestedQuerySpecification :: TypeDefinition
focusedNestedQuerySpecification = def "FocusedNestedQuerySpecification" $
  doc "A GQL focused nested query specification" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "nested">: gql "NestedQuerySpecification"]

focusedPrimitiveResultStatement :: TypeDefinition
focusedPrimitiveResultStatement = def "FocusedPrimitiveResultStatement" $
  doc "A GQL focused primitive result statement" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "primitiveResult">: gql "PrimitiveResultStatement"]

foldCharacterString :: TypeDefinition
foldCharacterString = def "FoldCharacterString" $
  doc "A GQL fold character string" $
  T.record [
    "case">: gql "Case",
    "valueExpression">: gql "ValueExpression"]

forItem :: TypeDefinition
forItem = def "ForItem" $
  doc "A GQL for item" $
  T.record [
    "alias">: gql "ForItemAlias",
    "source">: gql "ForItemSource"]

forItemAlias :: TypeDefinition
forItemAlias = def "ForItemAlias" $
  doc "A GQL for item alias, equivalent to a binding variable" $
  gql "BindingVariable"

forItemSource :: TypeDefinition
forItemSource = def "ForItemSource" $
  doc "A GQL for item source, equivalent to a value expression" $
  gql "ValueExpression"

forOrdinalityOrOffset :: TypeDefinition
forOrdinalityOrOffset = def "ForOrdinalityOrOffset" $
  doc "A GQL for ordinality or offset" $
  T.record [
    "type">: gql "OrdinalityOrOffsetType",
    "variable">: gql "BindingVariable"]

forStatement :: TypeDefinition
forStatement = def "ForStatement" $
  doc "A GQL for statement" $
  T.record [
    "item">: gql "ForItem",
    "ordinalityOrOffset">: T.optional $ gql "ForOrdinalityOrOffset"]

fullEdgeAnyDirection :: TypeDefinition
fullEdgeAnyDirection = def "FullEdgeAnyDirection" $
  doc "A GQL full edge any direction, equivalent to a element pattern filler" $
  gql "ElementPatternFiller"

fullEdgeLeftOrRight :: TypeDefinition
fullEdgeLeftOrRight = def "FullEdgeLeftOrRight" $
  doc "A GQL full edge left or right, equivalent to a element pattern filler" $
  gql "ElementPatternFiller"

fullEdgeLeftOrUndirected :: TypeDefinition
fullEdgeLeftOrUndirected = def "FullEdgeLeftOrUndirected" $
  doc "A GQL full edge left or undirected, equivalent to a element pattern filler" $
  gql "ElementPatternFiller"

fullEdgePattern :: TypeDefinition
fullEdgePattern = def "FullEdgePattern" $
  doc "A GQL full edge pattern: one of several alternative forms" $
  T.union [
    "pointingLeft">: gql "FullEdgePointingLeft",
    "undirected">: gql "FullEdgeUndirected",
    "pointingRight">: gql "FullEdgePointingRight",
    "leftOrUndirected">: gql "FullEdgeLeftOrUndirected",
    "undirectedOrRight">: gql "FullEdgeUndirectedOrRight",
    "leftOrRight">: gql "FullEdgeLeftOrRight",
    "anyDirection">: gql "FullEdgeAnyDirection"]

fullEdgePointingLeft :: TypeDefinition
fullEdgePointingLeft = def "FullEdgePointingLeft" $
  doc "A GQL full edge pointing left, equivalent to a element pattern filler" $
  gql "ElementPatternFiller"

fullEdgePointingRight :: TypeDefinition
fullEdgePointingRight = def "FullEdgePointingRight" $
  doc "A GQL full edge pointing right, equivalent to a element pattern filler" $
  gql "ElementPatternFiller"

fullEdgeUndirected :: TypeDefinition
fullEdgeUndirected = def "FullEdgeUndirected" $
  doc "A GQL full edge undirected, equivalent to a element pattern filler" $
  gql "ElementPatternFiller"

fullEdgeUndirectedOrRight :: TypeDefinition
fullEdgeUndirectedOrRight = def "FullEdgeUndirectedOrRight" $
  doc "A GQL full edge undirected or right, equivalent to a element pattern filler" $
  gql "ElementPatternFiller"

generalLiteral :: TypeDefinition
generalLiteral = def "GeneralLiteral" $
  doc "A GQL general literal: one of several alternative forms" $
  T.union [
    "boolean">: gql "BooleanLiteral",
    "characterString">: gql "CharacterStringLiteral",
    "byteString">: gql "ByteStringLiteral",
    "temporal">: gql "TemporalLiteral",
    "duration">: gql "DurationLiteral",
    "nullLiteral">: gql "NullLiteral",
    "list">: gql "ListLiteral",
    "record">: gql "RecordLiteral"]

generalLogarithmArgument :: TypeDefinition
generalLogarithmArgument = def "GeneralLogarithmArgument" $
  doc "A GQL general logarithm argument, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

generalLogarithmBase :: TypeDefinition
generalLogarithmBase = def "GeneralLogarithmBase" $
  doc "A GQL general logarithm base, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

generalLogarithmFunction :: TypeDefinition
generalLogarithmFunction = def "GeneralLogarithmFunction" $
  doc "A GQL general logarithm function" $
  T.record [
    "base">: gql "GeneralLogarithmBase",
    "argument">: gql "GeneralLogarithmArgument"]

generalQuantifier :: TypeDefinition
generalQuantifier = def "GeneralQuantifier" $
  doc "A GQL general quantifier" $
  T.record [
    "lowerBound">: T.optional $ gql "LowerBound",
    "upperBound">: T.optional $ gql "UpperBound"]

generalSetFunction :: TypeDefinition
generalSetFunction = def "GeneralSetFunction" $
  doc "A GQL general set function" $
  T.record [
    "functionType">: gql "GeneralSetFunctionType",
    "setQuantifier">: T.optional $ gql "SetQuantifier",
    "valueExpression">: gql "ValueExpression"]

generalSetFunctionType :: TypeDefinition
generalSetFunctionType = def "GeneralSetFunctionType" $
  doc "A GQL general set function type: one of a fixed set of alternatives" $
  T.enum [
    "avg",
    "count",
    "max",
    "min",
    "sum",
    "collectList",
    "stddevSamp",
    "stddevPop"]

generalValueSpecification :: TypeDefinition
generalValueSpecification = def "GeneralValueSpecification" $
  doc "A GQL general value specification: one of several alternative forms" $
  T.union [
    "dynamicParameterSpecification">: gql "DynamicParameterSpecification",
    "sessionUser">: T.unit]

gql :: String -> Type
gql = typeref ns


gqlProgram :: TypeDefinition
gqlProgram = def "GqlProgram" $
  doc "A GQL gql program" $
  T.record [
    "activity">: T.optional $ gql "ProgramActivity",
    "close">: T.optional $ gql "SessionCloseCommand"]

graphAndNestedQuerySpecification :: TypeDefinition
graphAndNestedQuerySpecification = def "GraphAndNestedQuerySpecification" $
  doc "A GQL graph and nested query specification" $
  T.record [
    "graphExpression">: gql "GraphExpression",
    "nested">: gql "NestedQuerySpecification"]

graphExpression :: TypeDefinition
graphExpression = def "GraphExpression" $
  doc "A GQL graph expression: one of several alternative forms" $
  T.union [
    "object">: gql "ObjectExpressionPrimary",
    "reference">: gql "GraphReference",
    "name">: gql "ObjectNameOrBindingVariable",
    "current">: gql "CurrentGraph"]

graphInitializer :: TypeDefinition
graphInitializer = def "GraphInitializer" $
  doc "A GQL graph initializer" $
  T.unit

graphName :: TypeDefinition
graphName = def "GraphName" $
  doc "A GQL graph name" $
  T.string

graphPattern :: TypeDefinition
graphPattern = def "GraphPattern" $
  doc "A GQL graph pattern" $
  T.record [
    "matchMode">: T.optional $ gql "MatchMode",
    "pathPatterns">: gql "PathPatternList",
    "keepClause">: T.optional $ gql "KeepClause",
    "whereClause">: T.optional $ gql "GraphPatternWhereClause"]

graphPatternBindingTable :: TypeDefinition
graphPatternBindingTable = def "GraphPatternBindingTable" $
  doc "A GQL graph pattern binding table" $
  T.record [
    "pattern">: gql "GraphPattern",
    "yieldClause">: T.optional $ gql "GraphPatternYieldClause"]

graphPatternQuantifier :: TypeDefinition
graphPatternQuantifier = def "GraphPatternQuantifier" $
  doc "A GQL graph pattern quantifier: one of several alternative forms" $
  T.union [
    "asterisk">: T.unit,
    "plusSign">: T.unit,
    "fixed">: gql "FixedQuantifier",
    "general">: gql "GeneralQuantifier"]

graphPatternWhereClause :: TypeDefinition
graphPatternWhereClause = def "GraphPatternWhereClause" $
  doc "A GQL graph pattern where clause, equivalent to a search condition" $
  gql "SearchCondition"

graphPatternYieldClause :: TypeDefinition
graphPatternYieldClause = def "GraphPatternYieldClause" $
  doc "A GQL graph pattern yield clause, equivalent to a graph pattern yield item list" $
  gql "GraphPatternYieldItemList"

graphPatternYieldItem :: TypeDefinition
graphPatternYieldItem = def "GraphPatternYieldItem" $
  doc "A GQL graph pattern yield item, equivalent to a binding variable reference" $
  gql "BindingVariableReference"

graphPatternYieldItemList :: TypeDefinition
graphPatternYieldItemList = def "GraphPatternYieldItemList" $
  doc "A GQL graph pattern yield item list: one of several alternative forms" $
  T.union [
    "items">: nonemptyList $ gql "GraphPatternYieldItem",
    "noBindings">: T.unit]

graphReference :: TypeDefinition
graphReference = def "GraphReference" $
  doc "A GQL graph reference: one of several alternative forms" $
  T.union [
    "parentAndGraphName">: gql "ParentAndGraphName",
    "delimitedGraphName">: gql "DelimitedGraphName",
    "homeGraph">: gql "HomeGraph",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

graphReferenceValueType :: TypeDefinition
graphReferenceValueType = def "GraphReferenceValueType" $
  doc "A GQL graph reference value type: one of several alternative forms" $
  T.union [
    "open">: gql "OpenGraphReferenceValueType",
    "closed">: gql "ClosedGraphReferenceValueType"]

graphSource :: TypeDefinition
graphSource = def "GraphSource" $
  doc "A GQL graph source, equivalent to a graph expression" $
  gql "GraphExpression"

graphTypeLikeGraph :: TypeDefinition
graphTypeLikeGraph = def "GraphTypeLikeGraph" $
  doc "A GQL graph type like graph, equivalent to a graph expression" $
  gql "GraphExpression"

graphTypeName :: TypeDefinition
graphTypeName = def "GraphTypeName" $
  doc "A GQL graph type name" $
  T.string

graphTypeOption :: TypeDefinition
graphTypeOption = def "GraphTypeOption" $
  doc "A GQL graph type option: one of several alternative forms" $
  T.union [
    "openGraphType">: gql "OpenGraphType",
    "ofGraphType">: gql "OfGraphType"]

graphTypeReference :: TypeDefinition
graphTypeReference = def "GraphTypeReference" $
  doc "A GQL graph type reference: one of several alternative forms" $
  T.union [
    "parentAndTypeName">: gql "CatalogGraphTypeParentAndName",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

graphTypeSource :: TypeDefinition
graphTypeSource = def "GraphTypeSource" $
  doc "A GQL graph type source: one of several alternative forms" $
  T.union [
    "copyOf">: gql "CopyOfGraphType",
    "likeGraph">: gql "GraphTypeLikeGraph",
    "nestedSpecification">: gql "NestedGraphTypeSpecification"]

graphTypeSpecificationBody :: TypeDefinition
graphTypeSpecificationBody = def "GraphTypeSpecificationBody" $
  doc "A GQL graph type specification body, equivalent to a element type list" $
  gql "ElementTypeList"

graphVariableDefinition :: TypeDefinition
graphVariableDefinition = def "GraphVariableDefinition" $
  doc "A GQL graph variable definition" $
  T.record [
    "variable">: gql "BindingVariable",
    "initializer">: gql "OptTypedGraphInitializer"]

groupByClause :: TypeDefinition
groupByClause = def "GroupByClause" $
  doc "A GQL group by clause, equivalent to a grouping element list" $
  gql "GroupingElementList"

groupingElement :: TypeDefinition
groupingElement = def "GroupingElement" $
  doc "A GQL grouping element, equivalent to a binding variable reference" $
  gql "BindingVariableReference"

groupingElementList :: TypeDefinition
groupingElementList = def "GroupingElementList" $
  doc "A GQL grouping element list: one of several alternative forms" $
  T.union [
    "elements">: nonemptyList $ gql "GroupingElement",
    "emptySet">: T.unit]

havingClause :: TypeDefinition
havingClause = def "HavingClause" $
  doc "A GQL having clause, equivalent to a search condition" $
  gql "SearchCondition"

homeGraph :: TypeDefinition
homeGraph = def "HomeGraph" $
  doc "A GQL home graph: one of a fixed set of alternatives" $
  T.enum ["homePropertyGraph", "homeGraph"]

identifier :: TypeDefinition
identifier = def "Identifier" $
  doc "A GQL identifier" $
  T.string

immaterialValueType :: TypeDefinition
immaterialValueType = def "ImmaterialValueType" $
  doc "A GQL immaterial value type: one of several alternative forms" $
  T.union [
    "nullType">: gql "NullType",
    "emptyType">: gql "EmptyType"]

implies :: TypeDefinition
implies = def "Implies" $
  doc "A GQL implies: one of a fixed set of alternatives" $
  T.enum ["rightDoubleArrow", "implies"]

independentValueExpression :: TypeDefinition
independentValueExpression = def "IndependentValueExpression" $
  doc "A GQL independent value expression, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

inlineProcedureCall :: TypeDefinition
inlineProcedureCall = def "InlineProcedureCall" $
  doc "A GQL inline procedure call" $
  T.record [
    "scope">: T.optional $ gql "VariableScopeClause",
    "nested">: gql "NestedProcedureSpecification"]

insertEdgeAndNode :: TypeDefinition
insertEdgeAndNode = def "InsertEdgeAndNode" $
  doc "A GQL insert edge and node" $
  T.record [
    "edge">: gql "InsertEdgePattern",
    "node">: gql "InsertNodePattern"]

insertEdgePattern :: TypeDefinition
insertEdgePattern = def "InsertEdgePattern" $
  doc "A GQL insert edge pattern: one of several alternative forms" $
  T.union [
    "pointingLeft">: gql "InsertEdgePointingLeft",
    "pointingRight">: gql "InsertEdgePointingRight",
    "undirected">: gql "InsertEdgeUndirected"]

insertEdgePointingLeft :: TypeDefinition
insertEdgePointingLeft = def "InsertEdgePointingLeft" $
  doc "An optional GQL insert edge pointing left" $
  T.optional $ gql "InsertElementPatternFiller"

insertEdgePointingRight :: TypeDefinition
insertEdgePointingRight = def "InsertEdgePointingRight" $
  doc "An optional GQL insert edge pointing right" $
  T.optional $ gql "InsertElementPatternFiller"

insertEdgeUndirected :: TypeDefinition
insertEdgeUndirected = def "InsertEdgeUndirected" $
  doc "An optional GQL insert edge undirected" $
  T.optional $ gql "InsertElementPatternFiller"

insertElementPatternFiller :: TypeDefinition
insertElementPatternFiller = def "InsertElementPatternFiller" $
  doc "A GQL insert element pattern filler" $
  T.record [
    "variableDeclaration">: T.optional $ gql "ElementVariableDeclaration",
    "labelAndProperties">: T.optional $ gql "LabelAndPropertySetSpecification"]

insertGraphPattern :: TypeDefinition
insertGraphPattern = def "InsertGraphPattern" $
  doc "A GQL insert graph pattern, equivalent to a insert path pattern list" $
  gql "InsertPathPatternList"

insertNodePattern :: TypeDefinition
insertNodePattern = def "InsertNodePattern" $
  doc "An optional GQL insert node pattern" $
  T.optional $ gql "InsertElementPatternFiller"

insertPathPattern :: TypeDefinition
insertPathPattern = def "InsertPathPattern" $
  doc "A GQL insert path pattern" $
  T.record [
    "startNode">: gql "InsertNodePattern",
    "edgesAndNodes">: T.list $ gql "InsertEdgeAndNode"]

insertPathPatternList :: TypeDefinition
insertPathPatternList = def "InsertPathPatternList" $
  doc "A list of GQL insert path pattern list elements" $
  nonemptyList $ gql "InsertPathPattern"

insertStatement :: TypeDefinition
insertStatement = def "InsertStatement" $
  doc "A GQL insert statement, equivalent to a insert graph pattern" $
  gql "InsertGraphPattern"

int128Type :: TypeDefinition
int128Type = def "Int128Type" $
  doc "A GQL int128 type" $
  T.record ["notNull">: T.boolean]

int16Type :: TypeDefinition
int16Type = def "Int16Type" $
  doc "A GQL int16 type" $
  T.record ["notNull">: T.boolean]

int256Type :: TypeDefinition
int256Type = def "Int256Type" $
  doc "A GQL int256 type" $
  T.record ["notNull">: T.boolean]

int32Type :: TypeDefinition
int32Type = def "Int32Type" $
  doc "A GQL int32 type" $
  T.record ["notNull">: T.boolean]

int64Type :: TypeDefinition
int64Type = def "Int64Type" $
  doc "A GQL int64 type" $
  T.record ["notNull">: T.boolean]

int8Type :: TypeDefinition
int8Type = def "Int8Type" $
  doc "A GQL int8 type" $
  T.record ["notNull">: T.boolean]

intWithPrecision :: TypeDefinition
intWithPrecision = def "IntWithPrecision" $
  doc "A GQL int with precision" $
  T.record [
    "precision">: T.optional $ gql "Precision",
    "notNull">: T.boolean]

integer128Type :: TypeDefinition
integer128Type = def "Integer128Type" $
  doc "A GQL integer128 type" $
  T.record ["notNull">: T.boolean]

integer16Type :: TypeDefinition
integer16Type = def "Integer16Type" $
  doc "A GQL integer16 type" $
  T.record ["notNull">: T.boolean]

integer256Type :: TypeDefinition
integer256Type = def "Integer256Type" $
  doc "A GQL integer256 type" $
  T.record ["notNull">: T.boolean]

integer32Type :: TypeDefinition
integer32Type = def "Integer32Type" $
  doc "A GQL integer32 type" $
  T.record ["notNull">: T.boolean]

integer64Type :: TypeDefinition
integer64Type = def "Integer64Type" $
  doc "A GQL integer64 type" $
  T.record ["notNull">: T.boolean]

integer8Type :: TypeDefinition
integer8Type = def "Integer8Type" $
  doc "A GQL integer8 type" $
  T.record ["notNull">: T.boolean]

integerWithPrecision :: TypeDefinition
integerWithPrecision = def "IntegerWithPrecision" $
  doc "A GQL integer with precision" $
  T.record [
    "precision">: T.optional $ gql "Precision",
    "notNull">: T.boolean]

isLabelExpression :: TypeDefinition
isLabelExpression = def "IsLabelExpression" $
  doc "A GQL is label expression" $
  T.record [
    "isOrColon">: gql "IsOrColon",
    "label">: gql "LabelExpression"]

isLabeledOrColon :: TypeDefinition
isLabeledOrColon = def "IsLabeledOrColon" $
  doc "A GQL is labeled or colon: one of several alternative forms" $
  T.union [
    "not">: T.boolean,
    "colon">: T.unit]

isNotExpr :: TypeDefinition
isNotExpr = def "IsNotExpr" $
  doc "A GQL is not expression" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "not">: T.boolean,
    "truthValue">: gql "TruthValue"]

isOrColon :: TypeDefinition
isOrColon = def "IsOrColon" $
  doc "A GQL is or colon: one of a fixed set of alternatives" $
  T.enum ["is", "colon"]

isOrColonWithLabels :: TypeDefinition
isOrColonWithLabels = def "IsOrColonWithLabels" $
  doc "A GQL is or colon with labels" $
  T.record [
    "isOrColon">: gql "IsOrColon",
    "labels">: gql "LabelSetSpecification"]

keepClause :: TypeDefinition
keepClause = def "KeepClause" $
  doc "A GQL keep clause, equivalent to a path pattern prefix" $
  gql "PathPatternPrefix"

labelAndPropertySetSpecification :: TypeDefinition
labelAndPropertySetSpecification = def "LabelAndPropertySetSpecification" $
  doc "A GQL label and property set specification" $
  T.record [
    "isOrColon">: T.optional $ gql "IsOrColon",
    "labelSet">: T.optional $ gql "LabelSetSpecification",
    "propertySpecification">: T.optional $ gql "ElementPropertySpecification"]

labelExpression :: TypeDefinition
labelExpression = def "LabelExpression" $
  doc "A GQL label expression: one of several alternative forms" $
  T.union [
    "negation">: gql "LabelExpression",
    "conjunction">: gql "ConjunctionLabelExpression",
    "disjunction">: gql "DisjunctionLabelExpression",
    "name">: gql "LabelName",
    "wildcard">: T.unit,
    "parenthesized">: gql "LabelExpression"]

labelName :: TypeDefinition
labelName = def "LabelName" $
  doc "A GQL label name" $
  T.string

labelSetPhrase :: TypeDefinition
labelSetPhrase = def "LabelSetPhrase" $
  doc "A GQL label set phrase: one of several alternative forms" $
  T.union [
    "singleLabel">: gql "LabelName",
    "multipleLabels">: gql "LabelSetSpecification",
    "isOrColonWithLabels">: gql "IsOrColonWithLabels"]

labelSetSpecification :: TypeDefinition
labelSetSpecification = def "LabelSetSpecification" $
  doc "A list of GQL label set specification elements" $
  nonemptyList $ gql "LabelName"

labeledPredicate :: TypeDefinition
labeledPredicate = def "LabeledPredicate" $
  doc "A GQL labeled predicate" $
  T.record [
    "elementVariableReference">: gql "ElementVariableReference",
    "labeledPart">: gql "LabeledPredicatePart2"]

labeledPredicatePart2 :: TypeDefinition
labeledPredicatePart2 = def "LabeledPredicatePart2" $
  doc "A GQL labeled predicate part2" $
  T.record [
    "isLabeledOrColon">: gql "IsLabeledOrColon",
    "labelExpression">: gql "LabelExpression"]

lengthExpression :: TypeDefinition
lengthExpression = def "LengthExpression" $
  doc "A GQL length expression: one of several alternative forms" $
  T.union [
    "char">: gql "CharLengthExpression",
    "byte">: gql "ByteLengthExpression",
    "path">: gql "PathLengthExpression"]

letStatement :: TypeDefinition
letStatement = def "LetStatement" $
  doc "A GQL let statement, equivalent to a let variable definition list" $
  gql "LetVariableDefinitionList"

letValueExpression :: TypeDefinition
letValueExpression = def "LetValueExpression" $
  doc "A GQL let value expression" $
  T.record [
    "letVariables">: gql "LetVariableDefinitionList",
    "valueExpression">: gql "ValueExpression"]

letVariableDefinition :: TypeDefinition
letVariableDefinition = def "LetVariableDefinition" $
  doc "A GQL let variable definition: one of several alternative forms" $
  T.union [
    "valueVariable">: gql "ValueVariableDefinition",
    "bindingEqualsValue">: gql "BindingEqualsValue"]

letVariableDefinitionList :: TypeDefinition
letVariableDefinitionList = def "LetVariableDefinitionList" $
  doc "A list of GQL let variable definition list elements" $
  nonemptyList $ gql "LetVariableDefinition"

limitClause :: TypeDefinition
limitClause = def "LimitClause" $
  doc "A GQL limit clause, equivalent to a non negative integer specification" $
  gql "NonNegativeIntegerSpecification"

linearCatalogModifyingStatement :: TypeDefinition
linearCatalogModifyingStatement = def "LinearCatalogModifyingStatement" $
  doc "A list of GQL linear catalog modifying statement elements" $
  nonemptyList $ gql "SimpleCatalogModifyingStatement"

linearDataModifyingStatement :: TypeDefinition
linearDataModifyingStatement = def "LinearDataModifyingStatement" $
  doc "A GQL linear data modifying statement: one of several alternative forms" $
  T.union [
    "focused">: gql "FocusedLinearDataModifyingStatement",
    "ambient">: gql "AmbientLinearDataModifyingStatement"]

linearQueryStatement :: TypeDefinition
linearQueryStatement = def "LinearQueryStatement" $
  doc "A GQL linear query statement: one of several alternative forms" $
  T.union [
    "focused">: gql "FocusedLinearQueryStatement",
    "ambient">: gql "AmbientLinearQueryStatement"]

listElement :: TypeDefinition
listElement = def "ListElement" $
  doc "A GQL list element, equivalent to a value expression" $
  gql "ValueExpression"

listElementList :: TypeDefinition
listElementList = def "ListElementList" $
  doc "A list of GQL list element list elements" $
  nonemptyList $ gql "ListElement"

listLiteral :: TypeDefinition
listLiteral = def "ListLiteral" $
  doc "A GQL list literal, equivalent to a list value constructor by enumeration" $
  gql "ListValueConstructorByEnumeration"

listValueConstructor :: TypeDefinition
listValueConstructor = def "ListValueConstructor" $
  doc "A GQL list value constructor, equivalent to a list value constructor by enumeration" $
  gql "ListValueConstructorByEnumeration"

listValueConstructorByEnumeration :: TypeDefinition
listValueConstructorByEnumeration = def "ListValueConstructorByEnumeration" $
  doc "A GQL list value constructor by enumeration" $
  T.record [
    "listValueTypeName">: T.optional $ gql "ListValueTypeName",
    "elements">: T.optional $ gql "ListElementList"]

listValueExpression :: TypeDefinition
listValueExpression = def "ListValueExpression" $
  doc "A GQL list value expression, equivalent to a value expression" $
  gql "ValueExpression"

listValueFunction :: TypeDefinition
listValueFunction = def "ListValueFunction" $
  doc "A GQL list value function: one of several alternative forms" $
  T.union [
    "trim">: gql "TrimListFunction",
    "elements">: gql "ElementsFunction"]

listValueTypeAlt1 :: TypeDefinition
listValueTypeAlt1 = def "ListValueTypeAlt1" $
  doc "A GQL list value type alt1" $
  T.record [
    "typeName">: gql "ListValueTypeName",
    "valueType">: gql "ValueType",
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

listValueTypeAlt2 :: TypeDefinition
listValueTypeAlt2 = def "ListValueTypeAlt2" $
  doc "A GQL list value type alt2" $
  T.record [
    "valueType">: gql "ValueType",
    "typeName">: gql "ListValueTypeName",
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

listValueTypeAlt3 :: TypeDefinition
listValueTypeAlt3 = def "ListValueTypeAlt3" $
  doc "A GQL list value type alt3" $
  T.record [
    "typeName">: gql "ListValueTypeName",
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

listValueTypeName :: TypeDefinition
listValueTypeName = def "ListValueTypeName" $
  doc "A GQL list value type name" $
  T.record [
    "group">: T.boolean,
    "synonym">: gql "ListValueTypeNameSynonym"]

listValueTypeNameSynonym :: TypeDefinition
listValueTypeNameSynonym = def "ListValueTypeNameSynonym" $
  doc "A GQL list value type name synonym: one of a fixed set of alternatives" $
  T.enum ["list", "array"]

localDatetimeType :: TypeDefinition
localDatetimeType = def "LocalDatetimeType" $
  doc "A GQL local datetime type" $
  T.record [
    "notNull">: T.boolean]

localDatetimeTypeChoice :: TypeDefinition
localDatetimeTypeChoice = def "LocalDatetimeTypeChoice" $
  doc "A GQL local datetime type choice: one of several alternative forms" $
  T.union [
    "localDatetime">: gql "LocalDatetimeType",
    "timestampWithoutTimeZone">: gql "TimestampWithoutTimeZoneType"]

localNodeTypeAlias :: TypeDefinition
localNodeTypeAlias = def "LocalNodeTypeAlias" $
  doc "A GQL local node type alias" $
  T.string

localTimeType :: TypeDefinition
localTimeType = def "LocalTimeType" $
  doc "A GQL local time type" $
  T.record [
    "notNull">: T.boolean]

localTimeTypeChoice :: TypeDefinition
localTimeTypeChoice = def "LocalTimeTypeChoice" $
  doc "A GQL local time type choice: one of several alternative forms" $
  T.union [
    "localTime">: gql "LocalTimeType",
    "timeWithoutTimeZone">: gql "TimeWithoutTimeZoneType"]

localdatetimeFunction :: TypeDefinition
localdatetimeFunction = def "LocaldatetimeFunction" $
  doc "A GQL localdatetime function: one of several alternative forms" $
  T.union [
    "localTimestamp">: T.unit,
    "localDatetimeWithParams">: T.optional $ gql "DatetimeFunctionParameters"]

localtimeFunction :: TypeDefinition
localtimeFunction = def "LocaltimeFunction" $
  doc "An optional GQL localtime function" $
  T.optional $ gql "TimeFunctionParameters"

lowerBound :: TypeDefinition
lowerBound = def "LowerBound" $
  doc "A GQL lower bound, equivalent to a unsigned integer" $
  gql "UnsignedInteger"

matchMode :: TypeDefinition
matchMode = def "MatchMode" $
  doc "A GQL match mode: one of several alternative forms" $
  T.union [
    "repeatableElements">: gql "RepeatableElementsMatchMode",
    "differentEdges">: gql "DifferentEdgesMatchMode"]

matchStatement :: TypeDefinition
matchStatement = def "MatchStatement" $
  doc "A GQL match statement: one of several alternative forms" $
  T.union [
    "simple">: gql "SimpleMatchStatement",
    "optional">: gql "OptionalMatchStatement"]

matchStatementBlock :: TypeDefinition
matchStatementBlock = def "MatchStatementBlock" $
  doc "A list of GQL match statement block elements" $
  nonemptyList $ gql "MatchStatement"

maxLength :: TypeDefinition
maxLength = def "MaxLength" $
  doc "A GQL max length, equivalent to a unsigned integer" $
  gql "UnsignedInteger"

minLength :: TypeDefinition
minLength = def "MinLength" $
  doc "A GQL min length, equivalent to a unsigned integer" $
  gql "UnsignedInteger"

modulusExpression :: TypeDefinition
modulusExpression = def "ModulusExpression" $
  doc "A GQL modulus expression" $
  T.record [
    "dividend">: gql "NumericValueExpressionDividend",
    "divisor">: gql "NumericValueExpressionDivisor"]

mulDivNumericValueExpression :: TypeDefinition
mulDivNumericValueExpression = def "MulDivNumericValueExpression" $
  doc "A GQL mul div numeric value expression" $
  T.record [
    "left">: gql "NumericValueExpression",
    "operator">: gql "MultDivOperator",
    "right">: gql "NumericValueExpression"]

multDivExpr :: TypeDefinition
multDivExpr = def "MultDivExpr" $
  doc "A GQL mult div expression" $
  T.record [
    "left">: gql "ValueExpression",
    "operator">: gql "MultDivOperator",
    "right">: gql "ValueExpression"]

multDivOperator :: TypeDefinition
multDivOperator = def "MultDivOperator" $
  doc "A GQL mult div operator: one of a fixed set of alternatives" $
  T.enum ["multiply", "divide"]

namedProcedureCall :: TypeDefinition
namedProcedureCall = def "NamedProcedureCall" $
  doc "A GQL named procedure call" $
  T.record [
    "reference">: gql "ProcedureReference",
    "arguments">: T.optional $ gql "ProcedureArgumentList",
    "yield">: T.optional $ gql "YieldClause"]

naturalLogarithm :: TypeDefinition
naturalLogarithm = def "NaturalLogarithm" $
  doc "A GQL natural logarithm, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

nestedBindingTableQuerySpecification :: TypeDefinition
nestedBindingTableQuerySpecification = def "NestedBindingTableQuerySpecification" $
  doc "A GQL nested binding table query specification" $
  T.unit

nestedDataModifyingProcedureSpecification :: TypeDefinition
nestedDataModifyingProcedureSpecification = def "NestedDataModifyingProcedureSpecification" $
  doc "A GQL nested data modifying procedure specification, equivalent to a procedure body" $
  gql "ProcedureBody"

nestedGraphTypeSpecification :: TypeDefinition
nestedGraphTypeSpecification = def "NestedGraphTypeSpecification" $
  doc "A GQL nested graph type specification, equivalent to a graph type specification body" $
  gql "GraphTypeSpecificationBody"

nestedProcedureSpecification :: TypeDefinition
nestedProcedureSpecification = def "NestedProcedureSpecification" $
  doc "A GQL nested procedure specification, equivalent to a procedure specification" $
  gql "ProcedureSpecification"

nestedQuerySpecification :: TypeDefinition
nestedQuerySpecification = def "NestedQuerySpecification" $
  doc "A GQL nested query specification, equivalent to a procedure body" $
  gql "ProcedureBody"

nextStatement :: TypeDefinition
nextStatement = def "NextStatement" $
  doc "A GQL next statement" $
  T.record [
    "yieldClause">: T.optional $ gql "YieldClause",
    "statement">: gql "Statement"]

nodeKeyLabelSetWithContent :: TypeDefinition
nodeKeyLabelSetWithContent = def "NodeKeyLabelSetWithContent" $
  doc "A GQL node key label set with content" $
  T.record [
    "keyLabelSet">: gql "NodeTypeKeyLabelSet",
    "impliedContent">: T.optional $ gql "NodeTypeImpliedContent"]

nodeLabelSetWithProperties :: TypeDefinition
nodeLabelSetWithProperties = def "NodeLabelSetWithProperties" $
  doc "A GQL node label set with properties" $
  T.record [
    "labelSet">: gql "NodeTypeLabelSet",
    "propertyTypes">: gql "NodeTypePropertyTypes"]

nodePattern :: TypeDefinition
nodePattern = def "NodePattern" $
  doc "A GQL node pattern, equivalent to a element pattern filler" $
  gql "ElementPatternFiller"

nodeReference :: TypeDefinition
nodeReference = def "NodeReference" $
  doc "A GQL node reference, equivalent to a element variable reference" $
  gql "ElementVariableReference"

nodeReferenceValueExpression :: TypeDefinition
nodeReferenceValueExpression = def "NodeReferenceValueExpression" $
  doc "A GQL node reference value expression, equivalent to a primary value expression" $
  gql "PrimaryValueExpression"

nodeReferenceValueType :: TypeDefinition
nodeReferenceValueType = def "NodeReferenceValueType" $
  doc "A GQL node reference value type: one of several alternative forms" $
  T.union [
    "open">: gql "OpenNodeReferenceValueType",
    "closed">: gql "ClosedNodeReferenceValueType"]

nodeSynonym :: TypeDefinition
nodeSynonym = def "NodeSynonym" $
  doc "A GQL node synonym: one of a fixed set of alternatives" $
  T.enum ["node", "vertex"]

nodeSynonymAndTypeName :: TypeDefinition
nodeSynonymAndTypeName = def "NodeSynonymAndTypeName" $
  doc "A GQL node synonym and type name" $
  T.record [
    "nodeSynonym">: gql "NodeSynonym",
    "typeName">: T.optional $ gql "NodeTypeName"]

nodeTypeFiller :: TypeDefinition
nodeTypeFiller = def "NodeTypeFiller" $
  doc "A GQL node type filler: one of several alternative forms" $
  T.union [
    "keyLabelSet">: gql "NodeKeyLabelSetWithContent",
    "impliedContent">: gql "NodeTypeImpliedContent"]

nodeTypeImpliedContent :: TypeDefinition
nodeTypeImpliedContent = def "NodeTypeImpliedContent" $
  doc "A GQL node type implied content: one of several alternative forms" $
  T.union [
    "labelSet">: gql "NodeTypeLabelSet",
    "propertyTypes">: gql "NodeTypePropertyTypes",
    "labelSetWithProperties">: gql "NodeLabelSetWithProperties"]

nodeTypeKeyLabelSet :: TypeDefinition
nodeTypeKeyLabelSet = def "NodeTypeKeyLabelSet" $
  doc "An optional GQL node type key label set" $
  T.optional $ gql "LabelSetPhrase"

nodeTypeLabelSet :: TypeDefinition
nodeTypeLabelSet = def "NodeTypeLabelSet" $
  doc "A GQL node type label set, equivalent to a label set phrase" $
  gql "LabelSetPhrase"

nodeTypeName :: TypeDefinition
nodeTypeName = def "NodeTypeName" $
  doc "A GQL node type name" $
  T.string

nodeTypeNameWithFiller :: TypeDefinition
nodeTypeNameWithFiller = def "NodeTypeNameWithFiller" $
  doc "A GQL node type name with filler" $
  T.record [
    "typeName">: gql "NodeTypeName",
    "filler">: T.optional $ gql "NodeTypeFiller"]

nodeTypePattern :: TypeDefinition
nodeTypePattern = def "NodeTypePattern" $
  doc "A GQL node type pattern" $
  T.record [
    "synonymAndTypeName">: T.optional $ gql "NodeSynonymAndTypeName",
    "alias">: T.optional $ gql "LocalNodeTypeAlias",
    "filler">: T.optional $ gql "NodeTypeFiller"]

nodeTypePhrase :: TypeDefinition
nodeTypePhrase = def "NodeTypePhrase" $
  doc "A GQL node type phrase" $
  T.record [
    "synonym">: gql "NodeSynonym",
    "typePhraseFiller">: gql "NodeTypePhraseFiller",
    "alias">: T.optional $ gql "LocalNodeTypeAlias"]

nodeTypePhraseFiller :: TypeDefinition
nodeTypePhraseFiller = def "NodeTypePhraseFiller" $
  doc "A GQL node type phrase filler: one of several alternative forms" $
  T.union [
    "typeName">: gql "NodeTypeNameWithFiller",
    "fillerOnly">: gql "NodeTypeFiller"]

nodeTypePropertyTypes :: TypeDefinition
nodeTypePropertyTypes = def "NodeTypePropertyTypes" $
  doc "A GQL node type property types, equivalent to a property types specification" $
  gql "PropertyTypesSpecification"

nodeTypeSpecification :: TypeDefinition
nodeTypeSpecification = def "NodeTypeSpecification" $
  doc "A GQL node type specification: one of several alternative forms" $
  T.union [
    "pattern">: gql "NodeTypePattern",
    "phrase">: gql "NodeTypePhrase"]

nonNegativeIntegerSpecification :: TypeDefinition
nonNegativeIntegerSpecification = def "NonNegativeIntegerSpecification" $
  doc "A GQL non negative integer specification: one of several alternative forms" $
  T.union [
    "unsignedInteger">: gql "UnsignedInteger",
    "dynamicParameterSpecification">: gql "DynamicParameterSpecification"]

nonParenthesizedPrimaryValueExpression :: TypeDefinition
nonParenthesizedPrimaryValueExpression = def "NonParenthesizedPrimaryValueExpression" $
  doc "A GQL non parenthesized primary value expression: one of several alternative forms" $
  T.union [
    "special">: gql "NonParenthesizedPrimaryValueExpressionSpecialCase",
    "bindingVariable">: gql "BindingVariableReference"]

nonParenthesizedPrimaryValueExpressionSpecialCase :: TypeDefinition
nonParenthesizedPrimaryValueExpressionSpecialCase = def "NonParenthesizedPrimaryValueExpressionSpecialCase" $
  doc "A GQL non parenthesized primary value expression special case: one of several alternative forms" $
  T.union [
    "aggregateFunction">: gql "AggregateFunction",
    "unsignedValueSpecification">: gql "UnsignedValueSpecification",
    "pathValueConstructor">: gql "PathValueConstructor",
    "propertyReference">: gql "PropertyReference",
    "valueQueryExpression">: gql "ValueQueryExpression",
    "caseExpression">: gql "CaseExpression",
    "castSpecification">: gql "CastSpecification",
    "elementIdFunction">: gql "ElementIdFunction",
    "letValueExpression">: gql "LetValueExpression"]

normalForm :: TypeDefinition
normalForm = def "NormalForm" $
  doc "A GQL normal form: one of a fixed set of alternatives" $
  T.enum ["nfc", "nfd", "nfkc", "nfkd"]

normalizeCharacterString :: TypeDefinition
normalizeCharacterString = def "NormalizeCharacterString" $
  doc "A GQL normalize character string" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "normalForm">: T.optional $ gql "NormalForm"]

normalizedPredicateExpr :: TypeDefinition
normalizedPredicateExpr = def "NormalizedPredicateExpr" $
  doc "A GQL normalized predicate expression" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "normalizedPredicate">: gql "NormalizedPredicatePart2"]

normalizedPredicatePart2 :: TypeDefinition
normalizedPredicatePart2 = def "NormalizedPredicatePart2" $
  doc "A GQL normalized predicate part2" $
  T.record [
    "not">: T.boolean,
    "normalForm">: T.optional $ gql "NormalForm"]

notExpr :: TypeDefinition
notExpr = def "NotExpr" $
  doc "A GQL not expression, equivalent to a value expression" $
  gql "ValueExpression"

notNull :: TypeDefinition
notNull = def "NotNull" $
  doc "A GQL not null" $
  T.unit

nullIfAbbreviation :: TypeDefinition
nullIfAbbreviation = def "NullIfAbbreviation" $
  doc "A GQL null if abbreviation" $
  T.record [
    "first">: gql "ValueExpression",
    "second">: gql "ValueExpression"]

nullLiteral :: TypeDefinition
nullLiteral = def "NullLiteral" $
  doc "A GQL null literal" $
  T.unit

nullOrdering :: TypeDefinition
nullOrdering = def "NullOrdering" $
  doc "A GQL null ordering: one of a fixed set of alternatives" $
  T.enum ["nullsFirst", "nullsLast"]

nullPredicate :: TypeDefinition
nullPredicate = def "NullPredicate" $
  doc "A GQL null predicate" $
  T.record [
    "valueExpression">: gql "PrimaryValueExpression",
    "nullPart">: gql "NullPredicatePart2"]

nullPredicatePart2 :: TypeDefinition
nullPredicatePart2 = def "NullPredicatePart2" $
  doc "A GQL null predicate part2" $
  T.record [
    "not">: T.boolean]

nullType :: TypeDefinition
nullType = def "NullType" $
  doc "A GQL null type" $
  T.unit

numberOfGroups :: TypeDefinition
numberOfGroups = def "NumberOfGroups" $
  doc "A GQL number of groups, equivalent to a non negative integer specification" $
  gql "NonNegativeIntegerSpecification"

numberOfPaths :: TypeDefinition
numberOfPaths = def "NumberOfPaths" $
  doc "A GQL number of paths, equivalent to a non negative integer specification" $
  gql "NonNegativeIntegerSpecification"

numericType :: TypeDefinition
numericType = def "NumericType" $
  doc "A GQL numeric type: one of several alternative forms" $
  T.union [
    "exact">: gql "ExactNumericType",
    "approximate">: gql "ApproximateNumericType"]

numericValueExpression :: TypeDefinition
numericValueExpression = def "NumericValueExpression" $
  doc "A GQL numeric value expression: one of several alternative forms" $
  T.union [
    "signed">: gql "SignedNumericValueExpression",
    "multiplicationOrDivision">: gql "MulDivNumericValueExpression",
    "additionOrSubtraction">: gql "AddSubNumericValueExpression",
    "primary">: gql "PrimaryValueExpression",
    "function">: gql "NumericValueFunction"]

numericValueExpressionBase :: TypeDefinition
numericValueExpressionBase = def "NumericValueExpressionBase" $
  doc "A GQL numeric value expression base, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

numericValueExpressionDividend :: TypeDefinition
numericValueExpressionDividend = def "NumericValueExpressionDividend" $
  doc "A GQL numeric value expression dividend, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

numericValueExpressionDivisor :: TypeDefinition
numericValueExpressionDivisor = def "NumericValueExpressionDivisor" $
  doc "A GQL numeric value expression divisor, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

numericValueExpressionExponent :: TypeDefinition
numericValueExpressionExponent = def "NumericValueExpressionExponent" $
  doc "A GQL numeric value expression exponent, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

numericValueFunction :: TypeDefinition
numericValueFunction = def "NumericValueFunction" $
  doc "A GQL numeric value function: one of several alternative forms" $
  T.union [
    "length">: gql "LengthExpression",
    "cardinality">: gql "CardinalityExpression",
    "absoluteValue">: gql "AbsoluteValueExpression",
    "modulus">: gql "ModulusExpression",
    "trigonometric">: gql "TrigonometricFunction",
    "logarithm">: gql "GeneralLogarithmFunction",
    "commonLogarithm">: gql "CommonLogarithm",
    "naturalLogarithm">: gql "NaturalLogarithm",
    "exponential">: gql "ExponentialFunction",
    "power">: gql "PowerFunction",
    "squareRoot">: gql "SquareRoot",
    "floor">: gql "FloorFunction",
    "ceiling">: gql "CeilingFunction"]

objectExpressionPrimary :: TypeDefinition
objectExpressionPrimary = def "ObjectExpressionPrimary" $
  doc "A GQL object expression primary: one of several alternative forms" $
  T.union [
    "variable">: gql "PrimaryValueExpression",
    "parenthesized">: gql "ParenthesizedValueExpression",
    "nonParenthesized">: gql "NonParenthesizedPrimaryValueExpressionSpecialCase"]

objectName :: TypeDefinition
objectName = def "ObjectName" $
  doc "A GQL object name" $
  T.string

objectNameOrBindingVariable :: TypeDefinition
objectNameOrBindingVariable = def "ObjectNameOrBindingVariable" $
  doc "A GQL object name or binding variable" $
  T.string

ofGraphType :: TypeDefinition
ofGraphType = def "OfGraphType" $
  doc "A GQL of graph type: one of several alternative forms" $
  T.union [
    "likeGraph">: gql "GraphTypeLikeGraph",
    "reference">: gql "TypedGraphTypeReference",
    "nested">: gql "TypedNestedGraphTypeSpecification"]

offsetAndOptionalLimit :: TypeDefinition
offsetAndOptionalLimit = def "OffsetAndOptionalLimit" $
  doc "A GQL offset and optional limit" $
  T.record [
    "offset">: gql "OffsetClause",
    "limit">: T.optional $ gql "LimitClause"]

offsetClause :: TypeDefinition
offsetClause = def "OffsetClause" $
  doc "A GQL offset clause" $
  T.record [
    "synonym">: gql "OffsetSynonym",
    "value">: gql "NonNegativeIntegerSpecification"]

offsetSynonym :: TypeDefinition
offsetSynonym = def "OffsetSynonym" $
  doc "A GQL offset synonym: one of a fixed set of alternatives" $
  T.enum ["offset", "skipReservedWord"]

openDynamicUnionType :: TypeDefinition
openDynamicUnionType = def "OpenDynamicUnionType" $
  doc "A GQL open dynamic union type" $
  T.record [
    "value">: T.boolean,
    "notNull">: T.boolean]

openEdgeReferenceValueType :: TypeDefinition
openEdgeReferenceValueType = def "OpenEdgeReferenceValueType" $
  doc "A GQL open edge reference value type" $
  T.record [
    "any">: T.boolean,
    "edgeSynonym">: gql "EdgeSynonym",
    "notNull">: T.boolean]

openGraphReferenceValueType :: TypeDefinition
openGraphReferenceValueType = def "OpenGraphReferenceValueType" $
  doc "A GQL open graph reference value type" $
  T.record [
    "any">: T.optional T.boolean,
    "property">: T.boolean,
    "notNull">: T.boolean]

openGraphType :: TypeDefinition
openGraphType = def "OpenGraphType" $
  doc "A GQL open graph type" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "graph">: T.boolean]

openNodeReferenceValueType :: TypeDefinition
openNodeReferenceValueType = def "OpenNodeReferenceValueType" $
  doc "A GQL open node reference value type" $
  T.record [
    "any">: T.boolean,
    "nodeSynonym">: gql "NodeSynonym",
    "notNull">: T.boolean]

optTypedBindingTableInitializer :: TypeDefinition
optTypedBindingTableInitializer = def "OptTypedBindingTableInitializer" $
  doc "A GQL opt typed binding table initializer" $
  T.record [
    "type">: T.optional $ gql "TypedBindingTableReferenceValueType",
    "initializer">: gql "BindingTableInitializer"]

optTypedGraphInitializer :: TypeDefinition
optTypedGraphInitializer = def "OptTypedGraphInitializer" $
  doc "A GQL opt typed graph initializer" $
  T.record [
    "type">: T.optional $ gql "TypedGraphReferenceValueType",
    "initializer">: gql "GraphInitializer"]

optTypedValueInitializer :: TypeDefinition
optTypedValueInitializer = def "OptTypedValueInitializer" $
  doc "A GQL opt typed value initializer" $
  T.record [
    "type">: T.optional $ gql "TypedValueType",
    "initializer">: gql "ValueInitializer"]

optionalMatchStatement :: TypeDefinition
optionalMatchStatement = def "OptionalMatchStatement" $
  doc "A GQL optional match statement, equivalent to a optional operand" $
  gql "OptionalOperand"

optionalOperand :: TypeDefinition
optionalOperand = def "OptionalOperand" $
  doc "A GQL optional operand: one of several alternative forms" $
  T.union [
    "simple">: gql "SimpleMatchStatement",
    "braceBlock">: gql "MatchStatementBlock",
    "parenBlock">: gql "MatchStatementBlock"]

orderByAndOptionalOffsetAndLimit :: TypeDefinition
orderByAndOptionalOffsetAndLimit = def "OrderByAndOptionalOffsetAndLimit" $
  doc "A GQL order by and optional offset and limit" $
  T.record [
    "orderBy">: gql "OrderByClause",
    "offset">: T.optional $ gql "OffsetClause",
    "limit">: T.optional $ gql "LimitClause"]

orderByAndPageStatement :: TypeDefinition
orderByAndPageStatement = def "OrderByAndPageStatement" $
  doc "A GQL order by and page statement: one of several alternative forms" $
  T.union [
    "orderByAndOptionalOffsetAndLimit">: gql "OrderByAndOptionalOffsetAndLimit",
    "offsetAndOptionalLimit">: gql "OffsetAndOptionalLimit",
    "limitOnly">: gql "LimitClause"]

orderByClause :: TypeDefinition
orderByClause = def "OrderByClause" $
  doc "A GQL order by clause, equivalent to a sort specification list" $
  gql "SortSpecificationList"

orderingSpecification :: TypeDefinition
orderingSpecification = def "OrderingSpecification" $
  doc "A GQL ordering specification: one of a fixed set of alternatives" $
  T.enum ["ascending", "descending"]

ordinalityOrOffsetType :: TypeDefinition
ordinalityOrOffsetType = def "OrdinalityOrOffsetType" $
  doc "A GQL ordinality or offset type: one of a fixed set of alternatives" $
  T.enum ["ordinality", "offset"]

parameterName :: TypeDefinition
parameterName = def "ParameterName" $
  doc "A GQL parameter name" $
  T.string

parameterSessionSpecification :: TypeDefinition
parameterSessionSpecification = def "ParameterSessionSpecification" $
  doc "A GQL parameter session specification" $
  T.record [
    "parameter">: T.boolean,
    "sessionParameterSpecification">: gql "SessionParameterSpecification"]

parametersOrCharacteristics :: TypeDefinition
parametersOrCharacteristics = def "ParametersOrCharacteristics" $
  doc "A GQL parameters or characteristics: one of a fixed set of alternatives" $
  T.enum ["parameters", "characteristics"]

parentAndGraphName :: TypeDefinition
parentAndGraphName = def "ParentAndGraphName" $
  doc "A GQL parent and graph name" $
  T.record [
    "parentReference">: gql "CatalogObjectParentReference",
    "graphName">: gql "GraphName"]

parentAndTableName :: TypeDefinition
parentAndTableName = def "ParentAndTableName" $
  doc "A GQL parent and table name" $
  T.record [
    "parentReference">: gql "CatalogObjectParentReference",
    "tableName">: gql "BindingTableName"]

parenthesizedPathPatternExpression :: TypeDefinition
parenthesizedPathPatternExpression = def "ParenthesizedPathPatternExpression" $
  doc "A GQL parenthesized path pattern expression" $
  T.record [
    "subpathDeclaration">: T.optional $ gql "SubpathVariableDeclaration",
    "pathMode">: T.optional $ gql "PathModePrefix",
    "expression">: gql "PathPatternExpression",
    "whereClause">: T.optional $ gql "ParenthesizedPathPatternWhereClause"]

parenthesizedPathPatternWhereClause :: TypeDefinition
parenthesizedPathPatternWhereClause = def "ParenthesizedPathPatternWhereClause" $
  doc "A GQL parenthesized path pattern where clause, equivalent to a search condition" $
  gql "SearchCondition"

parenthesizedValueExpression :: TypeDefinition
parenthesizedValueExpression = def "ParenthesizedValueExpression" $
  doc "A GQL parenthesized value expression, equivalent to a value expression" $
  gql "ValueExpression"

pathElementList :: TypeDefinition
pathElementList = def "PathElementList" $
  doc "A GQL path element list" $
  T.record [
    "start">: gql "PathElementListStart",
    "steps">: T.list $ gql "PathElementListStep"]

pathElementListStart :: TypeDefinition
pathElementListStart = def "PathElementListStart" $
  doc "A GQL path element list start, equivalent to a node reference value expression" $
  gql "NodeReferenceValueExpression"

pathElementListStep :: TypeDefinition
pathElementListStep = def "PathElementListStep" $
  doc "A GQL path element list step" $
  T.record [
    "edgeReference">: gql "EdgeReferenceValueExpression",
    "nodeReference">: gql "NodeReferenceValueExpression"]

pathFactor :: TypeDefinition
pathFactor = def "PathFactor" $
  doc "A GQL path factor: one of several alternative forms" $
  T.union [
    "primary">: gql "PathPrimary",
    "quantifiedPrimary">: gql "QuantifiedPathPrimary",
    "questionedPrimary">: gql "QuestionedPathPrimary"]

pathLengthExpression :: TypeDefinition
pathLengthExpression = def "PathLengthExpression" $
  doc "A GQL path length expression, equivalent to a path value expression" $
  gql "PathValueExpression"

pathMode :: TypeDefinition
pathMode = def "PathMode" $
  doc "A GQL path mode: one of a fixed set of alternatives" $
  T.enum ["walk", "trail", "simple", "acyclic"]

pathModePrefix :: TypeDefinition
pathModePrefix = def "PathModePrefix" $
  doc "A GQL path mode prefix" $
  T.record [
    "mode">: gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

pathOrPaths :: TypeDefinition
pathOrPaths = def "PathOrPaths" $
  doc "A GQL path or paths: one of a fixed set of alternatives" $
  T.enum ["path", "paths"]

pathPattern :: TypeDefinition
pathPattern = def "PathPattern" $
  doc "A GQL path pattern" $
  T.record [
    "variableDeclaration">: T.optional $ gql "PathVariableDeclaration",
    "prefix">: T.optional $ gql "PathPatternPrefix",
    "expression">: gql "PathPatternExpression"]

pathPatternExpression :: TypeDefinition
pathPatternExpression = def "PathPatternExpression" $
  doc "A GQL path pattern expression: one of several alternative forms" $
  T.union [
    "term">: gql "PathTerm",
    "multisetAlternation">: nonemptyList $ gql "PathTerm",
    "patternUnion">: nonemptyList $ gql "PathTerm"]

pathPatternList :: TypeDefinition
pathPatternList = def "PathPatternList" $
  doc "A list of GQL path pattern list elements" $
  nonemptyList $ gql "PathPattern"

pathPatternPrefix :: TypeDefinition
pathPatternPrefix = def "PathPatternPrefix" $
  doc "A GQL path pattern prefix: one of several alternative forms" $
  T.union [
    "modePrefix">: gql "PathModePrefix",
    "searchPrefix">: gql "PathSearchPrefix"]

pathPrimary :: TypeDefinition
pathPrimary = def "PathPrimary" $
  doc "A GQL path primary: one of several alternative forms" $
  T.union [
    "elementPattern">: gql "ElementPattern",
    "parenthesizedExpression">: gql "ParenthesizedPathPatternExpression",
    "simplifiedExpression">: gql "SimplifiedPathPatternExpression"]

pathSearchPrefix :: TypeDefinition
pathSearchPrefix = def "PathSearchPrefix" $
  doc "A GQL path search prefix: one of several alternative forms" $
  T.union [
    "all">: gql "AllPathSearch",
    "any">: gql "AnyPathSearch",
    "shortest">: gql "ShortestPathSearch"]

pathTerm :: TypeDefinition
pathTerm = def "PathTerm" $
  doc "A list of GQL path term elements" $
  nonemptyList $ gql "PathFactor"

pathValueConstructor :: TypeDefinition
pathValueConstructor = def "PathValueConstructor" $
  doc "A GQL path value constructor, equivalent to a path value constructor by enumeration" $
  gql "PathValueConstructorByEnumeration"

pathValueConstructorByEnumeration :: TypeDefinition
pathValueConstructorByEnumeration = def "PathValueConstructorByEnumeration" $
  doc "A GQL path value constructor by enumeration, equivalent to a path element list" $
  gql "PathElementList"

pathValueExpression :: TypeDefinition
pathValueExpression = def "PathValueExpression" $
  doc "A GQL path value expression, equivalent to a value expression" $
  gql "ValueExpression"

pathValueType :: TypeDefinition
pathValueType = def "PathValueType" $
  doc "A GQL path value type" $
  T.record [
    "notNull">: T.boolean]

pathVariable :: TypeDefinition
pathVariable = def "PathVariable" $
  doc "A GQL path variable, equivalent to a binding variable" $
  gql "BindingVariable"

pathVariableDeclaration :: TypeDefinition
pathVariableDeclaration = def "PathVariableDeclaration" $
  doc "A GQL path variable declaration, equivalent to a path variable" $
  gql "PathVariable"

pathVariableReference :: TypeDefinition
pathVariableReference = def "PathVariableReference" $
  doc "A GQL path variable reference, equivalent to a binding variable reference" $
  gql "BindingVariableReference"

powerFunction :: TypeDefinition
powerFunction = def "PowerFunction" $
  doc "A GQL power function" $
  T.record [
    "base">: gql "NumericValueExpressionBase",
    "exponent">: gql "NumericValueExpressionExponent"]

precision :: TypeDefinition
precision = def "Precision" $
  doc "A GQL precision, equivalent to a unsigned decimal integer" $
  gql "UnsignedDecimalInteger"

precisionAndScale :: TypeDefinition
precisionAndScale = def "PrecisionAndScale" $
  doc "A GQL precision and scale" $
  T.record [
    "precision">: gql "Precision",
    "scale">: T.optional $ gql "Scale",
    "notNull">: T.boolean]

predefinedSchemaReference :: TypeDefinition
predefinedSchemaReference = def "PredefinedSchemaReference" $
  doc "A GQL predefined schema reference: one of a fixed set of alternatives" $
  T.enum ["homeSchema", "currentSchema", "period"]

predefinedType :: TypeDefinition
predefinedType = def "PredefinedType" $
  doc "A GQL predefined type: one of several alternative forms" $
  T.union [
    "booleanType">: gql "BooleanType",
    "characterStringType">: gql "CharacterStringType",
    "byteStringType">: gql "ByteStringType",
    "numericType">: gql "NumericType",
    "temporalType">: gql "TemporalType",
    "referenceValueType">: gql "ReferenceValueType",
    "immaterialValueType">: gql "ImmaterialValueType"]

predicate :: TypeDefinition
predicate = def "Predicate" $
  doc "A GQL predicate: one of several alternative forms" $
  T.union [
    "existsPredicate">: gql "ExistsPredicate",
    "nullPredicate">: gql "NullPredicate",
    "valueTypePredicate">: gql "ValueTypePredicate",
    "directedPredicate">: gql "DirectedPredicate",
    "labeledPredicate">: gql "LabeledPredicate",
    "sourceDestinationPredicate">: gql "SourceDestinationPredicate",
    "allDifferentPredicate">: gql "AllDifferentPredicate",
    "samePredicate">: gql "SamePredicate",
    "propertyExistsPredicate">: gql "PropertyExistsPredicate"]

primaryValueExpression :: TypeDefinition
primaryValueExpression = def "PrimaryValueExpression" $
  doc "A GQL primary value expression: one of several alternative forms" $
  T.union [
    "parenthesized">: gql "ParenthesizedValueExpression",
    "aggregateFunction">: gql "AggregateFunction",
    "unsignedValueSpecification">: gql "UnsignedValueSpecification",
    "pathValueConstructor">: gql "PathValueConstructor",
    "propertyReference">: gql "PropertyReference",
    "valueQueryExpression">: gql "ValueQueryExpression",
    "caseExpression">: gql "CaseExpression",
    "castSpecification">: gql "CastSpecification",
    "elementIdFunction">: gql "ElementIdFunction",
    "letValueExpression">: gql "LetValueExpression",
    "bindingVariableReference">: gql "BindingVariableReference"]

primitiveCatalogModifyingStatement :: TypeDefinition
primitiveCatalogModifyingStatement = def "PrimitiveCatalogModifyingStatement" $
  doc "A GQL primitive catalog modifying statement: one of several alternative forms" $
  T.union [
    "createSchema">: gql "CreateSchemaStatement",
    "dropSchema">: gql "DropSchemaStatement",
    "createGraph">: gql "CreateGraphStatement",
    "dropGraph">: gql "DropGraphStatement",
    "createGraphType">: gql "CreateGraphTypeStatement",
    "dropGraphType">: gql "DropGraphTypeStatement"]

primitiveDataModifyingStatement :: TypeDefinition
primitiveDataModifyingStatement = def "PrimitiveDataModifyingStatement" $
  doc "A GQL primitive data modifying statement: one of several alternative forms" $
  T.union [
    "insert">: gql "InsertStatement",
    "set">: gql "SetStatement",
    "remove">: gql "RemoveStatement",
    "delete">: gql "DeleteStatement"]

primitiveQueryStatement :: TypeDefinition
primitiveQueryStatement = def "PrimitiveQueryStatement" $
  doc "A GQL primitive query statement: one of several alternative forms" $
  T.union [
    "match">: gql "MatchStatement",
    "let">: gql "LetStatement",
    "for">: gql "ForStatement",
    "filter">: gql "FilterStatement",
    "orderByAndPage">: gql "OrderByAndPageStatement"]

primitiveResultStatement :: TypeDefinition
primitiveResultStatement = def "PrimitiveResultStatement" $
  doc "A GQL primitive result statement: one of several alternative forms" $
  T.union [
    "returnAndOptionalOrderBy">: gql "ReturnAndOptionalOrderByAndPage",
    "finish">: T.unit]

procedureAndMaybeEnd :: TypeDefinition
procedureAndMaybeEnd = def "ProcedureAndMaybeEnd" $
  doc "A GQL procedure and maybe end" $
  T.record [
    "procedure">: gql "ProcedureSpecification",
    "end">: T.optional $ gql "EndTransactionCommand"]

procedureArgument :: TypeDefinition
procedureArgument = def "ProcedureArgument" $
  doc "A GQL procedure argument, equivalent to a value expression" $
  gql "ValueExpression"

procedureArgumentList :: TypeDefinition
procedureArgumentList = def "ProcedureArgumentList" $
  doc "A list of GQL procedure argument list elements" $
  nonemptyList $ gql "ProcedureArgument"

procedureBody :: TypeDefinition
procedureBody = def "ProcedureBody" $
  doc "A GQL procedure body" $
  T.record [
    "atSchema">: T.optional $ gql "AtSchemaClause",
    "bindings">: T.optional $ gql "BindingVariableDefinitionBlock",
    "statements">: gql "StatementBlock"]

procedureCall :: TypeDefinition
procedureCall = def "ProcedureCall" $
  doc "A GQL procedure call: one of several alternative forms" $
  T.union [
    "inline">: gql "InlineProcedureCall",
    "named">: gql "NamedProcedureCall"]

procedureName :: TypeDefinition
procedureName = def "ProcedureName" $
  doc "A GQL procedure name" $
  T.string

procedureReference :: TypeDefinition
procedureReference = def "ProcedureReference" $
  doc "A GQL procedure reference: one of several alternative forms" $
  T.union [
    "parentAndProcedureName">: gql "CatalogProcedureParentAndName",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

procedureSpecification :: TypeDefinition
procedureSpecification = def "ProcedureSpecification" $
  doc "A GQL procedure specification, equivalent to a procedure body" $
  gql "ProcedureBody"

programActivity :: TypeDefinition
programActivity = def "ProgramActivity" $
  doc "A GQL program activity: one of several alternative forms" $
  T.union [
    "session">: gql "SessionActivity",
    "transaction">: gql "TransactionActivity"]

propertyExistsPredicate :: TypeDefinition
propertyExistsPredicate = def "PropertyExistsPredicate" $
  doc "A GQL property exists predicate" $
  T.record [
    "elementVariableReference">: gql "ElementVariableReference",
    "propertyName">: gql "PropertyName"]

propertyKeyValuePair :: TypeDefinition
propertyKeyValuePair = def "PropertyKeyValuePair" $
  doc "A GQL property key value pair" $
  T.record [
    "name">: gql "PropertyName",
    "value">: gql "ValueExpression"]

propertyKeyValuePairList :: TypeDefinition
propertyKeyValuePairList = def "PropertyKeyValuePairList" $
  doc "A list of GQL property key value pair list elements" $
  nonemptyList $ gql "PropertyKeyValuePair"

propertyName :: TypeDefinition
propertyName = def "PropertyName" $
  doc "A GQL property name" $
  T.string

propertyReference :: TypeDefinition
propertyReference = def "PropertyReference" $
  doc "A GQL property reference" $
  T.record [
    "valueExpression">: gql "PrimaryValueExpression",
    "propertyName">: gql "PropertyName"]

propertyType :: TypeDefinition
propertyType = def "PropertyType" $
  doc "A GQL property type" $
  T.record [
    "name">: gql "PropertyName",
    "typed">: T.optional $ gql "Typed",
    "valueType">: gql "PropertyValueType"]

propertyTypeList :: TypeDefinition
propertyTypeList = def "PropertyTypeList" $
  doc "A list of GQL property type list elements" $
  nonemptyList $ gql "PropertyType"

propertyTypesSpecification :: TypeDefinition
propertyTypesSpecification = def "PropertyTypesSpecification" $
  doc "An optional GQL property types specification" $
  T.optional $ gql "PropertyTypeList"

propertyValueType :: TypeDefinition
propertyValueType = def "PropertyValueType" $
  doc "A GQL property value type, equivalent to a value type" $
  gql "ValueType"

quantifiedPathPrimary :: TypeDefinition
quantifiedPathPrimary = def "QuantifiedPathPrimary" $
  doc "A GQL quantified path primary" $
  T.record [
    "primary">: gql "PathPrimary",
    "quantifier">: gql "GraphPatternQuantifier"]

queryConjunction :: TypeDefinition
queryConjunction = def "QueryConjunction" $
  doc "A GQL query conjunction: one of several alternative forms" $
  T.union [
    "setOperator">: gql "SetOperator",
    "otherwise">: T.unit]

questionedPathPrimary :: TypeDefinition
questionedPathPrimary = def "QuestionedPathPrimary" $
  doc "A GQL questioned path primary, equivalent to a path primary" $
  gql "PathPrimary"

realType :: TypeDefinition
realType = def "RealType" $
  doc "A GQL real type" $
  T.record [
    "notNull">: T.boolean]

recordConstructor :: TypeDefinition
recordConstructor = def "RecordConstructor" $
  doc "A GQL record constructor, equivalent to a fields specification" $
  gql "FieldsSpecification"

recordLiteral :: TypeDefinition
recordLiteral = def "RecordLiteral" $
  doc "A GQL record literal, equivalent to a record constructor" $
  gql "RecordConstructor"

recordType :: TypeDefinition
recordType = def "RecordType" $
  doc "A GQL record type: one of several alternative forms" $
  T.union [
    "anyRecord">: gql "AnyRecordType",
    "specifiedRecord">: gql "SpecifiedRecordType"]

referenceParameterSpecification :: TypeDefinition
referenceParameterSpecification = def "ReferenceParameterSpecification" $
  doc "A GQL reference parameter specification" $
  T.unit

referenceValueType :: TypeDefinition
referenceValueType = def "ReferenceValueType" $
  doc "A GQL reference value type: one of several alternative forms" $
  T.union [
    "graph">: gql "GraphReferenceValueType",
    "bindingTable">: gql "BindingTableReferenceValueType",
    "node">: gql "NodeReferenceValueType",
    "edge">: gql "EdgeReferenceValueType"]

regularIdentifier :: TypeDefinition
regularIdentifier = def "RegularIdentifier" $
  doc "A GQL regular identifier" $
  T.string

relativeCatalogSchemaReference :: TypeDefinition
relativeCatalogSchemaReference = def "RelativeCatalogSchemaReference" $
  doc "A GQL relative catalog schema reference: one of several alternative forms" $
  T.union [
    "predefinedReference">: gql "PredefinedSchemaReference",
    "directoryAndSchema">: gql "RelativeDirectoryAndSchema"]

relativeDirectoryAndSchema :: TypeDefinition
relativeDirectoryAndSchema = def "RelativeDirectoryAndSchema" $
  doc "A GQL relative directory and schema" $
  T.record [
    "directoryPath">: gql "RelativeDirectoryPath",
    "schemaName">: gql "SchemaName"]

relativeDirectoryPath :: TypeDefinition
relativeDirectoryPath = def "RelativeDirectoryPath" $
  doc "A GQL relative directory path" $
  T.record [
    "parentDirectories">: T.nonNegativeInt32,
    "simplePath">: T.optional $ gql "SimpleDirectoryPath"]

removeItem :: TypeDefinition
removeItem = def "RemoveItem" $
  doc "A GQL remove item: one of several alternative forms" $
  T.union [
    "property">: gql "RemovePropertyItem",
    "label">: gql "RemoveLabelItem"]

removeItemList :: TypeDefinition
removeItemList = def "RemoveItemList" $
  doc "A list of GQL remove item list elements" $
  nonemptyList $ gql "RemoveItem"

removeLabelItem :: TypeDefinition
removeLabelItem = def "RemoveLabelItem" $
  doc "A GQL remove label item" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "isOrColon">: gql "IsOrColon",
    "label">: gql "LabelName"]

removePropertyItem :: TypeDefinition
removePropertyItem = def "RemovePropertyItem" $
  doc "A GQL remove property item" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "propertyName">: gql "PropertyName"]

removeStatement :: TypeDefinition
removeStatement = def "RemoveStatement" $
  doc "A GQL remove statement, equivalent to a remove item list" $
  gql "RemoveItemList"

repeatableElementsMatchMode :: TypeDefinition
repeatableElementsMatchMode = def "RepeatableElementsMatchMode" $
  doc "A GQL repeatable elements match mode, equivalent to a element bindings or elements" $
  gql "ElementBindingsOrElements"

result :: TypeDefinition
result = def "Result" $
  doc "A GQL result: one of several alternative forms" $
  T.union [
    "simple">: gql "ResultExpression",
    "nullLiteral">: T.unit]

resultExpression :: TypeDefinition
resultExpression = def "ResultExpression" $
  doc "A GQL result expression, equivalent to a value expression" $
  gql "ValueExpression"

returnAndOptionalOrderByAndPage :: TypeDefinition
returnAndOptionalOrderByAndPage = def "ReturnAndOptionalOrderByAndPage" $
  doc "A GQL return and optional order by and page" $
  T.record [
    "return">: gql "ReturnStatement",
    "orderByAndPage">: T.optional $ gql "OrderByAndPageStatement"]

returnItem :: TypeDefinition
returnItem = def "ReturnItem" $
  doc "A GQL return item" $
  T.record [
    "expression">: gql "AggregatingValueExpression",
    "alias">: T.optional $ gql "ReturnItemAlias"]

returnItemAlias :: TypeDefinition
returnItemAlias = def "ReturnItemAlias" $
  doc "A GQL return item alias" $
  T.string

returnItemList :: TypeDefinition
returnItemList = def "ReturnItemList" $
  doc "A list of GQL return item list elements" $
  nonemptyList $ gql "ReturnItem"

returnItems :: TypeDefinition
returnItems = def "ReturnItems" $
  doc "A GQL return items: one of several alternative forms" $
  T.union [
    "asterisk">: T.unit,
    "itemList">: gql "ReturnItemList"]

returnItemsAndGroupBy :: TypeDefinition
returnItemsAndGroupBy = def "ReturnItemsAndGroupBy" $
  doc "A GQL return items and group by" $
  T.record [
    "quantifier">: T.optional $ gql "SetQuantifier",
    "items">: gql "ReturnItems",
    "groupBy">: T.optional $ gql "GroupByClause"]

returnStatement :: TypeDefinition
returnStatement = def "ReturnStatement" $
  doc "A GQL return statement, equivalent to a return statement body" $
  gql "ReturnStatementBody"

returnStatementBody :: TypeDefinition
returnStatementBody = def "ReturnStatementBody" $
  doc "A GQL return statement body: one of several alternative forms" $
  T.union [
    "items">: gql "ReturnItemsAndGroupBy",
    "noBindings">: T.unit]

rollbackCommand :: TypeDefinition
rollbackCommand = def "RollbackCommand" $
  doc "A GQL rollback command" $
  T.unit

samePredicate :: TypeDefinition
samePredicate = def "SamePredicate" $
  doc "A GQL same predicate" $
  T.record [
    "references">: nonemptyList $ gql "ElementVariableReference"]

scale :: TypeDefinition
scale = def "Scale" $
  doc "A GQL scale, equivalent to a unsigned decimal integer" $
  gql "UnsignedDecimalInteger"

schemaAndObjects :: TypeDefinition
schemaAndObjects = def "SchemaAndObjects" $
  doc "A GQL schema and objects" $
  T.record [
    "schemaReference">: gql "SchemaReference",
    "objects">: T.list $ gql "ObjectName"]

schemaName :: TypeDefinition
schemaName = def "SchemaName" $
  doc "A GQL schema name" $
  T.string

schemaReference :: TypeDefinition
schemaReference = def "SchemaReference" $
  doc "A GQL schema reference: one of several alternative forms" $
  T.union [
    "absoluteReference">: gql "AbsoluteCatalogSchemaReference",
    "relativeReference">: gql "RelativeCatalogSchemaReference",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

searchCondition :: TypeDefinition
searchCondition = def "SearchCondition" $
  doc "A GQL search condition, equivalent to a boolean value expression" $
  gql "BooleanValueExpression"

searchedCase :: TypeDefinition
searchedCase = def "SearchedCase" $
  doc "A GQL searched case" $
  T.record [
    "whenClauses">: nonemptyList $ gql "SearchedWhenClause",
    "elseClause">: T.optional $ gql "ElseClause"]

searchedWhenClause :: TypeDefinition
searchedWhenClause = def "SearchedWhenClause" $
  doc "A GQL searched when clause" $
  T.record [
    "searchCondition">: gql "SearchCondition",
    "result">: gql "Result"]

selectGraphMatch :: TypeDefinition
selectGraphMatch = def "SelectGraphMatch" $
  doc "A GQL select graph match" $
  T.record [
    "graphExpression">: gql "GraphExpression",
    "matchStatement">: gql "MatchStatement"]

selectGraphMatchList :: TypeDefinition
selectGraphMatchList = def "SelectGraphMatchList" $
  doc "A list of GQL select graph match list elements" $
  nonemptyList $ gql "SelectGraphMatch"

selectItem :: TypeDefinition
selectItem = def "SelectItem" $
  doc "A GQL select item" $
  T.record [
    "expression">: gql "AggregatingValueExpression",
    "alias">: T.optional $ gql "SelectItemAlias"]

selectItemAlias :: TypeDefinition
selectItemAlias = def "SelectItemAlias" $
  doc "A GQL select item alias" $
  T.string

selectItemList :: TypeDefinition
selectItemList = def "SelectItemList" $
  doc "A list of GQL select item list elements" $
  nonemptyList $ gql "SelectItem"

selectItems :: TypeDefinition
selectItems = def "SelectItems" $
  doc "A GQL select items: one of several alternative forms" $
  T.union [
    "asterisk">: T.unit,
    "itemList">: gql "SelectItemList"]

selectQuerySpecification :: TypeDefinition
selectQuerySpecification = def "SelectQuerySpecification" $
  doc "A GQL select query specification: one of several alternative forms" $
  T.union [
    "nested">: gql "NestedQuerySpecification",
    "graphAndNested">: gql "GraphAndNestedQuerySpecification"]

selectStatement :: TypeDefinition
selectStatement = def "SelectStatement" $
  doc "A GQL select statement" $
  T.record [
    "quantifier">: T.optional $ gql "SetQuantifier",
    "items">: gql "SelectItems",
    "body">: T.optional $ gql "SelectStatementBodyAndClauses"]

selectStatementBody :: TypeDefinition
selectStatementBody = def "SelectStatementBody" $
  doc "A GQL select statement body: one of several alternative forms" $
  T.union [
    "graphMatchList">: gql "SelectGraphMatchList",
    "querySpecification">: gql "SelectQuerySpecification"]

selectStatementBodyAndClauses :: TypeDefinition
selectStatementBodyAndClauses = def "SelectStatementBodyAndClauses" $
  doc "A GQL select statement body and clauses" $
  T.record [
    "body">: gql "SelectStatementBody",
    "where">: T.optional $ gql "WhereClause",
    "groupBy">: T.optional $ gql "GroupByClause",
    "having">: T.optional $ gql "HavingClause",
    "orderBy">: T.optional $ gql "OrderByClause",
    "offset">: T.optional $ gql "OffsetClause",
    "limit">: T.optional $ gql "LimitClause"]

sessionActivity :: TypeDefinition
sessionActivity = def "SessionActivity" $
  doc "A GQL session activity: one of several alternative forms" $
  T.union [
    "reset">: nonemptyList $ gql "SessionResetCommand",
    "setAndResetCommands">: gql "SessionSetAndResetCommands"]

sessionCloseCommand :: TypeDefinition
sessionCloseCommand = def "SessionCloseCommand" $
  doc "A GQL session close command" $
  T.unit

sessionParameterSpecification :: TypeDefinition
sessionParameterSpecification = def "SessionParameterSpecification" $
  doc "A GQL session parameter specification, equivalent to a parameter name" $
  gql "ParameterName"

sessionResetArguments :: TypeDefinition
sessionResetArguments = def "SessionResetArguments" $
  doc "A GQL session reset arguments: one of several alternative forms" $
  T.union [
    "parametersOrCharacteristics">: gql "AllParametersOrCharacteristics",
    "schema">: T.unit,
    "graph">: T.unit,
    "timeZone">: T.unit,
    "parameterSessionSpecification">: gql "ParameterSessionSpecification"]

sessionResetCommand :: TypeDefinition
sessionResetCommand = def "SessionResetCommand" $
  doc "An optional GQL session reset command" $
  T.optional $ gql "SessionResetArguments"

sessionSetAndResetCommands :: TypeDefinition
sessionSetAndResetCommands = def "SessionSetAndResetCommands" $
  doc "A GQL session set and reset commands" $
  T.record [
    "set">: nonemptyList $ gql "SessionSetCommand",
    "reset">: T.list $ gql "SessionResetCommand"]

sessionSetBindingTableParameterClause :: TypeDefinition
sessionSetBindingTableParameterClause = def "SessionSetBindingTableParameterClause" $
  doc "A GQL session set binding table parameter clause" $
  T.record [
    "binding">: T.boolean,
    "param">: gql "SessionSetParameterName",
    "init">: gql "OptTypedBindingTableInitializer"]

sessionSetCommand :: TypeDefinition
sessionSetCommand = def "SessionSetCommand" $
  doc "A GQL session set command: one of several alternative forms" $
  T.union [
    "schema">: gql "SessionSetSchemaClause",
    "graph">: gql "SessionSetGraphClause",
    "timeZone">: gql "SessionSetTimeZoneClause",
    "parameter">: gql "SessionSetParameterClause"]

sessionSetGraphClause :: TypeDefinition
sessionSetGraphClause = def "SessionSetGraphClause" $
  doc "A GQL session set graph clause, equivalent to a graph expression" $
  gql "GraphExpression"

sessionSetGraphParameterClause :: TypeDefinition
sessionSetGraphParameterClause = def "SessionSetGraphParameterClause" $
  doc "A GQL session set graph parameter clause" $
  T.record [
    "graph">: gql "SessionSetParameterName",
    "initializer">: gql "OptTypedGraphInitializer"]

sessionSetParameterClause :: TypeDefinition
sessionSetParameterClause = def "SessionSetParameterClause" $
  doc "A GQL session set parameter clause: one of several alternative forms" $
  T.union [
    "graph">: gql "SessionSetGraphParameterClause",
    "bindings">: gql "SessionSetBindingTableParameterClause",
    "value">: gql "SessionSetValueParameterClause"]

sessionSetParameterName :: TypeDefinition
sessionSetParameterName = def "SessionSetParameterName" $
  doc "A GQL session set parameter name" $
  T.record [
    "ifNotExists">: T.boolean,
    "parameter">: gql "SessionParameterSpecification"]

sessionSetSchemaClause :: TypeDefinition
sessionSetSchemaClause = def "SessionSetSchemaClause" $
  doc "A GQL session set schema clause, equivalent to a schema reference" $
  gql "SchemaReference"

sessionSetTimeZoneClause :: TypeDefinition
sessionSetTimeZoneClause = def "SessionSetTimeZoneClause" $
  doc "A GQL session set time zone clause, equivalent to a set time zone value" $
  gql "SetTimeZoneValue"

sessionSetValueParameterClause :: TypeDefinition
sessionSetValueParameterClause = def "SessionSetValueParameterClause" $
  doc "A GQL session set value parameter clause" $
  T.record [
    "value">: gql "SessionSetParameterName",
    "initializer">: gql "OptTypedValueInitializer"]

setAllPropertiesItem :: TypeDefinition
setAllPropertiesItem = def "SetAllPropertiesItem" $
  doc "A GQL set all properties item" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "properties">: T.optional $ gql "PropertyKeyValuePairList"]

setItem :: TypeDefinition
setItem = def "SetItem" $
  doc "A GQL set item: one of several alternative forms" $
  T.union [
    "property">: gql "SetPropertyItem",
    "allProperties">: gql "SetAllPropertiesItem",
    "label">: gql "SetLabelItem"]

setItemList :: TypeDefinition
setItemList = def "SetItemList" $
  doc "A list of GQL set item list elements" $
  nonemptyList $ gql "SetItem"

setLabelItem :: TypeDefinition
setLabelItem = def "SetLabelItem" $
  doc "A GQL set label item" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "isOrColon">: gql "IsOrColon",
    "label">: gql "LabelName"]

setOperator :: TypeDefinition
setOperator = def "SetOperator" $
  doc "A GQL set operator" $
  T.record [
    "operatorType">: gql "SetOperatorType",
    "quantifier">: T.optional $ gql "SetQuantifier"]

setOperatorType :: TypeDefinition
setOperatorType = def "SetOperatorType" $
  doc "A GQL set operator type: one of a fixed set of alternatives" $
  T.enum ["union", "except", "intersect"]

setPropertyItem :: TypeDefinition
setPropertyItem = def "SetPropertyItem" $
  doc "A GQL set property item" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "propertyName">: gql "PropertyName",
    "value">: gql "ValueExpression"]

setQuantifier :: TypeDefinition
setQuantifier = def "SetQuantifier" $
  doc "A GQL set quantifier: one of a fixed set of alternatives" $
  T.enum ["distinct", "all"]

setStatement :: TypeDefinition
setStatement = def "SetStatement" $
  doc "A GQL set statement, equivalent to a set item list" $
  gql "SetItemList"

setTimeZoneValue :: TypeDefinition
setTimeZoneValue = def "SetTimeZoneValue" $
  doc "A GQL set time zone value, equivalent to a time zone string" $
  gql "TimeZoneString"

shortestPathSearch :: TypeDefinition
shortestPathSearch = def "ShortestPathSearch" $
  doc "A GQL shortest path search: one of several alternative forms" $
  T.union [
    "allShortest">: gql "AllShortestPathSearch",
    "anyShortest">: gql "AnyShortestPathSearch",
    "countedShortest">: gql "CountedShortestPathSearch",
    "countedShortestGroup">: gql "CountedShortestGroupSearch"]

side :: TypeDefinition
side = def "Side" $
  doc "A GQL side: one of a fixed set of alternatives" $
  T.enum ["left", "right"]

sign :: TypeDefinition
sign = def "Sign" $
  doc "A GQL sign: one of a fixed set of alternatives" $
  T.enum ["plus", "minus"]

signedBinaryExactNumericType :: TypeDefinition
signedBinaryExactNumericType = def "SignedBinaryExactNumericType" $
  doc "A GQL signed binary exact numeric type: one of several alternative forms" $
  T.union [
    "int8">: gql "Int8Type",
    "int16">: gql "Int16Type",
    "int32">: gql "Int32Type",
    "int64">: gql "Int64Type",
    "int128">: gql "Int128Type",
    "int256">: gql "Int256Type",
    "smallInt">: gql "SmallIntType",
    "intWithPrecision">: gql "IntWithPrecision",
    "bigInt">: gql "BigIntType",
    "signedVerboseType">: gql "SignedVerboseBinaryExactNumericType"]

signedExpr :: TypeDefinition
signedExpr = def "SignedExpr" $
  doc "A GQL signed expression" $
  T.record [
    "sign">: gql "Sign",
    "valueExpression">: gql "ValueExpression"]

signedNumericValueExpression :: TypeDefinition
signedNumericValueExpression = def "SignedNumericValueExpression" $
  doc "A GQL signed numeric value expression" $
  T.record [
    "sign">: gql "Sign",
    "expression">: gql "NumericValueExpression"]

signedVerboseBinaryExactNumericType :: TypeDefinition
signedVerboseBinaryExactNumericType = def "SignedVerboseBinaryExactNumericType" $
  doc "A GQL signed verbose binary exact numeric type" $
  T.record [
    "signed">: T.boolean,
    "verboseType">: gql "VerboseBinaryExactNumericType"]

simpleCase :: TypeDefinition
simpleCase = def "SimpleCase" $
  doc "A GQL simple case" $
  T.record [
    "caseOperand">: gql "CaseOperand",
    "whenClauses">: nonemptyList $ gql "SimpleWhenClause",
    "elseClause">: T.optional $ gql "ElseClause"]

simpleCatalogModifyingStatement :: TypeDefinition
simpleCatalogModifyingStatement = def "SimpleCatalogModifyingStatement" $
  doc "A GQL simple catalog modifying statement: one of several alternative forms" $
  T.union [
    "primitive">: gql "PrimitiveCatalogModifyingStatement",
    "callProcedure">: gql "CallCatalogModifyingProcedureStatement"]

simpleDataAccessingStatement :: TypeDefinition
simpleDataAccessingStatement = def "SimpleDataAccessingStatement" $
  doc "A GQL simple data accessing statement: one of several alternative forms" $
  T.union [
    "query">: gql "SimpleQueryStatement",
    "modifying">: gql "SimpleDataModifyingStatement"]

simpleDataModifyingStatement :: TypeDefinition
simpleDataModifyingStatement = def "SimpleDataModifyingStatement" $
  doc "A GQL simple data modifying statement: one of several alternative forms" $
  T.union [
    "primitive">: gql "PrimitiveDataModifyingStatement",
    "callProcedure">: gql "CallDataModifyingProcedureStatement"]

simpleDirectoryPath :: TypeDefinition
simpleDirectoryPath = def "SimpleDirectoryPath" $
  doc "A list of GQL simple directory path elements" $
  nonemptyList $ gql "DirectoryName"

simpleLinearDataAccessingStatement :: TypeDefinition
simpleLinearDataAccessingStatement = def "SimpleLinearDataAccessingStatement" $
  doc "A list of GQL simple linear data accessing statement elements" $
  nonemptyList $ gql "SimpleDataAccessingStatement"

simpleLinearQueryStatement :: TypeDefinition
simpleLinearQueryStatement = def "SimpleLinearQueryStatement" $
  doc "A list of GQL simple linear query statement elements" $
  nonemptyList $ gql "SimpleQueryStatement"

simpleMatchStatement :: TypeDefinition
simpleMatchStatement = def "SimpleMatchStatement" $
  doc "A GQL simple match statement, equivalent to a graph pattern binding table" $
  gql "GraphPatternBindingTable"

simpleQueryStatement :: TypeDefinition
simpleQueryStatement = def "SimpleQueryStatement" $
  doc "A GQL simple query statement: one of several alternative forms" $
  T.union [
    "primitive">: gql "PrimitiveQueryStatement",
    "call">: gql "CallQueryStatement"]

simpleWhenClause :: TypeDefinition
simpleWhenClause = def "SimpleWhenClause" $
  doc "A GQL simple when clause" $
  T.record [
    "whenOperands">: gql "WhenOperandList",
    "result">: gql "Result"]

simplifiedConcatenation :: TypeDefinition
simplifiedConcatenation = def "SimplifiedConcatenation" $
  doc "A GQL simplified concatenation" $
  T.record [
    "initialTerm">: gql "SimplifiedTerm",
    "nextFactor">: gql "SimplifiedFactorLow"]

simplifiedConjunction :: TypeDefinition
simplifiedConjunction = def "SimplifiedConjunction" $
  doc "A GQL simplified conjunction" $
  T.record [
    "left">: gql "SimplifiedFactorLow",
    "right">: gql "SimplifiedFactorHigh"]

simplifiedContents :: TypeDefinition
simplifiedContents = def "SimplifiedContents" $
  doc "A GQL simplified contents: one of several alternative forms" $
  T.union [
    "term">: gql "SimplifiedTerm",
    "pathUnion">: gql "SimplifiedPathUnion",
    "multisetAlternation">: gql "SimplifiedMultisetAlternation"]

simplifiedDefaultingAnyDirection :: TypeDefinition
simplifiedDefaultingAnyDirection = def "SimplifiedDefaultingAnyDirection" $
  doc "A GQL simplified defaulting any direction, equivalent to a simplified contents" $
  gql "SimplifiedContents"

simplifiedDefaultingLeft :: TypeDefinition
simplifiedDefaultingLeft = def "SimplifiedDefaultingLeft" $
  doc "A GQL simplified defaulting left, equivalent to a simplified contents" $
  gql "SimplifiedContents"

simplifiedDefaultingLeftOrRight :: TypeDefinition
simplifiedDefaultingLeftOrRight = def "SimplifiedDefaultingLeftOrRight" $
  doc "A GQL simplified defaulting left or right, equivalent to a simplified contents" $
  gql "SimplifiedContents"

simplifiedDefaultingLeftOrUndirected :: TypeDefinition
simplifiedDefaultingLeftOrUndirected = def "SimplifiedDefaultingLeftOrUndirected" $
  doc "A GQL simplified defaulting left or undirected, equivalent to a simplified contents" $
  gql "SimplifiedContents"

simplifiedDefaultingRight :: TypeDefinition
simplifiedDefaultingRight = def "SimplifiedDefaultingRight" $
  doc "A GQL simplified defaulting right, equivalent to a simplified contents" $
  gql "SimplifiedContents"

simplifiedDefaultingUndirected :: TypeDefinition
simplifiedDefaultingUndirected = def "SimplifiedDefaultingUndirected" $
  doc "A GQL simplified defaulting undirected, equivalent to a simplified contents" $
  gql "SimplifiedContents"

simplifiedDefaultingUndirectedOrRight :: TypeDefinition
simplifiedDefaultingUndirectedOrRight = def "SimplifiedDefaultingUndirectedOrRight" $
  doc "A GQL simplified defaulting undirected or right, equivalent to a simplified contents" $
  gql "SimplifiedContents"

simplifiedDirectionOverride :: TypeDefinition
simplifiedDirectionOverride = def "SimplifiedDirectionOverride" $
  doc "A GQL simplified direction override: one of several alternative forms" $
  T.union [
    "overrideLeft">: gql "SimplifiedOverrideLeft",
    "overrideUndirected">: gql "SimplifiedOverrideUndirected",
    "overrideRight">: gql "SimplifiedOverrideRight",
    "overrideLeftOrUndirected">: gql "SimplifiedOverrideLeftOrUndirected",
    "overrideUndirectedOrRight">: gql "SimplifiedOverrideUndirectedOrRight",
    "overrideLeftOrRight">: gql "SimplifiedOverrideLeftOrRight",
    "overrideAnyDirection">: gql "SimplifiedOverrideAnyDirection"]

simplifiedFactorHigh :: TypeDefinition
simplifiedFactorHigh = def "SimplifiedFactorHigh" $
  doc "A GQL simplified factor high: one of several alternative forms" $
  T.union [
    "tertiary">: gql "SimplifiedTertiary",
    "quantified">: gql "SimplifiedQuantified",
    "questioned">: gql "SimplifiedQuestioned"]

simplifiedFactorLow :: TypeDefinition
simplifiedFactorLow = def "SimplifiedFactorLow" $
  doc "A GQL simplified factor low: one of several alternative forms" $
  T.union [
    "factorHigh">: gql "SimplifiedFactorHigh",
    "conjunction">: gql "SimplifiedConjunction"]

simplifiedMultisetAlternation :: TypeDefinition
simplifiedMultisetAlternation = def "SimplifiedMultisetAlternation" $
  doc "A list of GQL simplified multiset alternation elements" $
  nonemptyList $ gql "SimplifiedTerm"

simplifiedNegation :: TypeDefinition
simplifiedNegation = def "SimplifiedNegation" $
  doc "A GQL simplified negation, equivalent to a simplified primary" $
  gql "SimplifiedPrimary"

simplifiedOverrideAnyDirection :: TypeDefinition
simplifiedOverrideAnyDirection = def "SimplifiedOverrideAnyDirection" $
  doc "A GQL simplified override any direction, equivalent to a simplified secondary" $
  gql "SimplifiedSecondary"

simplifiedOverrideLeft :: TypeDefinition
simplifiedOverrideLeft = def "SimplifiedOverrideLeft" $
  doc "A GQL simplified override left, equivalent to a simplified secondary" $
  gql "SimplifiedSecondary"

simplifiedOverrideLeftOrRight :: TypeDefinition
simplifiedOverrideLeftOrRight = def "SimplifiedOverrideLeftOrRight" $
  doc "A GQL simplified override left or right, equivalent to a simplified secondary" $
  gql "SimplifiedSecondary"

simplifiedOverrideLeftOrUndirected :: TypeDefinition
simplifiedOverrideLeftOrUndirected = def "SimplifiedOverrideLeftOrUndirected" $
  doc "A GQL simplified override left or undirected, equivalent to a simplified secondary" $
  gql "SimplifiedSecondary"

simplifiedOverrideRight :: TypeDefinition
simplifiedOverrideRight = def "SimplifiedOverrideRight" $
  doc "A GQL simplified override right, equivalent to a simplified secondary" $
  gql "SimplifiedSecondary"

simplifiedOverrideUndirected :: TypeDefinition
simplifiedOverrideUndirected = def "SimplifiedOverrideUndirected" $
  doc "A GQL simplified override undirected, equivalent to a simplified secondary" $
  gql "SimplifiedSecondary"

simplifiedOverrideUndirectedOrRight :: TypeDefinition
simplifiedOverrideUndirectedOrRight = def "SimplifiedOverrideUndirectedOrRight" $
  doc "A GQL simplified override undirected or right, equivalent to a simplified secondary" $
  gql "SimplifiedSecondary"

simplifiedPathPatternExpression :: TypeDefinition
simplifiedPathPatternExpression = def "SimplifiedPathPatternExpression" $
  doc "A GQL simplified path pattern expression: one of several alternative forms" $
  T.union [
    "left">: gql "SimplifiedDefaultingLeft",
    "undirected">: gql "SimplifiedDefaultingUndirected",
    "right">: gql "SimplifiedDefaultingRight",
    "leftOrUndirected">: gql "SimplifiedDefaultingLeftOrUndirected",
    "undirectedOrRight">: gql "SimplifiedDefaultingUndirectedOrRight",
    "leftOrRight">: gql "SimplifiedDefaultingLeftOrRight",
    "anyDirection">: gql "SimplifiedDefaultingAnyDirection"]

simplifiedPathUnion :: TypeDefinition
simplifiedPathUnion = def "SimplifiedPathUnion" $
  doc "A list of GQL simplified path union elements" $
  nonemptyList $ gql "SimplifiedTerm"

simplifiedPrimary :: TypeDefinition
simplifiedPrimary = def "SimplifiedPrimary" $
  doc "A GQL simplified primary: one of several alternative forms" $
  T.union [
    "labelName">: gql "LabelName",
    "parenthesizedContents">: gql "SimplifiedContents"]

simplifiedQuantified :: TypeDefinition
simplifiedQuantified = def "SimplifiedQuantified" $
  doc "A GQL simplified quantified" $
  T.record [
    "tertiary">: gql "SimplifiedTertiary",
    "quantifier">: gql "GraphPatternQuantifier"]

simplifiedQuestioned :: TypeDefinition
simplifiedQuestioned = def "SimplifiedQuestioned" $
  doc "A GQL simplified questioned, equivalent to a simplified tertiary" $
  gql "SimplifiedTertiary"

simplifiedSecondary :: TypeDefinition
simplifiedSecondary = def "SimplifiedSecondary" $
  doc "A GQL simplified secondary: one of several alternative forms" $
  T.union [
    "primary">: gql "SimplifiedPrimary",
    "negation">: gql "SimplifiedNegation"]

simplifiedTerm :: TypeDefinition
simplifiedTerm = def "SimplifiedTerm" $
  doc "A GQL simplified term: one of several alternative forms" $
  T.union [
    "factorLow">: gql "SimplifiedFactorLow",
    "concatenation">: gql "SimplifiedConcatenation"]

simplifiedTertiary :: TypeDefinition
simplifiedTertiary = def "SimplifiedTertiary" $
  doc "A GQL simplified tertiary: one of several alternative forms" $
  T.union [
    "directionOverride">: gql "SimplifiedDirectionOverride",
    "secondary">: gql "SimplifiedSecondary"]

smallIntType :: TypeDefinition
smallIntType = def "SmallIntType" $
  doc "A GQL small int type" $
  T.record ["notNull">: T.boolean]

smallIntegerType :: TypeDefinition
smallIntegerType = def "SmallIntegerType" $
  doc "A GQL small integer type" $
  T.record ["notNull">: T.boolean]

sortKey :: TypeDefinition
sortKey = def "SortKey" $
  doc "A GQL sort key, equivalent to a aggregating value expression" $
  gql "AggregatingValueExpression"

sortSpecification :: TypeDefinition
sortSpecification = def "SortSpecification" $
  doc "A GQL sort specification" $
  T.record [
    "sortKey">: gql "SortKey",
    "ordering">: T.optional $ gql "OrderingSpecification",
    "nullOrdering">: T.optional $ gql "NullOrdering"]

sortSpecificationList :: TypeDefinition
sortSpecificationList = def "SortSpecificationList" $
  doc "A list of GQL sort specification list elements" $
  nonemptyList $ gql "SortSpecification"

sourceDestinationPredicate :: TypeDefinition
sourceDestinationPredicate = def "SourceDestinationPredicate" $
  doc "A GQL source destination predicate: one of several alternative forms" $
  T.union [
    "sourcePredicate">: gql "SourcePredicate",
    "destinationPredicate">: gql "DestinationPredicate"]

sourceNodeTypeAlias :: TypeDefinition
sourceNodeTypeAlias = def "SourceNodeTypeAlias" $
  doc "A GQL source node type alias" $
  T.string

sourceNodeTypeReference :: TypeDefinition
sourceNodeTypeReference = def "SourceNodeTypeReference" $
  doc "A GQL source node type reference: one of several alternative forms" $
  T.union [
    "alias">: gql "SourceNodeTypeAlias",
    "filler">: T.optional $ gql "NodeTypeFiller"]

sourcePredicate :: TypeDefinition
sourcePredicate = def "SourcePredicate" $
  doc "A GQL source predicate" $
  T.record [
    "not">: T.boolean,
    "sourceOf">: gql "EdgeReference"]

specifiedRecordType :: TypeDefinition
specifiedRecordType = def "SpecifiedRecordType" $
  doc "A GQL specified record type" $
  T.record [
    "record">: T.boolean,
    "fieldTypes">: gql "FieldTypesSpecification",
    "notNull">: T.boolean]

squareRoot :: TypeDefinition
squareRoot = def "SquareRoot" $
  doc "A GQL square root, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

startAndMaybeProcedureAndMaybeEnd :: TypeDefinition
startAndMaybeProcedureAndMaybeEnd = def "StartAndMaybeProcedureAndMaybeEnd" $
  doc "A GQL start and maybe procedure and maybe end" $
  T.record [
    "start">: gql "StartTransactionCommand",
    "procedureAndEnd">: T.optional $ gql "ProcedureAndMaybeEnd"]

startTransactionCommand :: TypeDefinition
startTransactionCommand = def "StartTransactionCommand" $
  doc "An optional GQL start transaction command" $
  T.optional $ gql "TransactionCharacteristics"

statement :: TypeDefinition
statement = def "Statement" $
  doc "A GQL statement: one of several alternative forms" $
  T.union [
    "linearCatalogModifying">: gql "LinearCatalogModifyingStatement",
    "linearDataModifying">: gql "LinearDataModifyingStatement",
    "compositeQuery">: gql "CompositeQueryStatement"]

statementBlock :: TypeDefinition
statementBlock = def "StatementBlock" $
  doc "A GQL statement block" $
  T.record [
    "statement">: gql "Statement",
    "nextStatements">: T.list $ gql "NextStatement"]

stringLength :: TypeDefinition
stringLength = def "StringLength" $
  doc "A GQL string length, equivalent to a numeric value expression" $
  gql "NumericValueExpression"

stringType :: TypeDefinition
stringType = def "StringType" $
  doc "A GQL string type" $
  T.record [
    "minLength">: T.optional $ gql "MinLength",
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

subCharacterOrByteString :: TypeDefinition
subCharacterOrByteString = def "SubCharacterOrByteString" $
  doc "A GQL subtraction character or byte string" $
  T.record [
    "side">: gql "Side",
    "valueExpression">: gql "ValueExpression",
    "stringLength">: gql "StringLength"]

subpathVariable :: TypeDefinition
subpathVariable = def "SubpathVariable" $
  doc "A GQL subpath variable" $
  T.string

subpathVariableDeclaration :: TypeDefinition
subpathVariableDeclaration = def "SubpathVariableDeclaration" $
  doc "A GQL subpath variable declaration, equivalent to a subpath variable" $
  gql "SubpathVariable"

temporalDurationQualifier :: TypeDefinition
temporalDurationQualifier = def "TemporalDurationQualifier" $
  doc "A GQL temporal duration qualifier: one of a fixed set of alternatives" $
  T.enum ["yearToMonth", "dayToSecond"]

temporalDurationType :: TypeDefinition
temporalDurationType = def "TemporalDurationType" $
  doc "A GQL temporal duration type" $
  T.record [
    "qualifier">: gql "TemporalDurationQualifier",
    "notNull">: T.boolean]

temporalInstantType :: TypeDefinition
temporalInstantType = def "TemporalInstantType" $
  doc "A GQL temporal instant type: one of several alternative forms" $
  T.union [
    "datetimeType">: gql "DatetimeType",
    "localDatetimeTypeChoice">: gql "LocalDatetimeTypeChoice",
    "dateType">: gql "DateType",
    "timeType">: gql "TimeType",
    "localTimeTypeChoice">: gql "LocalTimeTypeChoice"]

temporalLiteral :: TypeDefinition
temporalLiteral = def "TemporalLiteral" $
  doc "A GQL temporal literal: one of several alternative forms" $
  T.union [
    "date">: gql "DateLiteral",
    "time">: gql "TimeLiteral",
    "datetime">: gql "DatetimeLiteral"]

temporalType :: TypeDefinition
temporalType = def "TemporalType" $
  doc "A GQL temporal type: one of several alternative forms" $
  T.union [
    "instantType">: gql "TemporalInstantType",
    "durationType">: gql "TemporalDurationType"]

timeFunction :: TypeDefinition
timeFunction = def "TimeFunction" $
  doc "A GQL time function: one of several alternative forms" $
  T.union [
    "currentTime">: T.unit,
    "zonedTimeWithParams">: T.optional $ gql "TimeFunctionParameters"]

timeFunctionParameters :: TypeDefinition
timeFunctionParameters = def "TimeFunctionParameters" $
  doc "A GQL time function parameters: one of several alternative forms" $
  T.union [
    "timeString">: gql "TimeString",
    "recordConstructor">: gql "RecordConstructor"]

timeLiteral :: TypeDefinition
timeLiteral = def "TimeLiteral" $
  doc "A GQL time literal, equivalent to a time string" $
  gql "TimeString"

timeString :: TypeDefinition
timeString = def "TimeString" $
  doc "A GQL time string, equivalent to a character string literal" $
  gql "CharacterStringLiteral"

timeType :: TypeDefinition
timeType = def "TimeType" $
  doc "A GQL time type: one of several alternative forms" $
  T.union [
    "zonedTime">: gql "ZonedTimeType",
    "timeWithTimeZone">: gql "TimeWithTimeZoneType"]

timeWithTimeZoneType :: TypeDefinition
timeWithTimeZoneType = def "TimeWithTimeZoneType" $
  doc "A GQL time with time zone type" $
  T.record [
    "notNull">: T.boolean]

timeWithoutTimeZoneType :: TypeDefinition
timeWithoutTimeZoneType = def "TimeWithoutTimeZoneType" $
  doc "A GQL time without time zone type" $
  T.record [
    "notNull">: T.boolean]

timeZoneString :: TypeDefinition
timeZoneString = def "TimeZoneString" $
  doc "A GQL time zone string, equivalent to a character string literal" $
  gql "CharacterStringLiteral"

timestampWithTimeZoneType :: TypeDefinition
timestampWithTimeZoneType = def "TimestampWithTimeZoneType" $
  doc "A GQL timestamp with time zone type" $
  T.record [
    "notNull">: T.boolean]

timestampWithoutTimeZoneType :: TypeDefinition
timestampWithoutTimeZoneType = def "TimestampWithoutTimeZoneType" $
  doc "A GQL timestamp without time zone type" $
  T.record [
    "notNull">: T.boolean]

transactionAccessMode :: TypeDefinition
transactionAccessMode = def "TransactionAccessMode" $
  doc "A GQL transaction access mode: one of a fixed set of alternatives" $
  T.enum [
    "readOnly",
    "readWrite"]

transactionActivity :: TypeDefinition
transactionActivity = def "TransactionActivity" $
  doc "A GQL transaction activity: one of several alternative forms" $
  T.union [
    "start">: gql "StartAndMaybeProcedureAndMaybeEnd",
    "procedure">: gql "ProcedureAndMaybeEnd",
    "end">: gql "EndTransactionCommand"]

transactionCharacteristics :: TypeDefinition
transactionCharacteristics = def "TransactionCharacteristics" $
  doc "A list of GQL transaction characteristics elements" $
  nonemptyList $ gql "TransactionMode"

transactionMode :: TypeDefinition
transactionMode = def "TransactionMode" $
  doc "A GQL transaction mode, equivalent to a transaction access mode" $
  gql "TransactionAccessMode"

trigonometricFunction :: TypeDefinition
trigonometricFunction = def "TrigonometricFunction" $
  doc "A GQL trigonometric function" $
  T.record [
    "name">: gql "TrigonometricFunctionName",
    "value">: gql "NumericValueExpression"]

trigonometricFunctionName :: TypeDefinition
trigonometricFunctionName = def "TrigonometricFunctionName" $
  doc "A GQL trigonometric function name: one of a fixed set of alternatives" $
  T.enum ["sin", "cos", "tan", "cot", "sinh", "cosh", "tanh", "asin", "acos", "atan", "degrees", "radians"]

trimCharacterOrByteString :: TypeDefinition
trimCharacterOrByteString = def "TrimCharacterOrByteString" $
  doc "A GQL trim character or byte string, equivalent to a value expression" $
  gql "ValueExpression"

trimCharacterOrByteStringSource :: TypeDefinition
trimCharacterOrByteStringSource = def "TrimCharacterOrByteStringSource" $
  doc "A GQL trim character or byte string source, equivalent to a value expression" $
  gql "ValueExpression"

trimListFunction :: TypeDefinition
trimListFunction = def "TrimListFunction" $
  doc "A GQL trim list function" $
  T.record [
    "listValue">: gql "ListValueExpression",
    "numericValue">: gql "NumericValueExpression"]

trimMultiCharacterCharacterString :: TypeDefinition
trimMultiCharacterCharacterString = def "TrimMultiCharacterCharacterString" $
  doc "A GQL trim multi character character string" $
  T.record [
    "trimType">: gql "TrimType",
    "valueExpression">: gql "ValueExpression",
    "optionalValueExpression">: T.optional $ gql "ValueExpression"]

trimOperands :: TypeDefinition
trimOperands = def "TrimOperands" $
  doc "A GQL trim operands" $
  T.record [
    "specification">: T.optional $ gql "TrimSpecification",
    "characterOrByteString">: T.optional $ gql "TrimCharacterOrByteString",
    "source">: gql "TrimCharacterOrByteStringSource"]

trimSingleCharacterOrByteString :: TypeDefinition
trimSingleCharacterOrByteString = def "TrimSingleCharacterOrByteString" $
  doc "A GQL trim single character or byte string, equivalent to a trim operands" $
  gql "TrimOperands"

trimSpecification :: TypeDefinition
trimSpecification = def "TrimSpecification" $
  doc "A GQL trim specification: one of a fixed set of alternatives" $
  T.enum ["leading", "trailing", "both"]

trimType :: TypeDefinition
trimType = def "TrimType" $
  doc "A GQL trim type: one of a fixed set of alternatives" $
  T.enum ["btrim", "ltrim", "rtrim"]

truthValue :: TypeDefinition
truthValue = def "TruthValue" $
  doc "A GQL truth value, equivalent to a boolean literal" $
  gql "BooleanLiteral"

typed :: TypeDefinition
typed = def "Typed" $
  doc "A GQL typed" $
  T.unit

typedBindingTableReferenceValueType :: TypeDefinition
typedBindingTableReferenceValueType = def "TypedBindingTableReferenceValueType" $
  doc "A GQL typed binding table reference value type" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "valueType">: gql "BindingTableReferenceValueType"]

typedGraphReferenceValueType :: TypeDefinition
typedGraphReferenceValueType = def "TypedGraphReferenceValueType" $
  doc "A GQL typed graph reference value type" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "valueType">: gql "GraphReferenceValueType"]

typedGraphTypeReference :: TypeDefinition
typedGraphTypeReference = def "TypedGraphTypeReference" $
  doc "A GQL typed graph type reference" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "reference">: gql "GraphTypeReference"]

typedNestedGraphTypeSpecification :: TypeDefinition
typedNestedGraphTypeSpecification = def "TypedNestedGraphTypeSpecification" $
  doc "A GQL typed nested graph type specification" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "graph">: T.boolean,
    "specification">: gql "NestedGraphTypeSpecification"]

typedValueType :: TypeDefinition
typedValueType = def "TypedValueType" $
  doc "A GQL typed value type" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "valueType">: gql "ValueType"]

uBigIntType :: TypeDefinition
uBigIntType = def "UBigIntType" $
  doc "A GQL u big int type" $
  T.record ["notNull">: T.boolean]

uSmallIntType :: TypeDefinition
uSmallIntType = def "USmallIntType" $
  doc "A GQL u small int type" $
  T.record ["notNull">: T.boolean]

uint128Type :: TypeDefinition
uint128Type = def "Uint128Type" $
  doc "A GQL uint128 type" $
  T.record ["notNull">: T.boolean]

uint16Type :: TypeDefinition
uint16Type = def "Uint16Type" $
  doc "A GQL uint16 type" $
  T.record ["notNull">: T.boolean]

uint256Type :: TypeDefinition
uint256Type = def "Uint256Type" $
  doc "A GQL uint256 type" $
  T.record ["notNull">: T.boolean]

uint32Type :: TypeDefinition
uint32Type = def "Uint32Type" $
  doc "A GQL uint32 type" $
  T.record ["notNull">: T.boolean]

uint64Type :: TypeDefinition
uint64Type = def "Uint64Type" $
  doc "A GQL uint64 type" $
  T.record ["notNull">: T.boolean]

uint8Type :: TypeDefinition
uint8Type = def "Uint8Type" $
  doc "A GQL uint8 type" $
  T.record ["notNull">: T.boolean]

uintWithPrecision :: TypeDefinition
uintWithPrecision = def "UintWithPrecision" $
  doc "A GQL uint with precision" $
  T.record [
    "precision">: T.optional $ gql "Precision",
    "notNull">: T.boolean]

unsignedBinaryExactNumericType :: TypeDefinition
unsignedBinaryExactNumericType = def "UnsignedBinaryExactNumericType" $
  doc "A GQL unsigned binary exact numeric type: one of several alternative forms" $
  T.union [
    "uint8">: gql "Uint8Type",
    "uint16">: gql "Uint16Type",
    "uint32">: gql "Uint32Type",
    "uint64">: gql "Uint64Type",
    "uint128">: gql "Uint128Type",
    "uint256">: gql "Uint256Type",
    "uSmallInt">: gql "USmallIntType",
    "uintWithPrecision">: gql "UintWithPrecision",
    "uBigInt">: gql "UBigIntType",
    "unsigned">: gql "VerboseBinaryExactNumericType"]

unsignedDecimalInteger :: TypeDefinition
unsignedDecimalInteger = def "UnsignedDecimalInteger" $
  doc "A GQL unsigned decimal integer" $
  T.string

unsignedInteger :: TypeDefinition
unsignedInteger = def "UnsignedInteger" $
  doc "A GQL unsigned integer: one of several alternative forms" $
  T.union [
    "decimal">: T.string,
    "hexadecimal">: T.string,
    "octal">: T.string,
    "binary">: T.string]

unsignedLiteral :: TypeDefinition
unsignedLiteral = def "UnsignedLiteral" $
  doc "A GQL unsigned literal: one of several alternative forms" $
  T.union [
    "numeric">: gql "UnsignedNumericLiteral",
    "general">: gql "GeneralLiteral"]

unsignedNumericLiteral :: TypeDefinition
unsignedNumericLiteral = def "UnsignedNumericLiteral" $
  doc "A GQL unsigned numeric literal: one of several alternative forms" $
  T.union [
    "exact">: gql "ExactNumericLiteral",
    "approximate">: gql "ApproximateNumericLiteral"]

unsignedValueSpecification :: TypeDefinition
unsignedValueSpecification = def "UnsignedValueSpecification" $
  doc "A GQL unsigned value specification: one of several alternative forms" $
  T.union [
    "unsignedLiteral">: gql "UnsignedLiteral",
    "generalValueSpecification">: gql "GeneralValueSpecification"]

upperBound :: TypeDefinition
upperBound = def "UpperBound" $
  doc "A GQL upper bound, equivalent to a unsigned integer" $
  gql "UnsignedInteger"

useGraphClause :: TypeDefinition
useGraphClause = def "UseGraphClause" $
  doc "A GQL use graph clause, equivalent to a graph expression" $
  gql "GraphExpression"

valueExpression :: TypeDefinition
valueExpression = def "ValueExpression" $
  doc "A GQL value expression: one of several alternative forms" $
  T.union [
    "signed">: gql "SignedExpr",
    "multDiv">: gql "MultDivExpr",
    "addSubtract">: gql "AddSubtractExpr",
    "concatenation">: gql "ConcatenationExpr",
    "not">: gql "NotExpr",
    "isNot">: gql "IsNotExpr",
    "conjunctive">: gql "ConjunctiveExpr",
    "disjunctive">: gql "DisjunctiveExpr",
    "comparison">: gql "ComparisonExpr",
    "predicate">: gql "Predicate",
    "normalizedPredicate">: gql "NormalizedPredicateExpr",
    "propertyGraph">: gql "GraphExpression",
    "bindingTable">: gql "BindingTableExpression",
    "valueFunction">: gql "ValueFunction",
    "primary">: gql "PrimaryValueExpression"]

valueFunction :: TypeDefinition
valueFunction = def "ValueFunction" $
  doc "A GQL value function: one of several alternative forms" $
  T.union [
    "numeric">: gql "NumericValueFunction",
    "datetimeSubtraction">: gql "DatetimeSubtraction",
    "datetime">: gql "DatetimeValueFunction",
    "duration">: gql "DurationValueFunction",
    "characterOrByteString">: gql "CharacterOrByteStringFunction",
    "list">: gql "ListValueFunction"]

valueInitializer :: TypeDefinition
valueInitializer = def "ValueInitializer" $
  doc "A GQL value initializer" $
  T.unit

valueQueryExpression :: TypeDefinition
valueQueryExpression = def "ValueQueryExpression" $
  doc "A GQL value query expression, equivalent to a nested query specification" $
  gql "NestedQuerySpecification"

valueType :: TypeDefinition
valueType = def "ValueType" $
  doc "A GQL value type: one of several alternative forms" $
  T.union [
    "predefinedType">: gql "PredefinedType",
    "pathValueType">: gql "PathValueType",
    "listValueTypeAlt1">: gql "ListValueTypeAlt1",
    "listValueTypeAlt2">: gql "ListValueTypeAlt2",
    "listValueTypeAlt3">: gql "ListValueTypeAlt3",
    "recordType">: gql "RecordType",
    "openDynamicUnionType">: gql "OpenDynamicUnionType",
    "dynamicPropertyValueType">: gql "DynamicPropertyValueType",
    "closedDynamicUnionTypeAlt1">: gql "ClosedDynamicUnionTypeAlt1",
    "closedDynamicUnionTypeAlt2">: gql "ClosedDynamicUnionTypeAlt2"]

valueTypePredicate :: TypeDefinition
valueTypePredicate = def "ValueTypePredicate" $
  doc "A GQL value type predicate" $
  T.record [
    "valueExpression">: gql "PrimaryValueExpression",
    "valueTypePart">: gql "ValueTypePredicatePart2"]

valueTypePredicatePart2 :: TypeDefinition
valueTypePredicatePart2 = def "ValueTypePredicatePart2" $
  doc "A GQL value type predicate part2" $
  T.record [
    "not">: T.boolean,
    "typed">: gql "Typed",
    "valueType">: gql "ValueType"]

valueVariableDefinition :: TypeDefinition
valueVariableDefinition = def "ValueVariableDefinition" $
  doc "A GQL value variable definition" $
  T.record [
    "variable">: gql "BindingVariable",
    "initializer">: gql "OptTypedValueInitializer"]

varbinaryType :: TypeDefinition
varbinaryType = def "VarbinaryType" $
  doc "A GQL varbinary type" $
  T.record [
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

varcharType :: TypeDefinition
varcharType = def "VarcharType" $
  doc "A GQL varchar type" $
  T.record [
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

variableScopeClause :: TypeDefinition
variableScopeClause = def "VariableScopeClause" $
  doc "An optional GQL variable scope clause" $
  T.optional $ gql "BindingVariableReferenceList"

verboseBinaryExactNumericType :: TypeDefinition
verboseBinaryExactNumericType = def "VerboseBinaryExactNumericType" $
  doc "A GQL verbose binary exact numeric type: one of several alternative forms" $
  T.union [
    "integer8">: gql "Integer8Type",
    "integer16">: gql "Integer16Type",
    "integer32">: gql "Integer32Type",
    "integer64">: gql "Integer64Type",
    "integer128">: gql "Integer128Type",
    "integer256">: gql "Integer256Type",
    "smallInteger">: gql "SmallIntegerType",
    "integerWithPrecision">: gql "IntegerWithPrecision",
    "bigInteger">: gql "BigIntegerType"]

whenOperand :: TypeDefinition
whenOperand = def "WhenOperand" $
  doc "A GQL when operand: one of several alternative forms" $
  T.union [
    "valueExpression">: gql "NonParenthesizedPrimaryValueExpression",
    "comparison">: gql "ComparisonPredicatePart2",
    "nullPredicate">: gql "NullPredicatePart2",
    "valueTypePredicate">: gql "ValueTypePredicatePart2",
    "normalizedPredicate">: gql "NormalizedPredicatePart2",
    "directedPredicate">: gql "DirectedPredicatePart2",
    "labeledPredicate">: gql "LabeledPredicatePart2",
    "sourcePredicate">: gql "SourcePredicate",
    "destinationPredicate">: gql "DestinationPredicate"]

whenOperandList :: TypeDefinition
whenOperandList = def "WhenOperandList" $
  doc "A list of GQL when operand list elements" $
  nonemptyList $ gql "WhenOperand"

whereClause :: TypeDefinition
whereClause = def "WhereClause" $
  doc "A GQL where clause, equivalent to a search condition" $
  gql "SearchCondition"

yieldClause :: TypeDefinition
yieldClause = def "YieldClause" $
  doc "A GQL yield clause, equivalent to a yield item list" $
  gql "YieldItemList"

yieldItem :: TypeDefinition
yieldItem = def "YieldItem" $
  doc "A GQL yield item" $
  T.record [
    "name">: gql "YieldItemName",
    "alias">: T.optional $ gql "YieldItemAlias"]

yieldItemAlias :: TypeDefinition
yieldItemAlias = def "YieldItemAlias" $
  doc "A GQL yield item alias, equivalent to a binding variable" $
  gql "BindingVariable"

yieldItemList :: TypeDefinition
yieldItemList = def "YieldItemList" $
  doc "A list of GQL yield item list elements" $
  nonemptyList $ gql "YieldItem"

yieldItemName :: TypeDefinition
yieldItemName = def "YieldItemName" $
  doc "A GQL yield item name, equivalent to a field name" $
  gql "FieldName"

zonedDatetimeType :: TypeDefinition
zonedDatetimeType = def "ZonedDatetimeType" $
  doc "A GQL zoned datetime type" $
  T.record [
    "notNull">: T.boolean]

zonedTimeType :: TypeDefinition
zonedTimeType = def "ZonedTimeType" $
  doc "A GQL zoned time type" $
  T.record [
    "notNull">: T.boolean]
