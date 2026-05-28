module Hydra.Sources.Gql.OpenGql where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel hiding (parameterName)
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
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
            moduleDefinitions = (map toTypeDef definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleDescription = Just ("A GQL model based on the OpenGQL ANTLR grammar, version 15b256b (2024-09-05), available at:"
    ++ " https://github.com/opengql/grammar/blob/main/GQL.g4")}
  where
    definitions = [
      gqlProgram,
      programActivity,
      sessionActivity,
      sessionSetAndResetCommands,
      transactionActivity,
      startAndMaybeProcedureAndMaybeEnd,
      procedureAndMaybeEnd,
      endTransactionCommand,
      sessionSetCommand,
      sessionSetSchemaClause,
      sessionSetGraphClause,
      sessionSetTimeZoneClause,
      setTimeZoneValue,
      sessionSetParameterClause,
      sessionSetGraphParameterClause,
      sessionSetBindingTableParameterClause,
      sessionSetValueParameterClause,
      sessionSetParameterName,
      sessionResetCommand,
      sessionResetArguments,
      allParametersOrCharacteristics,
      parametersOrCharacteristics,
      parameterSessionSpecification,
      sessionCloseCommand,
      sessionParameterSpecification,
      startTransactionCommand,
      transactionCharacteristics,
      transactionMode,
      transactionAccessMode,
      rollbackCommand,
      commitCommand,
      nestedProcedureSpecification,
      procedureSpecification,
      nestedDataModifyingProcedureSpecification,
      nestedQuerySpecification,
      procedureBody,
      bindingVariableDefinitionBlock,
      bindingVariableDefinition,
      statementBlock,
      statement,
      nextStatement,
      graphVariableDefinition,
      optTypedGraphInitializer,
      typedGraphReferenceValueType,
      graphInitializer,
      bindingTableVariableDefinition,
      optTypedBindingTableInitializer,
      typedBindingTableReferenceValueType,
      bindingTableInitializer,
      valueVariableDefinition,
      optTypedValueInitializer,
      typedValueType,
      valueInitializer,
      graphExpression,
      currentGraph,
      bindingTableExpression,
      nestedBindingTableQuerySpecification,
      objectExpressionPrimary,
      linearCatalogModifyingStatement,
      simpleCatalogModifyingStatement,
      primitiveCatalogModifyingStatement,
      createSchemaStatement,
      dropSchemaStatement,
      createGraphStatement,
      createGraphOption,
      graphTypeOption,
      openGraphType,
      ofGraphType,
      graphTypeLikeGraph,
      graphSource,
      typedGraphTypeReference,
      typedNestedGraphTypeSpecification,
      dropGraphStatement,
      createGraphTypeStatement,
      createGraphTypeOption,
      graphTypeSource,
      copyOfGraphType,
      dropGraphTypeStatement,
      callCatalogModifyingProcedureStatement,
      linearDataModifyingStatement,
      focusedLinearDataModifyingStatement,
      focusedLinearDataModifyingStatementBody,
      focusedNestedDataModifyingProcedureSpecification,
      ambientLinearDataModifyingStatement,
      ambientLinearDataModifyingStatementBody,
      simpleLinearDataAccessingStatement,
      simpleDataAccessingStatement,
      simpleDataModifyingStatement,
      primitiveDataModifyingStatement,
      insertStatement,
      setStatement,
      setItemList,
      setItem,
      setPropertyItem,
      setAllPropertiesItem,
      setLabelItem,
      removeStatement,
      removeItemList,
      removeItem,
      removePropertyItem,
      removeLabelItem,
      deleteStatement,
      detachOption,
      deleteItemList,
      deleteItem,
      callDataModifyingProcedureStatement,
      compositeQueryStatement,
      compositeQueryExpression,
      compositeQueryExpressionConjunction,
      queryConjunction,
      setOperator,
      setOperatorType,
      compositeQueryPrimary,
      linearQueryStatement,
      focusedLinearQueryStatement,
      focusedLinearQueryStatementPartsAndResult,
      focusedLinearQueryStatementPart,
      focusedLinearQueryAndPrimitiveResultStatementPart,
      focusedPrimitiveResultStatement,
      focusedNestedQuerySpecification,
      ambientLinearQueryStatement,
      ambientLinearQueryStatementSimpleAndPrimitiveResult,
      simpleLinearQueryStatement,
      simpleQueryStatement,
      primitiveQueryStatement,
      matchStatement,
      simpleMatchStatement,
      optionalMatchStatement,
      optionalOperand,
      matchStatementBlock,
      callQueryStatement,
      filterStatement,
      letStatement,
      letVariableDefinitionList,
      letVariableDefinition,
      bindingEqualsValue,
      forStatement,
      forItem,
      forItemAlias,
      forItemSource,
      forOrdinalityOrOffset,
      ordinalityOrOffsetType,
      orderByAndPageStatement,
      orderByAndOptionalOffsetAndLimit,
      offsetAndOptionalLimit,
      primitiveResultStatement,
      returnAndOptionalOrderByAndPage,
      returnStatement,
      returnStatementBody,
      returnItemsAndGroupBy,
      returnItems,
      returnItemList,
      returnItem,
      returnItemAlias,
      selectStatement,
      selectItems,
      selectStatementBodyAndClauses,
      selectItemList,
      selectItem,
      selectItemAlias,
      havingClause,
      selectStatementBody,
      selectGraphMatchList,
      selectGraphMatch,
      selectQuerySpecification,
      graphAndNestedQuerySpecification,
      callProcedureStatement,
      procedureCall,
      inlineProcedureCall,
      variableScopeClause,
      bindingVariableReferenceList,
      namedProcedureCall,
      procedureArgumentList,
      procedureArgument,
      atSchemaClause,
      useGraphClause,
      graphPatternBindingTable,
      graphPatternYieldClause,
      graphPatternYieldItemList,
      graphPatternYieldItem,
      graphPattern,
      matchMode,
      repeatableElementsMatchMode,
      differentEdgesMatchMode,
      elementBindingsOrElements,
      edgeBindingsOrEdges,
      pathPatternList,
      pathPattern,
      pathVariableDeclaration,
      keepClause,
      graphPatternWhereClause,
      insertGraphPattern,
      insertPathPatternList,
      insertPathPattern,
      insertEdgeAndNode,
      insertNodePattern,
      insertEdgePattern,
      insertEdgePointingLeft,
      insertEdgePointingRight,
      insertEdgeUndirected,
      insertElementPatternFiller,
      labelAndPropertySetSpecification,
      pathPatternPrefix,
      pathModePrefix,
      pathMode,
      pathSearchPrefix,
      allPathSearch,
      pathOrPaths,
      anyPathSearch,
      numberOfPaths,
      shortestPathSearch,
      allShortestPathSearch,
      anyShortestPathSearch,
      countedShortestPathSearch,
      countedShortestGroupSearch,
      numberOfGroups,
      pathPatternExpression,
      pathTerm,
      pathFactor,
      quantifiedPathPrimary,
      questionedPathPrimary,
      pathPrimary,
      elementPattern,
      nodePattern,
      elementPatternFiller,
      elementVariableDeclaration,
      isLabelExpression,
      isOrColon,
      elementPatternPredicate,
      elementPatternWhereClause,
      elementPropertySpecification,
      propertyKeyValuePairList,
      propertyKeyValuePair,
      edgePattern,
      fullEdgePattern,
      fullEdgePointingLeft,
      fullEdgeUndirected,
      fullEdgePointingRight,
      fullEdgeLeftOrUndirected,
      fullEdgeUndirectedOrRight,
      fullEdgeLeftOrRight,
      fullEdgeAnyDirection,
      abbreviatedEdgePattern,
      parenthesizedPathPatternExpression,
      subpathVariableDeclaration,
      parenthesizedPathPatternWhereClause,
      labelExpression,
      conjunctionLabelExpression,
      disjunctionLabelExpression,
      pathVariableReference,
      elementVariableReference,
      graphPatternQuantifier,
      fixedQuantifier,
      generalQuantifier,
      lowerBound,
      upperBound,
      simplifiedPathPatternExpression,
      simplifiedDefaultingLeft,
      simplifiedDefaultingUndirected,
      simplifiedDefaultingRight,
      simplifiedDefaultingLeftOrUndirected,
      simplifiedDefaultingUndirectedOrRight,
      simplifiedDefaultingLeftOrRight,
      simplifiedDefaultingAnyDirection,
      simplifiedContents,
      simplifiedPathUnion,
      simplifiedMultisetAlternation,
      simplifiedTerm,
      simplifiedConcatenation,
      simplifiedFactorLow,
      simplifiedConjunction,
      simplifiedFactorHigh,
      simplifiedQuantified,
      simplifiedQuestioned,
      simplifiedTertiary,
      simplifiedDirectionOverride,
      simplifiedOverrideLeft,
      simplifiedOverrideUndirected,
      simplifiedOverrideRight,
      simplifiedOverrideLeftOrUndirected,
      simplifiedOverrideUndirectedOrRight,
      simplifiedOverrideLeftOrRight,
      simplifiedOverrideAnyDirection,
      simplifiedSecondary,
      simplifiedNegation,
      simplifiedPrimary,
      whereClause,
      yieldClause,
      yieldItemList,
      yieldItem,
      yieldItemName,
      yieldItemAlias,
      groupByClause,
      groupingElementList,
      groupingElement,
      orderByClause,
      sortSpecificationList,
      sortSpecification,
      sortKey,
      orderingSpecification,
      nullOrdering,
      limitClause,
      offsetClause,
      offsetSynonym,
      schemaReference,
      absoluteCatalogSchemaReference,
      absoluteDirectoryAndSchema,
      catalogSchemaParentAndName,
      relativeCatalogSchemaReference,
      relativeDirectoryAndSchema,
      predefinedSchemaReference,
      absoluteDirectoryPath,
      relativeDirectoryPath,
      simpleDirectoryPath,
      graphReference,
      parentAndGraphName,
      catalogGraphParentAndName,
      homeGraph,
      graphTypeReference,
      catalogGraphTypeParentAndName,
      bindingTableReference,
      parentAndTableName,
      procedureReference,
      catalogProcedureParentAndName,
      catalogObjectParentReference,
      schemaAndObjects,
      referenceParameterSpecification,
      nestedGraphTypeSpecification,
      graphTypeSpecificationBody,
      elementTypeList,
      elementTypeSpecification,
      nodeTypeSpecification,
      nodeTypePattern,
      nodeSynonymAndTypeName,
      nodeTypePhrase,
      nodeTypePhraseFiller,
      nodeTypeNameWithFiller,
      nodeTypeFiller,
      nodeKeyLabelSetWithContent,
      localNodeTypeAlias,
      nodeTypeImpliedContent,
      nodeLabelSetWithProperties,
      nodeTypeKeyLabelSet,
      nodeTypeLabelSet,
      nodeTypePropertyTypes,
      edgeTypeSpecification,
      edgeTypePattern,
      edgeKindAndSynonym,
      edgeTypePatternType,
      edgeTypePhrase,
      edgeTypePhraseFiller,
      edgeTypeNameWithFiller,
      edgeTypeFiller,
      edgeKeyLabelSetWithContent,
      edgeTypeImpliedContent,
      edgeLabelSetWithProperties,
      edgeTypeKeyLabelSet,
      edgeTypeLabelSet,
      edgeTypePropertyTypes,
      edgeTypePatternDirected,
      edgeTypePatternPointingRight,
      edgeTypePatternPointingLeft,
      edgeTypePatternUndirected,
      arcTypePointingRight,
      arcTypePointingLeft,
      arcTypeUndirected,
      sourceNodeTypeReference,
      destinationNodeTypeReference,
      edgeKind,
      endpointPairPhrase,
      endpointPair,
      endpointPairDirected,
      endpointPairPointingRight,
      endpointPairPointingLeft,
      endpointPairUndirected,
      connectorPointingRight,
      connectorUndirected,
      sourceNodeTypeAlias,
      destinationNodeTypeAlias,
      labelSetPhrase,
      isOrColonWithLabels,
      labelSetSpecification,
      propertyTypesSpecification,
      propertyTypeList,
      propertyType,
      propertyValueType,
      bindingTableType,
      valueType,
      listValueTypeAlt1,
      listValueTypeAlt2,
      listValueTypeAlt3,
      openDynamicUnionType,
      dynamicPropertyValueType,
      closedDynamicUnionTypeAlt1,
      closedDynamicUnionTypeAlt2,
      typed,
      predefinedType,
      booleanType,
      characterStringType,
      stringType,
      charType,
      varcharType,
      byteStringType,
      bytesType,
      binaryType,
      varbinaryType,
      minLength,
      fixedLength,
      maxLength,
      numericType,
      exactNumericType,
      binaryExactNumericType,
      signedBinaryExactNumericType,
      int8Type,
      int16Type,
      int32Type,
      int64Type,
      int128Type,
      int256Type,
      smallIntType,
      bigIntType,
      intWithPrecision,
      signedVerboseBinaryExactNumericType,
      unsignedBinaryExactNumericType,
      uint8Type,
      uint16Type,
      uint32Type,
      uint64Type,
      uint128Type,
      uint256Type,
      uSmallIntType,
      uBigIntType,
      uintWithPrecision,
      verboseBinaryExactNumericType,
      integer8Type,
      integer16Type,
      integer32Type,
      integer64Type,
      integer128Type,
      integer256Type,
      smallIntegerType,
      bigIntegerType,
      integerWithPrecision,
      precision,
      decimalExactNumericType,
      precisionAndScale,
      scale,
      approximateNumericType,
      float16Type,
      float32Type,
      float64Type,
      float128Type,
      float256Type,
      floatTypeWithPrecision,
      realType,
      doubleTypeWithPrecision,
      temporalType,
      temporalInstantType,
      datetimeType,
      zonedDatetimeType,
      timestampWithTimeZoneType,
      localDatetimeTypeChoice,
      localDatetimeType,
      timestampWithoutTimeZoneType,
      dateType,
      timeType,
      zonedTimeType,
      timeWithTimeZoneType,
      localTimeTypeChoice,
      localTimeType,
      timeWithoutTimeZoneType,
      temporalDurationType,
      temporalDurationQualifier,
      referenceValueType,
      immaterialValueType,
      nullType,
      emptyType,
      graphReferenceValueType,
      closedGraphReferenceValueType,
      openGraphReferenceValueType,
      bindingTableReferenceValueType,
      nodeReferenceValueType,
      closedNodeReferenceValueType,
      openNodeReferenceValueType,
      edgeReferenceValueType,
      closedEdgeReferenceValueType,
      openEdgeReferenceValueType,
      pathValueType,
      listValueTypeName,
      listValueTypeNameSynonym,
      recordType,
      anyRecordType,
      specifiedRecordType,
      fieldTypesSpecification,
      fieldTypeList,
      notNull,
      fieldType,
      searchCondition,
      predicate,
      comparisonPredicatePart2,
      compOp,
      existsPredicate,
      nullPredicate,
      nullPredicatePart2,
      valueTypePredicate,
      valueTypePredicatePart2,
      normalizedPredicatePart2,
      directedPredicate,
      directedPredicatePart2,
      labeledPredicate,
      labeledPredicatePart2,
      isLabeledOrColon,
      sourceDestinationPredicate,
      nodeReference,
      sourcePredicate,
      destinationPredicate,
      edgeReference,
      allDifferentPredicate,
      samePredicate,
      propertyExistsPredicate,
      valueExpression,
      signedExpr,
      multDivExpr,
      addSubtractExpr,
      concatenationExpr,
      notExpr,
      isNotExpr,
      conjunctiveExpr,
      disjunctiveExpr,
      comparisonExpr,
      normalizedPredicateExpr,
      sign,
      multDivOperator,
      addSubtractOperator,
      disjunctiveOperator,
      valueFunction,
      booleanValueExpression,
      characterOrByteStringFunction,
      subCharacterOrByteString,
      side,
      trimSingleCharacterOrByteString,
      foldCharacterString,
      case_,
      trimMultiCharacterCharacterString,
      trimType,
      normalizeCharacterString,
      nodeReferenceValueExpression,
      edgeReferenceValueExpression,
      aggregatingValueExpression,
      primaryValueExpression,
      parenthesizedValueExpression,
      nonParenthesizedPrimaryValueExpression,
      nonParenthesizedPrimaryValueExpressionSpecialCase,
      unsignedValueSpecification,
      nonNegativeIntegerSpecification,
      generalValueSpecification,
      dynamicParameterSpecification,
      letValueExpression,
      valueQueryExpression,
      caseExpression,
      caseAbbreviation,
      nullIfAbbreviation,
      caseSpecification,
      simpleCase,
      searchedCase,
      simpleWhenClause,
      searchedWhenClause,
      elseClause,
      caseOperand,
      whenOperandList,
      whenOperand,
      result,
      resultExpression,
      castSpecification,
      castOperand,
      castTarget,
      aggregateFunction,
      generalSetFunction,
      binarySetFunction,
      generalSetFunctionType,
      setQuantifier,
      binarySetFunctionType,
      dependentValueExpression,
      independentValueExpression,
      elementIdFunction,
      propertyReference,
      bindingVariableReference,
      pathValueExpression,
      pathValueConstructor,
      pathValueConstructorByEnumeration,
      pathElementList,
      pathElementListStart,
      pathElementListStep,
      listValueExpression,
      listValueFunction,
      trimListFunction,
      elementsFunction,
      listValueConstructor,
      listValueConstructorByEnumeration,
      listElementList,
      listElement,
      recordConstructor,
      fieldsSpecification,
      fieldList,
      field,
      truthValue,
      numericValueExpression,
      signedNumericValueExpression,
      mulDivNumericValueExpression,
      addSubNumericValueExpression,
      numericValueFunction,
      lengthExpression,
      cardinalityExpression,
      cardinalityArgumentExpression,
      charLengthExpression,
      byteLengthExpression,
      pathLengthExpression,
      absoluteValueExpression,
      modulusExpression,
      numericValueExpressionDividend,
      numericValueExpressionDivisor,
      trigonometricFunction,
      trigonometricFunctionName,
      generalLogarithmFunction,
      generalLogarithmBase,
      generalLogarithmArgument,
      commonLogarithm,
      naturalLogarithm,
      exponentialFunction,
      powerFunction,
      numericValueExpressionBase,
      numericValueExpressionExponent,
      squareRoot,
      floorFunction,
      ceilingFunction,
      characterStringValueExpression,
      byteStringValueExpression,
      trimOperands,
      trimCharacterOrByteStringSource,
      trimSpecification,
      trimCharacterOrByteString,
      normalForm,
      stringLength,
      datetimeValueExpression,
      datetimeValueFunction,
      dateFunction,
      timeFunction,
      localtimeFunction,
      datetimeFunction,
      localdatetimeFunction,
      dateFunctionParameters,
      timeFunctionParameters,
      datetimeFunctionParameters,
      durationValueExpression,
      datetimeSubtraction,
      datetimeSubtractionParameters,
      datetimeValueExpression1,
      datetimeValueExpression2,
      durationValueFunction,
      durationFunction,
      durationFunctionParameters,
      objectName,
      objectNameOrBindingVariable,
      directoryName,
      schemaName,
      graphName,
      delimitedGraphName,
      graphTypeName,
      nodeTypeName,
      edgeTypeName,
      bindingTableName,
      delimitedBindingTableName,
      procedureName,
      labelName,
      propertyName,
      fieldName_,
      elementVariable,
      pathVariable,
      subpathVariable,
      bindingVariable,
      unsignedLiteral,
      generalLiteral,
      temporalLiteral,
      dateLiteral,
      timeLiteral,
      datetimeLiteral,
      listLiteral,
      recordLiteral,
      identifier,
      regularIdentifier,
      timeZoneString,
      characterStringLiteral,
      unsignedNumericLiteral,
      exactNumericLiteral,
      approximateNumericLiteral,
      unsignedInteger,
      unsignedDecimalInteger,
      nullLiteral,
      dateString,
      timeString,
      datetimeString,
      durationLiteral,
      durationString,
      nodeSynonym,
      edgesSynonym,
      edgeSynonym,
      implies,
      parameterName,
      booleanLiteral,
      byteStringLiteral]

abbreviatedEdgePattern :: Binding
abbreviatedEdgePattern = def "AbbreviatedEdgePattern" $
  T.enum ["leftArrow", "tilde", "rightArrow", "leftArrowTilde", "tildeRightArrow", "leftMinusRight", "minusSign"]

absoluteCatalogSchemaReference :: Binding
absoluteCatalogSchemaReference = def "AbsoluteCatalogSchemaReference" $
  T.union [
    "root">: T.unit,
    "directoryAndSchema">: gql "AbsoluteDirectoryAndSchema"]

absoluteDirectoryAndSchema :: Binding
absoluteDirectoryAndSchema = def "AbsoluteDirectoryAndSchema" $
  T.record [
    "directoryPath">: gql "AbsoluteDirectoryPath",
    "schemaName">: gql "SchemaName"]

absoluteDirectoryPath :: Binding
absoluteDirectoryPath = def "AbsoluteDirectoryPath" $
  T.maybe $ gql "SimpleDirectoryPath"

absoluteValueExpression :: Binding
absoluteValueExpression = def "AbsoluteValueExpression" $
  gql "ValueExpression"

addSubNumericValueExpression :: Binding
addSubNumericValueExpression = def "AddSubNumericValueExpression" $
  T.record [
    "left">: gql "NumericValueExpression",
    "operator">: gql "AddSubtractOperator",
    "right">: gql "NumericValueExpression"]

addSubtractExpr :: Binding
addSubtractExpr = def "AddSubtractExpr" $
  T.record [
    "left">: gql "ValueExpression",
    "operator">: gql "AddSubtractOperator",
    "right">: gql "ValueExpression"]

addSubtractOperator :: Binding
addSubtractOperator = def "AddSubtractOperator" $
  T.enum ["add", "subtract"]

aggregateFunction :: Binding
aggregateFunction = def "AggregateFunction" $
  T.union [
    "countAll">: T.unit,
    "generalSetFunction">: gql "GeneralSetFunction",
    "binarySetFunction">: gql "BinarySetFunction"]

aggregatingValueExpression :: Binding
aggregatingValueExpression = def "AggregatingValueExpression" $
  gql "ValueExpression"

allDifferentPredicate :: Binding
allDifferentPredicate = def "AllDifferentPredicate" $
  T.record [
    "references">: nonemptyList $ gql "ElementVariableReference"]

allParametersOrCharacteristics :: Binding
allParametersOrCharacteristics = def "AllParametersOrCharacteristics" $
  T.record [
    "all">: T.boolean,
    "type">: gql "ParametersOrCharacteristics"]

allPathSearch :: Binding
allPathSearch = def "AllPathSearch" $
  T.record [
    "mode">: T.maybe $ gql "PathMode",
    "orPaths">: T.maybe $ gql "PathOrPaths"]

allShortestPathSearch :: Binding
allShortestPathSearch = def "AllShortestPathSearch" $
  T.record [
    "mode">: T.maybe $ gql "PathMode",
    "orPaths">: T.maybe $ gql "PathOrPaths"]

ambientLinearDataModifyingStatement :: Binding
ambientLinearDataModifyingStatement = def "AmbientLinearDataModifyingStatement" $
  T.union [
    "simple">: gql "AmbientLinearDataModifyingStatementBody",
    "nested">: gql "NestedDataModifyingProcedureSpecification"]

ambientLinearDataModifyingStatementBody :: Binding
ambientLinearDataModifyingStatementBody = def "AmbientLinearDataModifyingStatementBody" $
  T.record [
    "simpleAccess">: gql "SimpleLinearDataAccessingStatement",
    "primitiveResult">: T.maybe $ gql "PrimitiveResultStatement"]

ambientLinearQueryStatement :: Binding
ambientLinearQueryStatement = def "AmbientLinearQueryStatement" $
  T.union [
    "simple">: gql "AmbientLinearQueryStatementSimpleAndPrimitiveResult",
    "nested">: gql "NestedQuerySpecification"]

ambientLinearQueryStatementSimpleAndPrimitiveResult :: Binding
ambientLinearQueryStatementSimpleAndPrimitiveResult = def "AmbientLinearQueryStatementSimpleAndPrimitiveResult" $
  T.record [
    "simple">: T.maybe $ gql "SimpleLinearQueryStatement",
    "primitiveResult">: gql "PrimitiveResultStatement"]

anyPathSearch :: Binding
anyPathSearch = def "AnyPathSearch" $
  T.record [
    "numberOfPaths">: T.maybe $ gql "NumberOfPaths",
    "mode">: T.maybe $ gql "PathMode",
    "orPaths">: T.maybe $ gql "PathOrPaths"]

anyRecordType :: Binding
anyRecordType = def "AnyRecordType" $
  T.record [
    "any">: T.boolean,
    "notNull">: T.boolean]

anyShortestPathSearch :: Binding
anyShortestPathSearch = def "AnyShortestPathSearch" $
  T.record [
    "mode">: T.maybe $ gql "PathMode",
    "orPaths">: T.maybe $ gql "PathOrPaths"]

approximateNumericLiteral :: Binding
approximateNumericLiteral = def "ApproximateNumericLiteral" $
  T.union [
    "scientificWithSuffix">: T.string,
    "scientificWithoutSuffix">: T.string,
    "commonWithSuffix">: T.string,
    "integerWithSuffix">: T.string]

approximateNumericType :: Binding
approximateNumericType = def "ApproximateNumericType" $
  T.union [
    "float16">: gql "Float16Type",
    "float32">: gql "Float32Type",
    "float64">: gql "Float64Type",
    "float128">: gql "Float128Type",
    "float256">: gql "Float256Type",
    "floatWithPrecision">: gql "FloatTypeWithPrecision",
    "real">: gql "RealType",
    "doubleWithPrecision">: gql "DoubleTypeWithPrecision"]

arcTypePointingLeft :: Binding
arcTypePointingLeft = def "ArcTypePointingLeft" $
  gql "EdgeTypeFiller"

arcTypePointingRight :: Binding
arcTypePointingRight = def "ArcTypePointingRight" $
  gql "EdgeTypeFiller"

arcTypeUndirected :: Binding
arcTypeUndirected = def "ArcTypeUndirected" $
  gql "EdgeTypeFiller"

atSchemaClause :: Binding
atSchemaClause = def "AtSchemaClause" $
  gql "SchemaReference"

bigIntType :: Binding
bigIntType = def "BigIntType" $
  T.record ["notNull">: T.boolean]

bigIntegerType :: Binding
bigIntegerType = def "BigIntegerType" $
  T.record ["notNull">: T.boolean]

binaryExactNumericType :: Binding
binaryExactNumericType = def "BinaryExactNumericType" $
  T.union [
    "signed">: gql "SignedBinaryExactNumericType",
    "unsigned">: gql "UnsignedBinaryExactNumericType"]

binarySetFunction :: Binding
binarySetFunction = def "BinarySetFunction" $
  T.record [
    "functionType">: gql "BinarySetFunctionType",
    "dependentValue">: gql "DependentValueExpression",
    "independentValue">: gql "IndependentValueExpression"]

binarySetFunctionType :: Binding
binarySetFunctionType = def "BinarySetFunctionType" $
  T.enum ["percentileCont", "percentileDisc"]

binaryType :: Binding
binaryType = def "BinaryType" $
  T.record [
    "fixedLength">: T.maybe $ gql "FixedLength",
    "notNull">: T.boolean]

bindingEqualsValue :: Binding
bindingEqualsValue = def "BindingEqualsValue" $
  T.record [
    "binding">: gql "BindingVariable",
    "value">: gql "ValueExpression"]

bindingTableExpression :: Binding
bindingTableExpression = def "BindingTableExpression" $
  T.union [
    "nested">: gql "NestedBindingTableQuerySpecification",
    "object">: gql "ObjectExpressionPrimary",
    "table">: gql "BindingTableReference",
    "name">: gql "ObjectNameOrBindingVariable"]

bindingTableInitializer :: Binding
bindingTableInitializer = def "BindingTableInitializer" T.unit

bindingTableName :: Binding
bindingTableName = def "BindingTableName" $
  T.union [
    "regularIdentifier">: T.string,
    "delimitedBindingTableName">: gql "DelimitedBindingTableName"]

bindingTableReference :: Binding
bindingTableReference = def "BindingTableReference" $
  T.union [
    "parentAndTableName">: gql "ParentAndTableName",
    "delimitedBindingTableName">: gql "DelimitedBindingTableName",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

bindingTableReferenceValueType :: Binding
bindingTableReferenceValueType = def "BindingTableReferenceValueType" $
  T.record [
    "bindingTableType">: gql "BindingTableType",
    "notNull">: T.boolean]

bindingTableType :: Binding
bindingTableType = def "BindingTableType" $
  T.record [
    "binding">: T.boolean,
    "fieldTypes">: gql "FieldTypesSpecification"]

bindingTableVariableDefinition :: Binding
bindingTableVariableDefinition = def "BindingTableVariableDefinition" $
  T.record [
    "binding">: T.boolean,
    "variable">: gql "BindingVariable",
    "initializer">: gql "OptTypedBindingTableInitializer"]

bindingVariable :: Binding
bindingVariable = def "BindingVariable" $
  T.string

bindingVariableDefinition :: Binding
bindingVariableDefinition = def "BindingVariableDefinition" $
  T.union [
    "graph">: gql "GraphVariableDefinition",
    "table">: gql "BindingTableVariableDefinition",
    "value">: gql "ValueVariableDefinition"]

bindingVariableDefinitionBlock :: Binding
bindingVariableDefinitionBlock = def "BindingVariableDefinitionBlock" $
  nonemptyList $ gql "BindingVariableDefinition"

bindingVariableReference :: Binding
bindingVariableReference = def "BindingVariableReference" $
  gql "BindingVariable"

bindingVariableReferenceList :: Binding
bindingVariableReferenceList = def "BindingVariableReferenceList" $
  nonemptyList $ gql "BindingVariableReference"

booleanLiteral :: Binding
booleanLiteral = def "BooleanLiteral" $
  T.enum ["true", "false", "unknown"]

booleanType :: Binding
booleanType = def "BooleanType" $
  T.record [
    "notNull">: T.boolean]

booleanValueExpression :: Binding
booleanValueExpression = def "BooleanValueExpression" $
  gql "ValueExpression"

byteLengthExpression :: Binding
byteLengthExpression = def "ByteLengthExpression" $
  gql "ByteStringValueExpression"

byteStringLiteral :: Binding
byteStringLiteral = def "ByteStringLiteral" $
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

byteStringType :: Binding
byteStringType = def "ByteStringType" $
  T.union [
    "bytesType">: gql "BytesType",
    "binaryType">: gql "BinaryType",
    "varbinaryType">: gql "VarbinaryType"]

byteStringValueExpression :: Binding
byteStringValueExpression = def "ByteStringValueExpression" $
  gql "ValueExpression"

bytesType :: Binding
bytesType = def "BytesType" $
  T.record [
    "minLength">: T.maybe $ gql "MinLength",
    "maxLength">: T.maybe $ gql "MaxLength",
    "notNull">: T.boolean]

callCatalogModifyingProcedureStatement :: Binding
callCatalogModifyingProcedureStatement = def "CallCatalogModifyingProcedureStatement" $
  gql "CallProcedureStatement"

callDataModifyingProcedureStatement :: Binding
callDataModifyingProcedureStatement = def "CallDataModifyingProcedureStatement" $
  gql "CallProcedureStatement"

callProcedureStatement :: Binding
callProcedureStatement = def "CallProcedureStatement" $
  T.record [
    "optional">: T.boolean,
    "call">: gql "ProcedureCall"]

callQueryStatement :: Binding
callQueryStatement = def "CallQueryStatement" $
  gql "CallProcedureStatement"

cardinalityArgumentExpression :: Binding
cardinalityArgumentExpression = def "CardinalityArgumentExpression" $
  gql "ValueExpression"

cardinalityExpression :: Binding
cardinalityExpression = def "CardinalityExpression" $
  T.union [
    "cardinality">: gql "CardinalityArgumentExpression",
    "size">: gql "ListValueExpression"]

caseAbbreviation :: Binding
caseAbbreviation = def "CaseAbbreviation" $
  T.union [
    "nullIf">: gql "NullIfAbbreviation",
    "coalesce">: nonemptyList $ gql "ValueExpression"]

caseExpression :: Binding
caseExpression = def "CaseExpression" $
  T.union [
    "abbreviation">: gql "CaseAbbreviation",
    "specification">: gql "CaseSpecification"]

caseOperand :: Binding
caseOperand = def "CaseOperand" $
  T.union [
    "valueExpression">: gql "NonParenthesizedPrimaryValueExpression",
    "elementReference">: gql "ElementVariableReference"]

caseSpecification :: Binding
caseSpecification = def "CaseSpecification" $
  T.union [
    "simple">: gql "SimpleCase",
    "searched">: gql "SearchedCase"]

case_ :: Binding
case_ = def "Case" $
  T.enum ["upper", "lower"]

castOperand :: Binding
castOperand = def "CastOperand" $
  T.union [
    "valueExpression">: gql "ValueExpression",
    "nullLiteral">: T.unit]

castSpecification :: Binding
castSpecification = def "CastSpecification" $
  T.record [
    "operand">: gql "CastOperand",
    "target">: gql "CastTarget"]

castTarget :: Binding
castTarget = def "CastTarget" $
  gql "ValueType"

catalogGraphParentAndName :: Binding
catalogGraphParentAndName = def "CatalogGraphParentAndName" $
  T.record [
    "parentReference">: T.maybe $ gql "CatalogObjectParentReference",
    "graphName">: gql "GraphName"]

catalogGraphTypeParentAndName :: Binding
catalogGraphTypeParentAndName = def "CatalogGraphTypeParentAndName" $
  T.record [
    "parentReference">: T.maybe $ gql "CatalogObjectParentReference",
    "graphTypeName">: gql "GraphTypeName"]

catalogObjectParentReference :: Binding
catalogObjectParentReference = def "CatalogObjectParentReference" $
  T.union [
    "schemaAndObjects">: gql "SchemaAndObjects",
    "objectsOnly">: nonemptyList $ gql "ObjectName"]

catalogProcedureParentAndName :: Binding
catalogProcedureParentAndName = def "CatalogProcedureParentAndName" $
  T.record [
    "parentReference">: T.maybe $ gql "CatalogObjectParentReference",
    "procedureName">: gql "ProcedureName"]

catalogSchemaParentAndName :: Binding
catalogSchemaParentAndName = def "CatalogSchemaParentAndName" $
  gql "AbsoluteDirectoryAndSchema"

ceilingFunction :: Binding
ceilingFunction = def "CeilingFunction" $
  gql "NumericValueExpression"

charLengthExpression :: Binding
charLengthExpression = def "CharLengthExpression" $
  gql "CharacterStringValueExpression"

charType :: Binding
charType = def "CharType" $
  T.record [
    "fixedLength">: T.maybe $ gql "FixedLength",
    "notNull">: T.boolean]

characterOrByteStringFunction :: Binding
characterOrByteStringFunction = def "CharacterOrByteStringFunction" $
  T.union [
    "sub">: gql "SubCharacterOrByteString",
    "trimSingle">: gql "TrimSingleCharacterOrByteString",
    "fold">: gql "FoldCharacterString",
    "trimMultiCharacter">: gql "TrimMultiCharacterCharacterString",
    "normalize">: gql "NormalizeCharacterString"]

characterStringLiteral :: Binding
characterStringLiteral = def "CharacterStringLiteral" $
  T.string

characterStringType :: Binding
characterStringType = def "CharacterStringType" $
  T.union [
    "stringType">: gql "StringType",
    "charType">: gql "CharType",
    "varcharType">: gql "VarcharType"]

characterStringValueExpression :: Binding
characterStringValueExpression = def "CharacterStringValueExpression" $
  gql "ValueExpression"

closedDynamicUnionTypeAlt1 :: Binding
closedDynamicUnionTypeAlt1 = def "ClosedDynamicUnionTypeAlt1" $
  T.record [
    "anyValue">: T.maybe T.boolean,
    "valueTypes">: nonemptyList $ gql "ValueType"]

closedDynamicUnionTypeAlt2 :: Binding
closedDynamicUnionTypeAlt2 = def "ClosedDynamicUnionTypeAlt2" $
  T.record [
    "valueTypes">: nonemptyList $ gql "ValueType"]

closedEdgeReferenceValueType :: Binding
closedEdgeReferenceValueType = def "ClosedEdgeReferenceValueType" $
  T.record [
    "edgeTypeSpec">: gql "EdgeTypeSpecification",
    "notNull">: T.boolean]

closedGraphReferenceValueType :: Binding
closedGraphReferenceValueType = def "ClosedGraphReferenceValueType" $
  T.record [
    "property">: T.boolean,
    "nestedSpec">: gql "NestedGraphTypeSpecification",
    "notNull">: T.boolean]

closedNodeReferenceValueType :: Binding
closedNodeReferenceValueType = def "ClosedNodeReferenceValueType" $
  T.record [
    "nodeTypeSpec">: gql "NodeTypeSpecification",
    "notNull">: T.boolean]

commitCommand :: Binding
commitCommand = def "CommitCommand" $
  T.unit

commonLogarithm :: Binding
commonLogarithm = def "CommonLogarithm" $
  gql "NumericValueExpression"

compOp :: Binding
compOp = def "CompOp" $
  T.enum [
    "equals",
    "notEquals",
    "lessThan",
    "greaterThan",
    "lessThanOrEquals",
    "greaterThanOrEquals"]

comparisonExpr :: Binding
comparisonExpr = def "ComparisonExpr" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "comparison">: gql "ComparisonPredicatePart2"]

comparisonPredicatePart2 :: Binding
comparisonPredicatePart2 = def "ComparisonPredicatePart2" $
  T.record [
    "compOp">: gql "CompOp",
    "valueExpression">: gql "ValueExpression"]

compositeQueryExpression :: Binding
compositeQueryExpression = def "CompositeQueryExpression" $
  T.union [
    "simple">: gql "CompositeQueryExpressionConjunction",
    "primary">: gql "CompositeQueryPrimary"]

compositeQueryExpressionConjunction :: Binding
compositeQueryExpressionConjunction = def "CompositeQueryExpressionConjunction" $
  T.record [
    "left">: gql "CompositeQueryExpression",
    "conjunction">: gql "QueryConjunction",
    "right">: gql "CompositeQueryPrimary"]

compositeQueryPrimary :: Binding
compositeQueryPrimary = def "CompositeQueryPrimary" $
  gql "LinearQueryStatement"

compositeQueryStatement :: Binding
compositeQueryStatement = def "CompositeQueryStatement" $
  gql "CompositeQueryExpression"

concatenationExpr :: Binding
concatenationExpr = def "ConcatenationExpr" $
  T.record [
    "left">: gql "ValueExpression",
    "right">: gql "ValueExpression"]

conjunctionLabelExpression :: Binding
conjunctionLabelExpression = def "ConjunctionLabelExpression" $
  T.record [
    "left">: gql "LabelExpression",
    "right">: gql "LabelExpression"]

conjunctiveExpr :: Binding
conjunctiveExpr = def "ConjunctiveExpr" $
  T.record [
    "left">: gql "ValueExpression",
    "right">: gql "ValueExpression"]

connectorPointingRight :: Binding
connectorPointingRight = def "ConnectorPointingRight" $
  T.enum ["to", "rightArrow"]

connectorUndirected :: Binding
connectorUndirected = def "ConnectorUndirected" $
  T.enum ["to", "tilde"]

copyOfGraphType :: Binding
copyOfGraphType = def "CopyOfGraphType" $
  gql "GraphTypeReference"

countedShortestGroupSearch :: Binding
countedShortestGroupSearch = def "CountedShortestGroupSearch" $
  T.record [
    "numberOfGroups">: T.maybe $ gql "NumberOfGroups",
    "mode">: T.maybe $ gql "PathMode",
    "orPaths">: T.maybe $ gql "PathOrPaths",
    "groups">: T.boolean]

countedShortestPathSearch :: Binding
countedShortestPathSearch = def "CountedShortestPathSearch" $
  T.record [
    "numberOfPaths">: gql "NumberOfPaths",
    "mode">: T.maybe $ gql "PathMode",
    "orPaths">: T.maybe $ gql "PathOrPaths"]

createGraphOption :: Binding
createGraphOption = def "CreateGraphOption" $
  T.union [
    "graphIfNotExists">: T.boolean,
    "orReplace">: T.unit]

createGraphStatement :: Binding
createGraphStatement = def "CreateGraphStatement" $
  T.record [
    "createOption">: gql "CreateGraphOption",
    "parentAndName">: gql "CatalogGraphParentAndName",
    "type">: gql "GraphTypeOption",
    "source">: T.maybe $ gql "GraphSource"]

createGraphTypeOption :: Binding
createGraphTypeOption = def "CreateGraphTypeOption" $
  T.union [
    "typeIfNotExists">: T.boolean,
    "orReplace">: T.unit]

createGraphTypeStatement :: Binding
createGraphTypeStatement = def "CreateGraphTypeStatement" $
  T.record [
    "createOption">: gql "CreateGraphTypeOption",
    "parentAndName">: gql "CatalogGraphTypeParentAndName",
    "source">: gql "GraphTypeSource"]

createSchemaStatement :: Binding
createSchemaStatement = def "CreateSchemaStatement" $
  T.record [
    "ifNotExists">: T.boolean,
    "parentAndName">: gql "CatalogSchemaParentAndName"]

currentGraph :: Binding
currentGraph = def "CurrentGraph" $
  T.enum ["graph", "propertyGraph"]

dateFunction :: Binding
dateFunction = def "DateFunction" $
  T.union [
    "currentDate">: T.unit,
    "dateWithParams">: T.maybe $ gql "DateFunctionParameters"]

dateFunctionParameters :: Binding
dateFunctionParameters = def "DateFunctionParameters" $
  T.union [
    "dateString">: gql "DateString",
    "recordConstructor">: gql "RecordConstructor"]

dateLiteral :: Binding
dateLiteral = def "DateLiteral" $
  gql "DateString"

dateString :: Binding
dateString = def "DateString" $
  gql "CharacterStringLiteral"

dateType :: Binding
dateType = def "DateType" $
  T.record [
    "notNull">: T.boolean]

datetimeFunction :: Binding
datetimeFunction = def "DatetimeFunction" $
  T.union [
    "currentTimestamp">: T.unit,
    "zonedDatetimeWithParams">: T.maybe $ gql "DatetimeFunctionParameters"]

datetimeFunctionParameters :: Binding
datetimeFunctionParameters = def "DatetimeFunctionParameters" $
  T.union [
    "datetimeString">: gql "DatetimeString",
    "recordConstructor">: gql "RecordConstructor"]

datetimeLiteral :: Binding
datetimeLiteral = def "DatetimeLiteral" $
  gql "DatetimeString"

datetimeString :: Binding
datetimeString = def "DatetimeString" $
  gql "CharacterStringLiteral"

datetimeSubtraction :: Binding
datetimeSubtraction = def "DatetimeSubtraction" $
  T.record [
    "parameters">: gql "DatetimeSubtractionParameters",
    "temporalDurationQualifier">: T.maybe $ gql "TemporalDurationQualifier"]

datetimeSubtractionParameters :: Binding
datetimeSubtractionParameters = def "DatetimeSubtractionParameters" $
  T.record [
    "expression1">: gql "DatetimeValueExpression1",
    "expression2">: gql "DatetimeValueExpression2"]

datetimeType :: Binding
datetimeType = def "DatetimeType" $
  T.union [
    "zonedDatetime">: gql "ZonedDatetimeType",
    "timestampWithTimeZone">: gql "TimestampWithTimeZoneType"]

datetimeValueExpression :: Binding
datetimeValueExpression = def "DatetimeValueExpression" $
  gql "ValueExpression"

datetimeValueExpression1 :: Binding
datetimeValueExpression1 = def "DatetimeValueExpression1" $
  gql "DatetimeValueExpression"

datetimeValueExpression2 :: Binding
datetimeValueExpression2 = def "DatetimeValueExpression2" $
  gql "DatetimeValueExpression"

datetimeValueFunction :: Binding
datetimeValueFunction = def "DatetimeValueFunction" $
  T.union [
    "dateFunction">: gql "DateFunction",
    "timeFunction">: gql "TimeFunction",
    "datetimeFunction">: gql "DatetimeFunction",
    "localtimeFunction">: gql "LocaltimeFunction",
    "localdatetimeFunction">: gql "LocaldatetimeFunction"]

decimalExactNumericType :: Binding
decimalExactNumericType = def "DecimalExactNumericType" $
  T.maybe $ gql "PrecisionAndScale"

def :: String -> Type -> Binding
def = datatype ns

deleteItem :: Binding
deleteItem = def "DeleteItem" $
  gql "ValueExpression"

deleteItemList :: Binding
deleteItemList = def "DeleteItemList" $
  nonemptyList $ gql "DeleteItem"

deleteStatement :: Binding
deleteStatement = def "DeleteStatement" $
  T.record [
    "detach">: T.maybe $ gql "DetachOption",
    "items">: gql "DeleteItemList"]

delimitedBindingTableName :: Binding
delimitedBindingTableName = def "DelimitedBindingTableName" $
  T.string

delimitedGraphName :: Binding
delimitedGraphName = def "DelimitedGraphName" $
  T.string

dependentValueExpression :: Binding
dependentValueExpression = def "DependentValueExpression" $
  T.record [
    "setQuantifier">: T.maybe $ gql "SetQuantifier",
    "numericValue">: gql "NumericValueExpression"]

destinationNodeTypeAlias :: Binding
destinationNodeTypeAlias = def "DestinationNodeTypeAlias" $
  T.string

destinationNodeTypeReference :: Binding
destinationNodeTypeReference = def "DestinationNodeTypeReference" $
  T.union [
    "alias">: gql "DestinationNodeTypeAlias",
    "filler">: T.maybe $ gql "NodeTypeFiller"]

destinationPredicate :: Binding
destinationPredicate = def "DestinationPredicate" $
  T.record [
    "nodeReference">: gql "NodeReference",
    "not">: T.boolean,
    "destinationOf">: gql "EdgeReference"]

detachOption :: Binding
detachOption = def "DetachOption" $
  T.enum [
    "detach",
    "noDetach"]

differentEdgesMatchMode :: Binding
differentEdgesMatchMode = def "DifferentEdgesMatchMode" $
  gql "EdgeBindingsOrEdges"

directedPredicate :: Binding
directedPredicate = def "DirectedPredicate" $
  T.record [
    "elementVariableReference">: gql "ElementVariableReference",
    "directedPart">: gql "DirectedPredicatePart2"]

directedPredicatePart2 :: Binding
directedPredicatePart2 = def "DirectedPredicatePart2" $
  T.record [
    "not">: T.boolean]

directoryName :: Binding
directoryName = def "DirectoryName" $
  T.string

disjunctionLabelExpression :: Binding
disjunctionLabelExpression = def "DisjunctionLabelExpression" $
  T.record [
    "left">: gql "LabelExpression",
    "right">: gql "LabelExpression"]

disjunctiveExpr :: Binding
disjunctiveExpr = def "DisjunctiveExpr" $
  T.record [
    "left">: gql "ValueExpression",
    "operator">: gql "DisjunctiveOperator",
    "right">: gql "ValueExpression"]

disjunctiveOperator :: Binding
disjunctiveOperator = def "DisjunctiveOperator" $
  T.enum ["or", "xor"]

doubleTypeWithPrecision :: Binding
doubleTypeWithPrecision = def "DoubleTypeWithPrecision" $
  T.record [
    "precision">: T.boolean,
    "notNull">: T.boolean]

dropGraphStatement :: Binding
dropGraphStatement = def "DropGraphStatement" $
  T.record [
    "ifExists">: T.boolean,
    "parentAndName">: gql "CatalogGraphParentAndName"]

dropGraphTypeStatement :: Binding
dropGraphTypeStatement = def "DropGraphTypeStatement" $
  T.record [
    "ifExists">: T.boolean,
    "parentAndName">: gql "CatalogGraphTypeParentAndName"]

dropSchemaStatement :: Binding
dropSchemaStatement = def "DropSchemaStatement" $
  T.record [
    "ifExists">: T.boolean,
    "parentAndName">: gql "CatalogSchemaParentAndName"]

durationFunction :: Binding
durationFunction = def "DurationFunction" $
  gql "DurationFunctionParameters"

durationFunctionParameters :: Binding
durationFunctionParameters = def "DurationFunctionParameters" $
  T.union [
    "durationString">: gql "DurationString",
    "recordConstructor">: gql "RecordConstructor"]

durationLiteral :: Binding
durationLiteral = def "DurationLiteral" $
  gql "DurationString"

durationString :: Binding
durationString = def "DurationString" $
  gql "CharacterStringLiteral"

durationValueExpression :: Binding
durationValueExpression = def "DurationValueExpression" $
  gql "ValueExpression"

durationValueFunction :: Binding
durationValueFunction = def "DurationValueFunction" $
  T.union [
    "durationFunction">: gql "DurationFunction",
    "absoluteValue">: gql "AbsoluteValueExpression"]

dynamicParameterSpecification :: Binding
dynamicParameterSpecification = def "DynamicParameterSpecification" $
  gql "ParameterName"

dynamicPropertyValueType :: Binding
dynamicPropertyValueType = def "DynamicPropertyValueType" $
  T.record [
    "any">: T.maybe T.boolean,
    "property">: T.unit,
    "value">: T.unit,
    "notNull">: T.boolean]

edgeBindingsOrEdges :: Binding
edgeBindingsOrEdges = def "EdgeBindingsOrEdges" $
  T.union [
    "edgeBindings">: T.boolean,
    "edges">: T.unit]

edgeKeyLabelSetWithContent :: Binding
edgeKeyLabelSetWithContent = def "EdgeKeyLabelSetWithContent" $
  T.record [
    "keyLabelSet">: gql "EdgeTypeKeyLabelSet",
    "impliedContent">: T.maybe $ gql "EdgeTypeImpliedContent"]

edgeKind :: Binding
edgeKind = def "EdgeKind" $
  T.enum ["directed", "undirected"]

edgeKindAndSynonym :: Binding
edgeKindAndSynonym = def "EdgeKindAndSynonym" $
  T.record [
    "kind">: T.maybe $ gql "EdgeKind",
    "synonym">: gql "EdgeSynonym",
    "typeName">: T.maybe $ gql "EdgeTypeName"]

edgeLabelSetWithProperties :: Binding
edgeLabelSetWithProperties = def "EdgeLabelSetWithProperties" $
  T.record [
    "labelSet">: gql "EdgeTypeLabelSet",
    "propertyTypes">: gql "EdgeTypePropertyTypes"]

edgePattern :: Binding
edgePattern = def "EdgePattern" $
  T.union [
    "fullEdge">: gql "FullEdgePattern",
    "abbreviatedEdge">: gql "AbbreviatedEdgePattern"]

edgeReference :: Binding
edgeReference = def "EdgeReference" $
  gql "ElementVariableReference"

edgeReferenceValueExpression :: Binding
edgeReferenceValueExpression = def "EdgeReferenceValueExpression" $
  gql "PrimaryValueExpression"

edgeReferenceValueType :: Binding
edgeReferenceValueType = def "EdgeReferenceValueType" $
  T.union [
    "open">: gql "OpenEdgeReferenceValueType",
    "closed">: gql "ClosedEdgeReferenceValueType"]

edgeSynonym :: Binding
edgeSynonym = def "EdgeSynonym" $
  T.enum ["edge", "relationship"]

edgeTypeFiller :: Binding
edgeTypeFiller = def "EdgeTypeFiller" $
  T.union [
    "keyLabelSetWithContent">: gql "EdgeKeyLabelSetWithContent",
    "impliedContent">: gql "EdgeTypeImpliedContent"]

edgeTypeImpliedContent :: Binding
edgeTypeImpliedContent = def "EdgeTypeImpliedContent" $
  T.union [
    "labelSet">: gql "EdgeTypeLabelSet",
    "propertyTypes">: gql "EdgeTypePropertyTypes",
    "labelSetWithProperties">: gql "EdgeLabelSetWithProperties"]

edgeTypeKeyLabelSet :: Binding
edgeTypeKeyLabelSet = def "EdgeTypeKeyLabelSet" $
  T.maybe $ gql "LabelSetPhrase"

edgeTypeLabelSet :: Binding
edgeTypeLabelSet = def "EdgeTypeLabelSet" $
  gql "LabelSetPhrase"

edgeTypeName :: Binding
edgeTypeName = def "EdgeTypeName" $
  T.string

edgeTypeNameWithFiller :: Binding
edgeTypeNameWithFiller = def "EdgeTypeNameWithFiller" $
  T.record [
    "typeName">: gql "EdgeTypeName",
    "filler">: T.maybe $ gql "EdgeTypeFiller"]

edgeTypePattern :: Binding
edgeTypePattern = def "EdgeTypePattern" $
  T.record [
    "kindAndSynonym">: T.maybe $ gql "EdgeKindAndSynonym",
    "patternType">: gql "EdgeTypePatternType"]

edgeTypePatternDirected :: Binding
edgeTypePatternDirected = def "EdgeTypePatternDirected" $
  T.union [
    "pointingRight">: gql "EdgeTypePatternPointingRight",
    "pointingLeft">: gql "EdgeTypePatternPointingLeft"]

edgeTypePatternPointingLeft :: Binding
edgeTypePatternPointingLeft = def "EdgeTypePatternPointingLeft" $
  T.record [
    "destination">: gql "DestinationNodeTypeReference",
    "arc">: gql "ArcTypePointingLeft",
    "source">: gql "SourceNodeTypeReference"]

edgeTypePatternPointingRight :: Binding
edgeTypePatternPointingRight = def "EdgeTypePatternPointingRight" $
  T.record [
    "source">: gql "SourceNodeTypeReference",
    "arc">: gql "ArcTypePointingRight",
    "destination">: gql "DestinationNodeTypeReference"]

edgeTypePatternType :: Binding
edgeTypePatternType = def "EdgeTypePatternType" $
  T.union [
    "directed">: gql "EdgeTypePatternDirected",
    "undirected">: gql "EdgeTypePatternUndirected"]

edgeTypePatternUndirected :: Binding
edgeTypePatternUndirected = def "EdgeTypePatternUndirected" $
  T.record [
    "source">: gql "SourceNodeTypeReference",
    "arc">: gql "ArcTypeUndirected",
    "destination">: gql "DestinationNodeTypeReference"]

edgeTypePhrase :: Binding
edgeTypePhrase = def "EdgeTypePhrase" $
  T.record [
    "kind">: gql "EdgeKind",
    "synonym">: gql "EdgeSynonym",
    "typeNameAndFiller">: gql "EdgeTypePhraseFiller",
    "endpointPair">: gql "EndpointPairPhrase"]

edgeTypePhraseFiller :: Binding
edgeTypePhraseFiller = def "EdgeTypePhraseFiller" $
  T.union [
    "typeNameWithFiller">: gql "EdgeTypeNameWithFiller",
    "fillerOnly">: gql "EdgeTypeFiller"]

edgeTypePropertyTypes :: Binding
edgeTypePropertyTypes = def "EdgeTypePropertyTypes" $
  gql "PropertyTypesSpecification"

edgeTypeSpecification :: Binding
edgeTypeSpecification = def "EdgeTypeSpecification" $
  T.union [
    "pattern">: gql "EdgeTypePattern",
    "phrase">: gql "EdgeTypePhrase"]

edgesSynonym :: Binding
edgesSynonym = def "EdgesSynonym" $
  T.enum ["edges", "relationships"]

elementBindingsOrElements :: Binding
elementBindingsOrElements = def "ElementBindingsOrElements" $
  T.union [
    "elementBindings">: T.boolean,
    "elements">: T.unit]

elementIdFunction :: Binding
elementIdFunction = def "ElementIdFunction" $
  gql "ElementVariableReference"

elementPattern :: Binding
elementPattern = def "ElementPattern" $
  T.union [
    "node">: gql "NodePattern",
    "edge">: gql "EdgePattern"]

elementPatternFiller :: Binding
elementPatternFiller = def "ElementPatternFiller" $
  T.record [
    "variableDeclaration">: T.maybe $ gql "ElementVariableDeclaration",
    "isLabelExpression">: T.maybe $ gql "IsLabelExpression",
    "predicate">: T.maybe $ gql "ElementPatternPredicate"]

elementPatternPredicate :: Binding
elementPatternPredicate = def "ElementPatternPredicate" $
  T.union [
    "whereClause">: gql "ElementPatternWhereClause",
    "propertySpecification">: gql "ElementPropertySpecification"]

elementPatternWhereClause :: Binding
elementPatternWhereClause = def "ElementPatternWhereClause" $
  gql "SearchCondition"

elementPropertySpecification :: Binding
elementPropertySpecification = def "ElementPropertySpecification" $
  gql "PropertyKeyValuePairList"

elementTypeList :: Binding
elementTypeList = def "ElementTypeList" $
  nonemptyList $ gql "ElementTypeSpecification"

elementTypeSpecification :: Binding
elementTypeSpecification = def "ElementTypeSpecification" $
  T.union [
    "nodeType">: gql "NodeTypeSpecification",
    "edgeType">: gql "EdgeTypeSpecification"]

elementVariable :: Binding
elementVariable = def "ElementVariable" $
  gql "BindingVariable"

elementVariableDeclaration :: Binding
elementVariableDeclaration = def "ElementVariableDeclaration" $
  T.record [
    "temp">: T.maybe T.boolean,
    "variable">: gql "ElementVariable"]

elementVariableReference :: Binding
elementVariableReference = def "ElementVariableReference" $
  gql "BindingVariableReference"

elementsFunction :: Binding
elementsFunction = def "ElementsFunction" $
  gql "PathValueExpression"

elseClause :: Binding
elseClause = def "ElseClause" $
  gql "Result"

emptyType :: Binding
emptyType = def "EmptyType" $
  T.unit

endTransactionCommand :: Binding
endTransactionCommand = def "EndTransactionCommand" $
  T.union [
    "rollback">: gql "RollbackCommand",
    "commit">: gql "CommitCommand"]

endpointPair :: Binding
endpointPair = def "EndpointPair" $
  T.union [
    "directedPair">: gql "EndpointPairDirected",
    "undirectedPair">: gql "EndpointPairUndirected"]

endpointPairDirected :: Binding
endpointPairDirected = def "EndpointPairDirected" $
  T.union [
    "pointingRight">: gql "EndpointPairPointingRight",
    "pointingLeft">: gql "EndpointPairPointingLeft"]

endpointPairPhrase :: Binding
endpointPairPhrase = def "EndpointPairPhrase" $
  gql "EndpointPair"

endpointPairPointingLeft :: Binding
endpointPairPointingLeft = def "EndpointPairPointingLeft" $
  T.record [
    "destinationAlias">: gql "DestinationNodeTypeAlias",
    "sourceAlias">: gql "SourceNodeTypeAlias"]

endpointPairPointingRight :: Binding
endpointPairPointingRight = def "EndpointPairPointingRight" $
  T.record [
    "sourceAlias">: gql "SourceNodeTypeAlias",
    "connector">: gql "ConnectorPointingRight",
    "destinationAlias">: gql "DestinationNodeTypeAlias"]

endpointPairUndirected :: Binding
endpointPairUndirected = def "EndpointPairUndirected" $
  T.record [
    "sourceAlias">: gql "SourceNodeTypeAlias",
    "connector">: gql "ConnectorUndirected",
    "destinationAlias">: gql "DestinationNodeTypeAlias"]

exactNumericLiteral :: Binding
exactNumericLiteral = def "ExactNumericLiteral" $
  T.union [
    "scientificWithSuffix">: T.string,
    "commonWithSuffix">: T.string,
    "commonWithoutSuffix">: T.string,
    "integerWithSuffix">: T.string,
    "unsignedInteger">: gql "UnsignedInteger"]

exactNumericType :: Binding
exactNumericType = def "ExactNumericType" $
  T.union [
    "binary">: gql "BinaryExactNumericType",
    "decimal">: gql "DecimalExactNumericType"]

existsPredicate :: Binding
existsPredicate = def "ExistsPredicate" $
  T.union [
    "graphPatternBrace">: gql "GraphPattern",
    "graphPatternParen">: gql "GraphPattern",
    "matchBlockBrace">: gql "MatchStatementBlock",
    "matchBlockParen">: gql "MatchStatementBlock",
    "nestedQuery">: gql "NestedQuerySpecification"]

exponentialFunction :: Binding
exponentialFunction = def "ExponentialFunction" $
  gql "NumericValueExpression"

field :: Binding
field = def "Field" $
  T.record [
    "name">: gql "FieldName",
    "value">: gql "ValueExpression"]

fieldList :: Binding
fieldList = def "FieldList" $
  nonemptyList $ gql "Field"

fieldName_ :: Binding
fieldName_ = def "FieldName" $
  T.string

fieldType :: Binding
fieldType = def "FieldType" $
  T.record [
    "fieldName">: gql "FieldName",
    "typed">: T.maybe $ gql "Typed",
    "valueType">: gql "ValueType"]

fieldTypeList :: Binding
fieldTypeList = def "FieldTypeList" $
  nonemptyList $ gql "FieldType"

fieldTypesSpecification :: Binding
fieldTypesSpecification = def "FieldTypesSpecification" $
  T.maybe $ gql "FieldTypeList"

fieldsSpecification :: Binding
fieldsSpecification = def "FieldsSpecification" $
  T.maybe $ gql "FieldList"

filterStatement :: Binding
filterStatement = def "FilterStatement" $
  T.union [
    "whereClause">: gql "WhereClause",
    "searchCondition">: gql "SearchCondition"]

fixedLength :: Binding
fixedLength = def "FixedLength" $
  gql "UnsignedInteger"

fixedQuantifier :: Binding
fixedQuantifier = def "FixedQuantifier" $
  gql "UnsignedInteger"

float128Type :: Binding
float128Type = def "Float128Type" $
  T.record [
    "notNull">: T.boolean]

float16Type :: Binding
float16Type = def "Float16Type" $
  T.record [
    "notNull">: T.boolean]

float256Type :: Binding
float256Type = def "Float256Type" $
  T.record [
    "notNull">: T.boolean]

float32Type :: Binding
float32Type = def "Float32Type" $
  T.record [
    "notNull">: T.boolean]

float64Type :: Binding
float64Type = def "Float64Type" $
  T.record [
    "notNull">: T.boolean]

floatTypeWithPrecision :: Binding
floatTypeWithPrecision = def "FloatTypeWithPrecision" $
  T.record [
    "precisionAndScale">: T.maybe $ gql "PrecisionAndScale",
    "notNull">: T.boolean]

floorFunction :: Binding
floorFunction = def "FloorFunction" $
  gql "NumericValueExpression"

focusedLinearDataModifyingStatement :: Binding
focusedLinearDataModifyingStatement = def "FocusedLinearDataModifyingStatement" $
  T.union [
    "simple">: gql "FocusedLinearDataModifyingStatementBody",
    "nested">: gql "FocusedNestedDataModifyingProcedureSpecification"]

focusedLinearDataModifyingStatementBody :: Binding
focusedLinearDataModifyingStatementBody = def "FocusedLinearDataModifyingStatementBody" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "simpleAccess">: gql "SimpleLinearDataAccessingStatement",
    "primitiveResult">: T.maybe $ gql "PrimitiveResultStatement"]

focusedLinearQueryAndPrimitiveResultStatementPart :: Binding
focusedLinearQueryAndPrimitiveResultStatementPart = def "FocusedLinearQueryAndPrimitiveResultStatementPart" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "simple">: gql "SimpleLinearQueryStatement",
    "primitiveResult">: gql "PrimitiveResultStatement"]

focusedLinearQueryStatement :: Binding
focusedLinearQueryStatement = def "FocusedLinearQueryStatement" $
  T.union [
    "parts">: gql "FocusedLinearQueryStatementPartsAndResult",
    "primitive">: gql "FocusedPrimitiveResultStatement",
    "nested">: gql "FocusedNestedQuerySpecification",
    "select">: gql "SelectStatement"]

focusedLinearQueryStatementPart :: Binding
focusedLinearQueryStatementPart = def "FocusedLinearQueryStatementPart" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "simple">: gql "SimpleLinearQueryStatement"]

focusedLinearQueryStatementPartsAndResult :: Binding
focusedLinearQueryStatementPartsAndResult = def "FocusedLinearQueryStatementPartsAndResult" $
  T.record [
    "parts">: T.list $ gql "FocusedLinearQueryStatementPart",
    "result">: gql "FocusedLinearQueryAndPrimitiveResultStatementPart"]

focusedNestedDataModifyingProcedureSpecification :: Binding
focusedNestedDataModifyingProcedureSpecification = def "FocusedNestedDataModifyingProcedureSpecification" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "nestedSpec">: gql "NestedDataModifyingProcedureSpecification"]

focusedNestedQuerySpecification :: Binding
focusedNestedQuerySpecification = def "FocusedNestedQuerySpecification" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "nested">: gql "NestedQuerySpecification"]

focusedPrimitiveResultStatement :: Binding
focusedPrimitiveResultStatement = def "FocusedPrimitiveResultStatement" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "primitiveResult">: gql "PrimitiveResultStatement"]

foldCharacterString :: Binding
foldCharacterString = def "FoldCharacterString" $
  T.record [
    "case">: gql "Case",
    "valueExpression">: gql "ValueExpression"]

forItem :: Binding
forItem = def "ForItem" $
  T.record [
    "alias">: gql "ForItemAlias",
    "source">: gql "ForItemSource"]

forItemAlias :: Binding
forItemAlias = def "ForItemAlias" $
  gql "BindingVariable"

forItemSource :: Binding
forItemSource = def "ForItemSource" $
  gql "ValueExpression"

forOrdinalityOrOffset :: Binding
forOrdinalityOrOffset = def "ForOrdinalityOrOffset" $
  T.record [
    "type">: gql "OrdinalityOrOffsetType",
    "variable">: gql "BindingVariable"]

forStatement :: Binding
forStatement = def "ForStatement" $
  T.record [
    "item">: gql "ForItem",
    "ordinalityOrOffset">: T.maybe $ gql "ForOrdinalityOrOffset"]

fullEdgeAnyDirection :: Binding
fullEdgeAnyDirection = def "FullEdgeAnyDirection" $
  gql "ElementPatternFiller"

fullEdgeLeftOrRight :: Binding
fullEdgeLeftOrRight = def "FullEdgeLeftOrRight" $
  gql "ElementPatternFiller"

fullEdgeLeftOrUndirected :: Binding
fullEdgeLeftOrUndirected = def "FullEdgeLeftOrUndirected" $
  gql "ElementPatternFiller"

fullEdgePattern :: Binding
fullEdgePattern = def "FullEdgePattern" $
  T.union [
    "pointingLeft">: gql "FullEdgePointingLeft",
    "undirected">: gql "FullEdgeUndirected",
    "pointingRight">: gql "FullEdgePointingRight",
    "leftOrUndirected">: gql "FullEdgeLeftOrUndirected",
    "undirectedOrRight">: gql "FullEdgeUndirectedOrRight",
    "leftOrRight">: gql "FullEdgeLeftOrRight",
    "anyDirection">: gql "FullEdgeAnyDirection"]

fullEdgePointingLeft :: Binding
fullEdgePointingLeft = def "FullEdgePointingLeft" $
  gql "ElementPatternFiller"

fullEdgePointingRight :: Binding
fullEdgePointingRight = def "FullEdgePointingRight" $
  gql "ElementPatternFiller"

fullEdgeUndirected :: Binding
fullEdgeUndirected = def "FullEdgeUndirected" $
  gql "ElementPatternFiller"

fullEdgeUndirectedOrRight :: Binding
fullEdgeUndirectedOrRight = def "FullEdgeUndirectedOrRight" $
  gql "ElementPatternFiller"

generalLiteral :: Binding
generalLiteral = def "GeneralLiteral" $
  T.union [
    "boolean">: gql "BooleanLiteral",
    "characterString">: gql "CharacterStringLiteral",
    "byteString">: gql "ByteStringLiteral",
    "temporal">: gql "TemporalLiteral",
    "duration">: gql "DurationLiteral",
    "nullLiteral">: gql "NullLiteral",
    "list">: gql "ListLiteral",
    "record">: gql "RecordLiteral"]

generalLogarithmArgument :: Binding
generalLogarithmArgument = def "GeneralLogarithmArgument" $
  gql "NumericValueExpression"

generalLogarithmBase :: Binding
generalLogarithmBase = def "GeneralLogarithmBase" $
  gql "NumericValueExpression"

generalLogarithmFunction :: Binding
generalLogarithmFunction = def "GeneralLogarithmFunction" $
  T.record [
    "base">: gql "GeneralLogarithmBase",
    "argument">: gql "GeneralLogarithmArgument"]

generalQuantifier :: Binding
generalQuantifier = def "GeneralQuantifier" $
  T.record [
    "lowerBound">: T.maybe $ gql "LowerBound",
    "upperBound">: T.maybe $ gql "UpperBound"]

generalSetFunction :: Binding
generalSetFunction = def "GeneralSetFunction" $
  T.record [
    "functionType">: gql "GeneralSetFunctionType",
    "setQuantifier">: T.maybe $ gql "SetQuantifier",
    "valueExpression">: gql "ValueExpression"]

generalSetFunctionType :: Binding
generalSetFunctionType = def "GeneralSetFunctionType" $
  T.enum [
    "avg",
    "count",
    "max",
    "min",
    "sum",
    "collectList",
    "stddevSamp",
    "stddevPop"]

generalValueSpecification :: Binding
generalValueSpecification = def "GeneralValueSpecification" $
  T.union [
    "dynamicParameterSpecification">: gql "DynamicParameterSpecification",
    "sessionUser">: T.unit]

gql :: String -> Type
gql = typeref ns


gqlProgram :: Binding
gqlProgram = def "GqlProgram" $
  T.record [
    "activity">: T.maybe $ gql "ProgramActivity",
    "close">: T.maybe $ gql "SessionCloseCommand"]

graphAndNestedQuerySpecification :: Binding
graphAndNestedQuerySpecification = def "GraphAndNestedQuerySpecification" $
  T.record [
    "graphExpression">: gql "GraphExpression",
    "nested">: gql "NestedQuerySpecification"]

graphExpression :: Binding
graphExpression = def "GraphExpression" $
  T.union [
    "object">: gql "ObjectExpressionPrimary",
    "reference">: gql "GraphReference",
    "name">: gql "ObjectNameOrBindingVariable",
    "current">: gql "CurrentGraph"]

graphInitializer :: Binding
graphInitializer = def "GraphInitializer" T.unit

graphName :: Binding
graphName = def "GraphName" $
  T.string

graphPattern :: Binding
graphPattern = def "GraphPattern" $
  T.record [
    "matchMode">: T.maybe $ gql "MatchMode",
    "pathPatterns">: gql "PathPatternList",
    "keepClause">: T.maybe $ gql "KeepClause",
    "whereClause">: T.maybe $ gql "GraphPatternWhereClause"]

graphPatternBindingTable :: Binding
graphPatternBindingTable = def "GraphPatternBindingTable" $
  T.record [
    "pattern">: gql "GraphPattern",
    "yieldClause">: T.maybe $ gql "GraphPatternYieldClause"]

graphPatternQuantifier :: Binding
graphPatternQuantifier = def "GraphPatternQuantifier" $
  T.union [
    "asterisk">: T.unit,
    "plusSign">: T.unit,
    "fixed">: gql "FixedQuantifier",
    "general">: gql "GeneralQuantifier"]

graphPatternWhereClause :: Binding
graphPatternWhereClause = def "GraphPatternWhereClause" $
  gql "SearchCondition"

graphPatternYieldClause :: Binding
graphPatternYieldClause = def "GraphPatternYieldClause" $
  gql "GraphPatternYieldItemList"

graphPatternYieldItem :: Binding
graphPatternYieldItem = def "GraphPatternYieldItem" $
  gql "BindingVariableReference"

graphPatternYieldItemList :: Binding
graphPatternYieldItemList = def "GraphPatternYieldItemList" $
  T.union [
    "items">: nonemptyList $ gql "GraphPatternYieldItem",
    "noBindings">: T.unit]

graphReference :: Binding
graphReference = def "GraphReference" $
  T.union [
    "parentAndGraphName">: gql "ParentAndGraphName",
    "delimitedGraphName">: gql "DelimitedGraphName",
    "homeGraph">: gql "HomeGraph",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

graphReferenceValueType :: Binding
graphReferenceValueType = def "GraphReferenceValueType" $
  T.union [
    "open">: gql "OpenGraphReferenceValueType",
    "closed">: gql "ClosedGraphReferenceValueType"]

graphSource :: Binding
graphSource = def "GraphSource" $
  gql "GraphExpression"

graphTypeLikeGraph :: Binding
graphTypeLikeGraph = def "GraphTypeLikeGraph" $
  gql "GraphExpression"

graphTypeName :: Binding
graphTypeName = def "GraphTypeName" $
  T.string

graphTypeOption :: Binding
graphTypeOption = def "GraphTypeOption" $
  T.union [
    "openGraphType">: gql "OpenGraphType",
    "ofGraphType">: gql "OfGraphType"]

graphTypeReference :: Binding
graphTypeReference = def "GraphTypeReference" $
  T.union [
    "parentAndTypeName">: gql "CatalogGraphTypeParentAndName",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

graphTypeSource :: Binding
graphTypeSource = def "GraphTypeSource" $
  T.union [
    "copyOf">: gql "CopyOfGraphType",
    "likeGraph">: gql "GraphTypeLikeGraph",
    "nestedSpecification">: gql "NestedGraphTypeSpecification"]

graphTypeSpecificationBody :: Binding
graphTypeSpecificationBody = def "GraphTypeSpecificationBody" $
  gql "ElementTypeList"

graphVariableDefinition :: Binding
graphVariableDefinition = def "GraphVariableDefinition" $
  T.record [
    "variable">: gql "BindingVariable",
    "initializer">: gql "OptTypedGraphInitializer"]

groupByClause :: Binding
groupByClause = def "GroupByClause" $
  gql "GroupingElementList"

groupingElement :: Binding
groupingElement = def "GroupingElement" $
  gql "BindingVariableReference"

groupingElementList :: Binding
groupingElementList = def "GroupingElementList" $
  T.union [
    "elements">: nonemptyList $ gql "GroupingElement",
    "emptySet">: T.unit]

havingClause :: Binding
havingClause = def "HavingClause" $
  gql "SearchCondition"

homeGraph :: Binding
homeGraph = def "HomeGraph" $
  T.enum ["homePropertyGraph", "homeGraph"]

identifier :: Binding
identifier = def "Identifier" $
  T.string

immaterialValueType :: Binding
immaterialValueType = def "ImmaterialValueType" $
  T.union [
    "nullType">: gql "NullType",
    "emptyType">: gql "EmptyType"]

implies :: Binding
implies = def "Implies" $
  T.enum ["rightDoubleArrow", "implies"]

independentValueExpression :: Binding
independentValueExpression = def "IndependentValueExpression" $
  gql "NumericValueExpression"

inlineProcedureCall :: Binding
inlineProcedureCall = def "InlineProcedureCall" $
  T.record [
    "scope">: T.maybe $ gql "VariableScopeClause",
    "nested">: gql "NestedProcedureSpecification"]

insertEdgeAndNode :: Binding
insertEdgeAndNode = def "InsertEdgeAndNode" $
  T.record [
    "edge">: gql "InsertEdgePattern",
    "node">: gql "InsertNodePattern"]

insertEdgePattern :: Binding
insertEdgePattern = def "InsertEdgePattern" $
  T.union [
    "pointingLeft">: gql "InsertEdgePointingLeft",
    "pointingRight">: gql "InsertEdgePointingRight",
    "undirected">: gql "InsertEdgeUndirected"]

insertEdgePointingLeft :: Binding
insertEdgePointingLeft = def "InsertEdgePointingLeft" $
  T.maybe $ gql "InsertElementPatternFiller"

insertEdgePointingRight :: Binding
insertEdgePointingRight = def "InsertEdgePointingRight" $
  T.maybe $ gql "InsertElementPatternFiller"

insertEdgeUndirected :: Binding
insertEdgeUndirected = def "InsertEdgeUndirected" $
  T.maybe $ gql "InsertElementPatternFiller"

insertElementPatternFiller :: Binding
insertElementPatternFiller = def "InsertElementPatternFiller" $
  T.record [
    "variableDeclaration">: T.maybe $ gql "ElementVariableDeclaration",
    "labelAndProperties">: T.maybe $ gql "LabelAndPropertySetSpecification"]

insertGraphPattern :: Binding
insertGraphPattern = def "InsertGraphPattern" $
  gql "InsertPathPatternList"

insertNodePattern :: Binding
insertNodePattern = def "InsertNodePattern" $
  T.maybe $ gql "InsertElementPatternFiller"

insertPathPattern :: Binding
insertPathPattern = def "InsertPathPattern" $
  T.record [
    "startNode">: gql "InsertNodePattern",
    "edgesAndNodes">: T.list $ gql "InsertEdgeAndNode"]

insertPathPatternList :: Binding
insertPathPatternList = def "InsertPathPatternList" $
  nonemptyList $ gql "InsertPathPattern"

insertStatement :: Binding
insertStatement = def "InsertStatement" $
  gql "InsertGraphPattern"

int128Type :: Binding
int128Type = def "Int128Type" $
  T.record ["notNull">: T.boolean]

int16Type :: Binding
int16Type = def "Int16Type" $
  T.record ["notNull">: T.boolean]

int256Type :: Binding
int256Type = def "Int256Type" $
  T.record ["notNull">: T.boolean]

int32Type :: Binding
int32Type = def "Int32Type" $
  T.record ["notNull">: T.boolean]

int64Type :: Binding
int64Type = def "Int64Type" $
  T.record ["notNull">: T.boolean]

int8Type :: Binding
int8Type = def "Int8Type" $
  T.record ["notNull">: T.boolean]

intWithPrecision :: Binding
intWithPrecision = def "IntWithPrecision" $
  T.record [
    "precision">: T.maybe $ gql "Precision",
    "notNull">: T.boolean]

integer128Type :: Binding
integer128Type = def "Integer128Type" $
  T.record ["notNull">: T.boolean]

integer16Type :: Binding
integer16Type = def "Integer16Type" $
  T.record ["notNull">: T.boolean]

integer256Type :: Binding
integer256Type = def "Integer256Type" $
  T.record ["notNull">: T.boolean]

integer32Type :: Binding
integer32Type = def "Integer32Type" $
  T.record ["notNull">: T.boolean]

integer64Type :: Binding
integer64Type = def "Integer64Type" $
  T.record ["notNull">: T.boolean]

integer8Type :: Binding
integer8Type = def "Integer8Type" $
  T.record ["notNull">: T.boolean]

integerWithPrecision :: Binding
integerWithPrecision = def "IntegerWithPrecision" $
  T.record [
    "precision">: T.maybe $ gql "Precision",
    "notNull">: T.boolean]

isLabelExpression :: Binding
isLabelExpression = def "IsLabelExpression" $
  T.record [
    "isOrColon">: gql "IsOrColon",
    "label">: gql "LabelExpression"]

isLabeledOrColon :: Binding
isLabeledOrColon = def "IsLabeledOrColon" $
  T.union [
    "not">: T.boolean,
    "colon">: T.unit]

isNotExpr :: Binding
isNotExpr = def "IsNotExpr" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "not">: T.boolean,
    "truthValue">: gql "TruthValue"]

isOrColon :: Binding
isOrColon = def "IsOrColon" $
  T.enum ["is", "colon"]

isOrColonWithLabels :: Binding
isOrColonWithLabels = def "IsOrColonWithLabels" $
  T.record [
    "isOrColon">: gql "IsOrColon",
    "labels">: gql "LabelSetSpecification"]

keepClause :: Binding
keepClause = def "KeepClause" $
  gql "PathPatternPrefix"

labelAndPropertySetSpecification :: Binding
labelAndPropertySetSpecification = def "LabelAndPropertySetSpecification" $
  T.record [
    "isOrColon">: T.maybe $ gql "IsOrColon",
    "labelSet">: T.maybe $ gql "LabelSetSpecification",
    "propertySpecification">: T.maybe $ gql "ElementPropertySpecification"]

labelExpression :: Binding
labelExpression = def "LabelExpression" $
  T.union [
    "negation">: gql "LabelExpression",
    "conjunction">: gql "ConjunctionLabelExpression",
    "disjunction">: gql "DisjunctionLabelExpression",
    "name">: gql "LabelName",
    "wildcard">: T.unit,
    "parenthesized">: gql "LabelExpression"]

labelName :: Binding
labelName = def "LabelName" $
  T.string

labelSetPhrase :: Binding
labelSetPhrase = def "LabelSetPhrase" $
  T.union [
    "singleLabel">: gql "LabelName",
    "multipleLabels">: gql "LabelSetSpecification",
    "isOrColonWithLabels">: gql "IsOrColonWithLabels"]

labelSetSpecification :: Binding
labelSetSpecification = def "LabelSetSpecification" $
  nonemptyList $ gql "LabelName"

labeledPredicate :: Binding
labeledPredicate = def "LabeledPredicate" $
  T.record [
    "elementVariableReference">: gql "ElementVariableReference",
    "labeledPart">: gql "LabeledPredicatePart2"]

labeledPredicatePart2 :: Binding
labeledPredicatePart2 = def "LabeledPredicatePart2" $
  T.record [
    "isLabeledOrColon">: gql "IsLabeledOrColon",
    "labelExpression">: gql "LabelExpression"]

lengthExpression :: Binding
lengthExpression = def "LengthExpression" $
  T.union [
    "char">: gql "CharLengthExpression",
    "byte">: gql "ByteLengthExpression",
    "path">: gql "PathLengthExpression"]

letStatement :: Binding
letStatement = def "LetStatement" $
  gql "LetVariableDefinitionList"

letValueExpression :: Binding
letValueExpression = def "LetValueExpression" $
  T.record [
    "letVariables">: gql "LetVariableDefinitionList",
    "valueExpression">: gql "ValueExpression"]

letVariableDefinition :: Binding
letVariableDefinition = def "LetVariableDefinition" $
  T.union [
    "valueVariable">: gql "ValueVariableDefinition",
    "bindingEqualsValue">: gql "BindingEqualsValue"]

letVariableDefinitionList :: Binding
letVariableDefinitionList = def "LetVariableDefinitionList" $
  nonemptyList $ gql "LetVariableDefinition"

limitClause :: Binding
limitClause = def "LimitClause" $
  gql "NonNegativeIntegerSpecification"

linearCatalogModifyingStatement :: Binding
linearCatalogModifyingStatement = def "LinearCatalogModifyingStatement" $
  nonemptyList $ gql "SimpleCatalogModifyingStatement"

linearDataModifyingStatement :: Binding
linearDataModifyingStatement = def "LinearDataModifyingStatement" $
  T.union [
    "focused">: gql "FocusedLinearDataModifyingStatement",
    "ambient">: gql "AmbientLinearDataModifyingStatement"]

linearQueryStatement :: Binding
linearQueryStatement = def "LinearQueryStatement" $
  T.union [
    "focused">: gql "FocusedLinearQueryStatement",
    "ambient">: gql "AmbientLinearQueryStatement"]

listElement :: Binding
listElement = def "ListElement" $
  gql "ValueExpression"

listElementList :: Binding
listElementList = def "ListElementList" $
  nonemptyList $ gql "ListElement"

listLiteral :: Binding
listLiteral = def "ListLiteral" $
  gql "ListValueConstructorByEnumeration"

listValueConstructor :: Binding
listValueConstructor = def "ListValueConstructor" $
  gql "ListValueConstructorByEnumeration"

listValueConstructorByEnumeration :: Binding
listValueConstructorByEnumeration = def "ListValueConstructorByEnumeration" $
  T.record [
    "listValueTypeName">: T.maybe $ gql "ListValueTypeName",
    "elements">: T.maybe $ gql "ListElementList"]

listValueExpression :: Binding
listValueExpression = def "ListValueExpression" $
  gql "ValueExpression"

listValueFunction :: Binding
listValueFunction = def "ListValueFunction" $
  T.union [
    "trim">: gql "TrimListFunction",
    "elements">: gql "ElementsFunction"]

listValueTypeAlt1 :: Binding
listValueTypeAlt1 = def "ListValueTypeAlt1" $
  T.record [
    "typeName">: gql "ListValueTypeName",
    "valueType">: gql "ValueType",
    "maxLength">: T.maybe $ gql "MaxLength",
    "notNull">: T.boolean]

listValueTypeAlt2 :: Binding
listValueTypeAlt2 = def "ListValueTypeAlt2" $
  T.record [
    "valueType">: gql "ValueType",
    "typeName">: gql "ListValueTypeName",
    "maxLength">: T.maybe $ gql "MaxLength",
    "notNull">: T.boolean]

listValueTypeAlt3 :: Binding
listValueTypeAlt3 = def "ListValueTypeAlt3" $
  T.record [
    "typeName">: gql "ListValueTypeName",
    "maxLength">: T.maybe $ gql "MaxLength",
    "notNull">: T.boolean]

listValueTypeName :: Binding
listValueTypeName = def "ListValueTypeName" $
  T.record [
    "group">: T.boolean,
    "synonym">: gql "ListValueTypeNameSynonym"]

listValueTypeNameSynonym :: Binding
listValueTypeNameSynonym = def "ListValueTypeNameSynonym" $
  T.enum ["list", "array"]

localDatetimeType :: Binding
localDatetimeType = def "LocalDatetimeType" $
  T.record [
    "notNull">: T.boolean]

localDatetimeTypeChoice :: Binding
localDatetimeTypeChoice = def "LocalDatetimeTypeChoice" $
  T.union [
    "localDatetime">: gql "LocalDatetimeType",
    "timestampWithoutTimeZone">: gql "TimestampWithoutTimeZoneType"]

localNodeTypeAlias :: Binding
localNodeTypeAlias = def "LocalNodeTypeAlias" $
  T.string

localTimeType :: Binding
localTimeType = def "LocalTimeType" $
  T.record [
    "notNull">: T.boolean]

localTimeTypeChoice :: Binding
localTimeTypeChoice = def "LocalTimeTypeChoice" $
  T.union [
    "localTime">: gql "LocalTimeType",
    "timeWithoutTimeZone">: gql "TimeWithoutTimeZoneType"]

localdatetimeFunction :: Binding
localdatetimeFunction = def "LocaldatetimeFunction" $
  T.union [
    "localTimestamp">: T.unit,
    "localDatetimeWithParams">: T.maybe $ gql "DatetimeFunctionParameters"]

localtimeFunction :: Binding
localtimeFunction = def "LocaltimeFunction" $
  T.maybe $ gql "TimeFunctionParameters"

lowerBound :: Binding
lowerBound = def "LowerBound" $
  gql "UnsignedInteger"

matchMode :: Binding
matchMode = def "MatchMode" $
  T.union [
    "repeatableElements">: gql "RepeatableElementsMatchMode",
    "differentEdges">: gql "DifferentEdgesMatchMode"]

matchStatement :: Binding
matchStatement = def "MatchStatement" $
  T.union [
    "simple">: gql "SimpleMatchStatement",
    "optional">: gql "OptionalMatchStatement"]

matchStatementBlock :: Binding
matchStatementBlock = def "MatchStatementBlock" $
  nonemptyList $ gql "MatchStatement"

maxLength :: Binding
maxLength = def "MaxLength" $
  gql "UnsignedInteger"

minLength :: Binding
minLength = def "MinLength" $
  gql "UnsignedInteger"

modulusExpression :: Binding
modulusExpression = def "ModulusExpression" $
  T.record [
    "dividend">: gql "NumericValueExpressionDividend",
    "divisor">: gql "NumericValueExpressionDivisor"]

mulDivNumericValueExpression :: Binding
mulDivNumericValueExpression = def "MulDivNumericValueExpression" $
  T.record [
    "left">: gql "NumericValueExpression",
    "operator">: gql "MultDivOperator",
    "right">: gql "NumericValueExpression"]

multDivExpr :: Binding
multDivExpr = def "MultDivExpr" $
  T.record [
    "left">: gql "ValueExpression",
    "operator">: gql "MultDivOperator",
    "right">: gql "ValueExpression"]

multDivOperator :: Binding
multDivOperator = def "MultDivOperator" $
  T.enum ["multiply", "divide"]

namedProcedureCall :: Binding
namedProcedureCall = def "NamedProcedureCall" $
  T.record [
    "reference">: gql "ProcedureReference",
    "arguments">: T.maybe $ gql "ProcedureArgumentList",
    "yield">: T.maybe $ gql "YieldClause"]

naturalLogarithm :: Binding
naturalLogarithm = def "NaturalLogarithm" $
  gql "NumericValueExpression"

nestedBindingTableQuerySpecification :: Binding
nestedBindingTableQuerySpecification = def "NestedBindingTableQuerySpecification" T.unit

nestedDataModifyingProcedureSpecification :: Binding
nestedDataModifyingProcedureSpecification = def "NestedDataModifyingProcedureSpecification" $
  gql "ProcedureBody"

nestedGraphTypeSpecification :: Binding
nestedGraphTypeSpecification = def "NestedGraphTypeSpecification" $
  gql "GraphTypeSpecificationBody"

nestedProcedureSpecification :: Binding
nestedProcedureSpecification = def "NestedProcedureSpecification" $
  gql "ProcedureSpecification"

nestedQuerySpecification :: Binding
nestedQuerySpecification = def "NestedQuerySpecification" $
  gql "ProcedureBody"

nextStatement :: Binding
nextStatement = def "NextStatement" $
  T.record [
    "yieldClause">: T.maybe $ gql "YieldClause",
    "statement">: gql "Statement"]

nodeKeyLabelSetWithContent :: Binding
nodeKeyLabelSetWithContent = def "NodeKeyLabelSetWithContent" $
  T.record [
    "keyLabelSet">: gql "NodeTypeKeyLabelSet",
    "impliedContent">: T.maybe $ gql "NodeTypeImpliedContent"]

nodeLabelSetWithProperties :: Binding
nodeLabelSetWithProperties = def "NodeLabelSetWithProperties" $
  T.record [
    "labelSet">: gql "NodeTypeLabelSet",
    "propertyTypes">: gql "NodeTypePropertyTypes"]

nodePattern :: Binding
nodePattern = def "NodePattern" $
  gql "ElementPatternFiller"

nodeReference :: Binding
nodeReference = def "NodeReference" $
  gql "ElementVariableReference"

nodeReferenceValueExpression :: Binding
nodeReferenceValueExpression = def "NodeReferenceValueExpression" $
  gql "PrimaryValueExpression"

nodeReferenceValueType :: Binding
nodeReferenceValueType = def "NodeReferenceValueType" $
  T.union [
    "open">: gql "OpenNodeReferenceValueType",
    "closed">: gql "ClosedNodeReferenceValueType"]

nodeSynonym :: Binding
nodeSynonym = def "NodeSynonym" $
  T.enum ["node", "vertex"]

nodeSynonymAndTypeName :: Binding
nodeSynonymAndTypeName = def "NodeSynonymAndTypeName" $
  T.record [
    "nodeSynonym">: gql "NodeSynonym",
    "typeName">: T.maybe $ gql "NodeTypeName"]

nodeTypeFiller :: Binding
nodeTypeFiller = def "NodeTypeFiller" $
  T.union [
    "keyLabelSet">: gql "NodeKeyLabelSetWithContent",
    "impliedContent">: gql "NodeTypeImpliedContent"]

nodeTypeImpliedContent :: Binding
nodeTypeImpliedContent = def "NodeTypeImpliedContent" $
  T.union [
    "labelSet">: gql "NodeTypeLabelSet",
    "propertyTypes">: gql "NodeTypePropertyTypes",
    "labelSetWithProperties">: gql "NodeLabelSetWithProperties"]

nodeTypeKeyLabelSet :: Binding
nodeTypeKeyLabelSet = def "NodeTypeKeyLabelSet" $
  T.maybe $ gql "LabelSetPhrase"

nodeTypeLabelSet :: Binding
nodeTypeLabelSet = def "NodeTypeLabelSet" $
  gql "LabelSetPhrase"

nodeTypeName :: Binding
nodeTypeName = def "NodeTypeName" $
  T.string

nodeTypeNameWithFiller :: Binding
nodeTypeNameWithFiller = def "NodeTypeNameWithFiller" $
  T.record [
    "typeName">: gql "NodeTypeName",
    "filler">: T.maybe $ gql "NodeTypeFiller"]

nodeTypePattern :: Binding
nodeTypePattern = def "NodeTypePattern" $
  T.record [
    "synonymAndTypeName">: T.maybe $ gql "NodeSynonymAndTypeName",
    "alias">: T.maybe $ gql "LocalNodeTypeAlias",
    "filler">: T.maybe $ gql "NodeTypeFiller"]

nodeTypePhrase :: Binding
nodeTypePhrase = def "NodeTypePhrase" $
  T.record [
    "synonym">: gql "NodeSynonym",
    "typePhraseFiller">: gql "NodeTypePhraseFiller",
    "alias">: T.maybe $ gql "LocalNodeTypeAlias"]

nodeTypePhraseFiller :: Binding
nodeTypePhraseFiller = def "NodeTypePhraseFiller" $
  T.union [
    "typeName">: gql "NodeTypeNameWithFiller",
    "fillerOnly">: gql "NodeTypeFiller"]

nodeTypePropertyTypes :: Binding
nodeTypePropertyTypes = def "NodeTypePropertyTypes" $
  gql "PropertyTypesSpecification"

nodeTypeSpecification :: Binding
nodeTypeSpecification = def "NodeTypeSpecification" $
  T.union [
    "pattern">: gql "NodeTypePattern",
    "phrase">: gql "NodeTypePhrase"]

nonNegativeIntegerSpecification :: Binding
nonNegativeIntegerSpecification = def "NonNegativeIntegerSpecification" $
  T.union [
    "unsignedInteger">: gql "UnsignedInteger",
    "dynamicParameterSpecification">: gql "DynamicParameterSpecification"]

nonParenthesizedPrimaryValueExpression :: Binding
nonParenthesizedPrimaryValueExpression = def "NonParenthesizedPrimaryValueExpression" $
  T.union [
    "special">: gql "NonParenthesizedPrimaryValueExpressionSpecialCase",
    "bindingVariable">: gql "BindingVariableReference"]

nonParenthesizedPrimaryValueExpressionSpecialCase :: Binding
nonParenthesizedPrimaryValueExpressionSpecialCase = def "NonParenthesizedPrimaryValueExpressionSpecialCase" $
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

normalForm :: Binding
normalForm = def "NormalForm" $
  T.enum ["nfc", "nfd", "nfkc", "nfkd"]

normalizeCharacterString :: Binding
normalizeCharacterString = def "NormalizeCharacterString" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "normalForm">: T.maybe $ gql "NormalForm"]

normalizedPredicateExpr :: Binding
normalizedPredicateExpr = def "NormalizedPredicateExpr" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "normalizedPredicate">: gql "NormalizedPredicatePart2"]

normalizedPredicatePart2 :: Binding
normalizedPredicatePart2 = def "NormalizedPredicatePart2" $
  T.record [
    "not">: T.boolean,
    "normalForm">: T.maybe $ gql "NormalForm"]

notExpr :: Binding
notExpr = def "NotExpr" $
  gql "ValueExpression"

notNull :: Binding
notNull = def "NotNull" $
  T.unit

nullIfAbbreviation :: Binding
nullIfAbbreviation = def "NullIfAbbreviation" $
  T.record [
    "first">: gql "ValueExpression",
    "second">: gql "ValueExpression"]

nullLiteral :: Binding
nullLiteral = def "NullLiteral" $
  T.unit

nullOrdering :: Binding
nullOrdering = def "NullOrdering" $
  T.enum ["nullsFirst", "nullsLast"]

nullPredicate :: Binding
nullPredicate = def "NullPredicate" $
  T.record [
    "valueExpression">: gql "PrimaryValueExpression",
    "nullPart">: gql "NullPredicatePart2"]

nullPredicatePart2 :: Binding
nullPredicatePart2 = def "NullPredicatePart2" $
  T.record [
    "not">: T.boolean]

nullType :: Binding
nullType = def "NullType" $
  T.unit

numberOfGroups :: Binding
numberOfGroups = def "NumberOfGroups" $
  gql "NonNegativeIntegerSpecification"

numberOfPaths :: Binding
numberOfPaths = def "NumberOfPaths" $
  gql "NonNegativeIntegerSpecification"

numericType :: Binding
numericType = def "NumericType" $
  T.union [
    "exact">: gql "ExactNumericType",
    "approximate">: gql "ApproximateNumericType"]

numericValueExpression :: Binding
numericValueExpression = def "NumericValueExpression" $
  T.union [
    "signed">: gql "SignedNumericValueExpression",
    "multiplicationOrDivision">: gql "MulDivNumericValueExpression",
    "additionOrSubtraction">: gql "AddSubNumericValueExpression",
    "primary">: gql "PrimaryValueExpression",
    "function">: gql "NumericValueFunction"]

numericValueExpressionBase :: Binding
numericValueExpressionBase = def "NumericValueExpressionBase" $
  gql "NumericValueExpression"

numericValueExpressionDividend :: Binding
numericValueExpressionDividend = def "NumericValueExpressionDividend" $
  gql "NumericValueExpression"

numericValueExpressionDivisor :: Binding
numericValueExpressionDivisor = def "NumericValueExpressionDivisor" $
  gql "NumericValueExpression"

numericValueExpressionExponent :: Binding
numericValueExpressionExponent = def "NumericValueExpressionExponent" $
  gql "NumericValueExpression"

numericValueFunction :: Binding
numericValueFunction = def "NumericValueFunction" $
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

objectExpressionPrimary :: Binding
objectExpressionPrimary = def "ObjectExpressionPrimary" $
  T.union [
    "variable">: gql "PrimaryValueExpression",
    "parenthesized">: gql "ParenthesizedValueExpression",
    "nonParenthesized">: gql "NonParenthesizedPrimaryValueExpressionSpecialCase"]

objectName :: Binding
objectName = def "ObjectName" $
  T.string

objectNameOrBindingVariable :: Binding
objectNameOrBindingVariable = def "ObjectNameOrBindingVariable" $
  T.string

ofGraphType :: Binding
ofGraphType = def "OfGraphType" $
  T.union [
    "likeGraph">: gql "GraphTypeLikeGraph",
    "reference">: gql "TypedGraphTypeReference",
    "nested">: gql "TypedNestedGraphTypeSpecification"]

offsetAndOptionalLimit :: Binding
offsetAndOptionalLimit = def "OffsetAndOptionalLimit" $
  T.record [
    "offset">: gql "OffsetClause",
    "limit">: T.maybe $ gql "LimitClause"]

offsetClause :: Binding
offsetClause = def "OffsetClause" $
  T.record [
    "synonym">: gql "OffsetSynonym",
    "value">: gql "NonNegativeIntegerSpecification"]

offsetSynonym :: Binding
offsetSynonym = def "OffsetSynonym" $
  T.enum ["offset", "skipReservedWord"]

openDynamicUnionType :: Binding
openDynamicUnionType = def "OpenDynamicUnionType" $
  T.record [
    "value">: T.boolean,
    "notNull">: T.boolean]

openEdgeReferenceValueType :: Binding
openEdgeReferenceValueType = def "OpenEdgeReferenceValueType" $
  T.record [
    "any">: T.boolean,
    "edgeSynonym">: gql "EdgeSynonym",
    "notNull">: T.boolean]

openGraphReferenceValueType :: Binding
openGraphReferenceValueType = def "OpenGraphReferenceValueType" $
  T.record [
    "any">: T.maybe T.boolean,
    "property">: T.boolean,
    "notNull">: T.boolean]

openGraphType :: Binding
openGraphType = def "OpenGraphType" $
  T.record [
    "typed">: T.maybe $ gql "Typed",
    "graph">: T.boolean]

openNodeReferenceValueType :: Binding
openNodeReferenceValueType = def "OpenNodeReferenceValueType" $
  T.record [
    "any">: T.boolean,
    "nodeSynonym">: gql "NodeSynonym",
    "notNull">: T.boolean]

optTypedBindingTableInitializer :: Binding
optTypedBindingTableInitializer = def "OptTypedBindingTableInitializer" $
  T.record [
    "type">: T.maybe $ gql "TypedBindingTableReferenceValueType",
    "initializer">: gql "BindingTableInitializer"]

optTypedGraphInitializer :: Binding
optTypedGraphInitializer = def "OptTypedGraphInitializer" $
  T.record [
    "type">: T.maybe $ gql "TypedGraphReferenceValueType",
    "initializer">: gql "GraphInitializer"]

optTypedValueInitializer :: Binding
optTypedValueInitializer = def "OptTypedValueInitializer" $
  T.record [
    "type">: T.maybe $ gql "TypedValueType",
    "initializer">: gql "ValueInitializer"]

optionalMatchStatement :: Binding
optionalMatchStatement = def "OptionalMatchStatement" $
  gql "OptionalOperand"

optionalOperand :: Binding
optionalOperand = def "OptionalOperand" $
  T.union [
    "simple">: gql "SimpleMatchStatement",
    "braceBlock">: gql "MatchStatementBlock",
    "parenBlock">: gql "MatchStatementBlock"]

orderByAndOptionalOffsetAndLimit :: Binding
orderByAndOptionalOffsetAndLimit = def "OrderByAndOptionalOffsetAndLimit" $
  T.record [
    "orderBy">: gql "OrderByClause",
    "offset">: T.maybe $ gql "OffsetClause",
    "limit">: T.maybe $ gql "LimitClause"]

orderByAndPageStatement :: Binding
orderByAndPageStatement = def "OrderByAndPageStatement" $
  T.union [
    "orderByAndOptionalOffsetAndLimit">: gql "OrderByAndOptionalOffsetAndLimit",
    "offsetAndOptionalLimit">: gql "OffsetAndOptionalLimit",
    "limitOnly">: gql "LimitClause"]

orderByClause :: Binding
orderByClause = def "OrderByClause" $
  gql "SortSpecificationList"

orderingSpecification :: Binding
orderingSpecification = def "OrderingSpecification" $
  T.enum ["ascending", "descending"]

ordinalityOrOffsetType :: Binding
ordinalityOrOffsetType = def "OrdinalityOrOffsetType" $
  T.enum ["ordinality", "offset"]

parameterName :: Binding
parameterName = def "ParameterName" $
  T.string

parameterSessionSpecification :: Binding
parameterSessionSpecification = def "ParameterSessionSpecification" $
  T.record [
    "parameter">: T.boolean,
    "sessionParameterSpecification">: gql "SessionParameterSpecification"]

parametersOrCharacteristics :: Binding
parametersOrCharacteristics = def "ParametersOrCharacteristics" $
  T.enum ["parameters", "characteristics"]

parentAndGraphName :: Binding
parentAndGraphName = def "ParentAndGraphName" $
  T.record [
    "parentReference">: gql "CatalogObjectParentReference",
    "graphName">: gql "GraphName"]

parentAndTableName :: Binding
parentAndTableName = def "ParentAndTableName" $
  T.record [
    "parentReference">: gql "CatalogObjectParentReference",
    "tableName">: gql "BindingTableName"]

parenthesizedPathPatternExpression :: Binding
parenthesizedPathPatternExpression = def "ParenthesizedPathPatternExpression" $
  T.record [
    "subpathDeclaration">: T.maybe $ gql "SubpathVariableDeclaration",
    "pathMode">: T.maybe $ gql "PathModePrefix",
    "expression">: gql "PathPatternExpression",
    "whereClause">: T.maybe $ gql "ParenthesizedPathPatternWhereClause"]

parenthesizedPathPatternWhereClause :: Binding
parenthesizedPathPatternWhereClause = def "ParenthesizedPathPatternWhereClause" $
  gql "SearchCondition"

parenthesizedValueExpression :: Binding
parenthesizedValueExpression = def "ParenthesizedValueExpression" $
  gql "ValueExpression"

pathElementList :: Binding
pathElementList = def "PathElementList" $
  T.record [
    "start">: gql "PathElementListStart",
    "steps">: T.list $ gql "PathElementListStep"]

pathElementListStart :: Binding
pathElementListStart = def "PathElementListStart" $
  gql "NodeReferenceValueExpression"

pathElementListStep :: Binding
pathElementListStep = def "PathElementListStep" $
  T.record [
    "edgeReference">: gql "EdgeReferenceValueExpression",
    "nodeReference">: gql "NodeReferenceValueExpression"]

pathFactor :: Binding
pathFactor = def "PathFactor" $
  T.union [
    "primary">: gql "PathPrimary",
    "quantifiedPrimary">: gql "QuantifiedPathPrimary",
    "questionedPrimary">: gql "QuestionedPathPrimary"]

pathLengthExpression :: Binding
pathLengthExpression = def "PathLengthExpression" $
  gql "PathValueExpression"

pathMode :: Binding
pathMode = def "PathMode" $
  T.enum ["walk", "trail", "simple", "acyclic"]

pathModePrefix :: Binding
pathModePrefix = def "PathModePrefix" $
  T.record [
    "mode">: gql "PathMode",
    "orPaths">: T.maybe $ gql "PathOrPaths"]

pathOrPaths :: Binding
pathOrPaths = def "PathOrPaths" $
  T.enum ["path", "paths"]

pathPattern :: Binding
pathPattern = def "PathPattern" $
  T.record [
    "variableDeclaration">: T.maybe $ gql "PathVariableDeclaration",
    "prefix">: T.maybe $ gql "PathPatternPrefix",
    "expression">: gql "PathPatternExpression"]

pathPatternExpression :: Binding
pathPatternExpression = def "PathPatternExpression" $
  T.union [
    "term">: gql "PathTerm",
    "multisetAlternation">: nonemptyList $ gql "PathTerm",
    "patternUnion">: nonemptyList $ gql "PathTerm"]

pathPatternList :: Binding
pathPatternList = def "PathPatternList" $
  nonemptyList $ gql "PathPattern"

pathPatternPrefix :: Binding
pathPatternPrefix = def "PathPatternPrefix" $
  T.union [
    "modePrefix">: gql "PathModePrefix",
    "searchPrefix">: gql "PathSearchPrefix"]

pathPrimary :: Binding
pathPrimary = def "PathPrimary" $
  T.union [
    "elementPattern">: gql "ElementPattern",
    "parenthesizedExpression">: gql "ParenthesizedPathPatternExpression",
    "simplifiedExpression">: gql "SimplifiedPathPatternExpression"]

pathSearchPrefix :: Binding
pathSearchPrefix = def "PathSearchPrefix" $
  T.union [
    "all">: gql "AllPathSearch",
    "any">: gql "AnyPathSearch",
    "shortest">: gql "ShortestPathSearch"]

pathTerm :: Binding
pathTerm = def "PathTerm" $
  nonemptyList $ gql "PathFactor"

pathValueConstructor :: Binding
pathValueConstructor = def "PathValueConstructor" $
  gql "PathValueConstructorByEnumeration"

pathValueConstructorByEnumeration :: Binding
pathValueConstructorByEnumeration = def "PathValueConstructorByEnumeration" $
  gql "PathElementList"

pathValueExpression :: Binding
pathValueExpression = def "PathValueExpression" $
  gql "ValueExpression"

pathValueType :: Binding
pathValueType = def "PathValueType" $
  T.record [
    "notNull">: T.boolean]

pathVariable :: Binding
pathVariable = def "PathVariable" $
  gql "BindingVariable"

pathVariableDeclaration :: Binding
pathVariableDeclaration = def "PathVariableDeclaration" $
  gql "PathVariable"

pathVariableReference :: Binding
pathVariableReference = def "PathVariableReference" $
  gql "BindingVariableReference"

powerFunction :: Binding
powerFunction = def "PowerFunction" $
  T.record [
    "base">: gql "NumericValueExpressionBase",
    "exponent">: gql "NumericValueExpressionExponent"]

precision :: Binding
precision = def "Precision" $
  gql "UnsignedDecimalInteger"

precisionAndScale :: Binding
precisionAndScale = def "PrecisionAndScale" $
  T.record [
    "precision">: gql "Precision",
    "scale">: T.maybe $ gql "Scale",
    "notNull">: T.boolean]

predefinedSchemaReference :: Binding
predefinedSchemaReference = def "PredefinedSchemaReference" $
  T.enum ["homeSchema", "currentSchema", "period"]

predefinedType :: Binding
predefinedType = def "PredefinedType" $
  T.union [
    "booleanType">: gql "BooleanType",
    "characterStringType">: gql "CharacterStringType",
    "byteStringType">: gql "ByteStringType",
    "numericType">: gql "NumericType",
    "temporalType">: gql "TemporalType",
    "referenceValueType">: gql "ReferenceValueType",
    "immaterialValueType">: gql "ImmaterialValueType"]

predicate :: Binding
predicate = def "Predicate" $
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

primaryValueExpression :: Binding
primaryValueExpression = def "PrimaryValueExpression" $
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

primitiveCatalogModifyingStatement :: Binding
primitiveCatalogModifyingStatement = def "PrimitiveCatalogModifyingStatement" $
  T.union [
    "createSchema">: gql "CreateSchemaStatement",
    "dropSchema">: gql "DropSchemaStatement",
    "createGraph">: gql "CreateGraphStatement",
    "dropGraph">: gql "DropGraphStatement",
    "createGraphType">: gql "CreateGraphTypeStatement",
    "dropGraphType">: gql "DropGraphTypeStatement"]

primitiveDataModifyingStatement :: Binding
primitiveDataModifyingStatement = def "PrimitiveDataModifyingStatement" $
  T.union [
    "insert">: gql "InsertStatement",
    "set">: gql "SetStatement",
    "remove">: gql "RemoveStatement",
    "delete">: gql "DeleteStatement"]

primitiveQueryStatement :: Binding
primitiveQueryStatement = def "PrimitiveQueryStatement" $
  T.union [
    "match">: gql "MatchStatement",
    "let">: gql "LetStatement",
    "for">: gql "ForStatement",
    "filter">: gql "FilterStatement",
    "orderByAndPage">: gql "OrderByAndPageStatement"]

primitiveResultStatement :: Binding
primitiveResultStatement = def "PrimitiveResultStatement" $
  T.union [
    "returnAndOptionalOrderBy">: gql "ReturnAndOptionalOrderByAndPage",
    "finish">: T.unit]

procedureAndMaybeEnd :: Binding
procedureAndMaybeEnd = def "ProcedureAndMaybeEnd" $
  T.record [
    "procedure">: gql "ProcedureSpecification",
    "end">: T.maybe $ gql "EndTransactionCommand"]

procedureArgument :: Binding
procedureArgument = def "ProcedureArgument" $
  gql "ValueExpression"

procedureArgumentList :: Binding
procedureArgumentList = def "ProcedureArgumentList" $
  nonemptyList $ gql "ProcedureArgument"

procedureBody :: Binding
procedureBody = def "ProcedureBody" $
  T.record [
    "atSchema">: T.maybe $ gql "AtSchemaClause",
    "bindings">: T.maybe $ gql "BindingVariableDefinitionBlock",
    "statements">: gql "StatementBlock"]

procedureCall :: Binding
procedureCall = def "ProcedureCall" $
  T.union [
    "inline">: gql "InlineProcedureCall",
    "named">: gql "NamedProcedureCall"]

procedureName :: Binding
procedureName = def "ProcedureName" $
  T.string

procedureReference :: Binding
procedureReference = def "ProcedureReference" $
  T.union [
    "parentAndProcedureName">: gql "CatalogProcedureParentAndName",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

procedureSpecification :: Binding
procedureSpecification = def "ProcedureSpecification" $
  gql "ProcedureBody"

programActivity :: Binding
programActivity = def "ProgramActivity" $
  T.union [
    "session">: gql "SessionActivity",
    "transaction">: gql "TransactionActivity"]

propertyExistsPredicate :: Binding
propertyExistsPredicate = def "PropertyExistsPredicate" $
  T.record [
    "elementVariableReference">: gql "ElementVariableReference",
    "propertyName">: gql "PropertyName"]

propertyKeyValuePair :: Binding
propertyKeyValuePair = def "PropertyKeyValuePair" $
  T.record [
    "name">: gql "PropertyName",
    "value">: gql "ValueExpression"]

propertyKeyValuePairList :: Binding
propertyKeyValuePairList = def "PropertyKeyValuePairList" $
  nonemptyList $ gql "PropertyKeyValuePair"

propertyName :: Binding
propertyName = def "PropertyName" $
  T.string

propertyReference :: Binding
propertyReference = def "PropertyReference" $
  T.record [
    "valueExpression">: gql "PrimaryValueExpression",
    "propertyName">: gql "PropertyName"]

propertyType :: Binding
propertyType = def "PropertyType" $
  T.record [
    "name">: gql "PropertyName",
    "typed">: T.maybe $ gql "Typed",
    "valueType">: gql "PropertyValueType"]

propertyTypeList :: Binding
propertyTypeList = def "PropertyTypeList" $
  nonemptyList $ gql "PropertyType"

propertyTypesSpecification :: Binding
propertyTypesSpecification = def "PropertyTypesSpecification" $
  T.maybe $ gql "PropertyTypeList"

propertyValueType :: Binding
propertyValueType = def "PropertyValueType" $
  gql "ValueType"

quantifiedPathPrimary :: Binding
quantifiedPathPrimary = def "QuantifiedPathPrimary" $
  T.record [
    "primary">: gql "PathPrimary",
    "quantifier">: gql "GraphPatternQuantifier"]

queryConjunction :: Binding
queryConjunction = def "QueryConjunction" $
  T.union [
    "setOperator">: gql "SetOperator",
    "otherwise">: T.unit]

questionedPathPrimary :: Binding
questionedPathPrimary = def "QuestionedPathPrimary" $
  gql "PathPrimary"

realType :: Binding
realType = def "RealType" $
  T.record [
    "notNull">: T.boolean]

recordConstructor :: Binding
recordConstructor = def "RecordConstructor" $
  gql "FieldsSpecification"

recordLiteral :: Binding
recordLiteral = def "RecordLiteral" $
  gql "RecordConstructor"

recordType :: Binding
recordType = def "RecordType" $
  T.union [
    "anyRecord">: gql "AnyRecordType",
    "specifiedRecord">: gql "SpecifiedRecordType"]

referenceParameterSpecification :: Binding
referenceParameterSpecification = def "ReferenceParameterSpecification" $
  T.unit

referenceValueType :: Binding
referenceValueType = def "ReferenceValueType" $
  T.union [
    "graph">: gql "GraphReferenceValueType",
    "bindingTable">: gql "BindingTableReferenceValueType",
    "node">: gql "NodeReferenceValueType",
    "edge">: gql "EdgeReferenceValueType"]

regularIdentifier :: Binding
regularIdentifier = def "RegularIdentifier" $
  T.string

relativeCatalogSchemaReference :: Binding
relativeCatalogSchemaReference = def "RelativeCatalogSchemaReference" $
  T.union [
    "predefinedReference">: gql "PredefinedSchemaReference",
    "directoryAndSchema">: gql "RelativeDirectoryAndSchema"]

relativeDirectoryAndSchema :: Binding
relativeDirectoryAndSchema = def "RelativeDirectoryAndSchema" $
  T.record [
    "directoryPath">: gql "RelativeDirectoryPath",
    "schemaName">: gql "SchemaName"]

relativeDirectoryPath :: Binding
relativeDirectoryPath = def "RelativeDirectoryPath" $
  T.record [
    "parentDirectories">: T.nonNegativeInt32,
    "simplePath">: T.maybe $ gql "SimpleDirectoryPath"]

removeItem :: Binding
removeItem = def "RemoveItem" $
  T.union [
    "property">: gql "RemovePropertyItem",
    "label">: gql "RemoveLabelItem"]

removeItemList :: Binding
removeItemList = def "RemoveItemList" $
  nonemptyList $ gql "RemoveItem"

removeLabelItem :: Binding
removeLabelItem = def "RemoveLabelItem" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "isOrColon">: gql "IsOrColon",
    "label">: gql "LabelName"]

removePropertyItem :: Binding
removePropertyItem = def "RemovePropertyItem" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "propertyName">: gql "PropertyName"]

removeStatement :: Binding
removeStatement = def "RemoveStatement" $
  gql "RemoveItemList"

repeatableElementsMatchMode :: Binding
repeatableElementsMatchMode = def "RepeatableElementsMatchMode" $
  gql "ElementBindingsOrElements"

result :: Binding
result = def "Result" $
  T.union [
    "simple">: gql "ResultExpression",
    "nullLiteral">: T.unit]

resultExpression :: Binding
resultExpression = def "ResultExpression" $
  gql "ValueExpression"

returnAndOptionalOrderByAndPage :: Binding
returnAndOptionalOrderByAndPage = def "ReturnAndOptionalOrderByAndPage" $
  T.record [
    "return">: gql "ReturnStatement",
    "orderByAndPage">: T.maybe $ gql "OrderByAndPageStatement"]

returnItem :: Binding
returnItem = def "ReturnItem" $
  T.record [
    "expression">: gql "AggregatingValueExpression",
    "alias">: T.maybe $ gql "ReturnItemAlias"]

returnItemAlias :: Binding
returnItemAlias = def "ReturnItemAlias" $
  T.string

returnItemList :: Binding
returnItemList = def "ReturnItemList" $
  nonemptyList $ gql "ReturnItem"

returnItems :: Binding
returnItems = def "ReturnItems" $
  T.union [
    "asterisk">: T.unit,
    "itemList">: gql "ReturnItemList"]

returnItemsAndGroupBy :: Binding
returnItemsAndGroupBy = def "ReturnItemsAndGroupBy" $
  T.record [
    "quantifier">: T.maybe $ gql "SetQuantifier",
    "items">: gql "ReturnItems",
    "groupBy">: T.maybe $ gql "GroupByClause"]

returnStatement :: Binding
returnStatement = def "ReturnStatement" $
  gql "ReturnStatementBody"

returnStatementBody :: Binding
returnStatementBody = def "ReturnStatementBody" $
  T.union [
    "items">: gql "ReturnItemsAndGroupBy",
    "noBindings">: T.unit]

rollbackCommand :: Binding
rollbackCommand = def "RollbackCommand" $
  T.unit

samePredicate :: Binding
samePredicate = def "SamePredicate" $
  T.record [
    "references">: nonemptyList $ gql "ElementVariableReference"]

scale :: Binding
scale = def "Scale" $
  gql "UnsignedDecimalInteger"

schemaAndObjects :: Binding
schemaAndObjects = def "SchemaAndObjects" $
  T.record [
    "schemaReference">: gql "SchemaReference",
    "objects">: T.list $ gql "ObjectName"]

schemaName :: Binding
schemaName = def "SchemaName" $
  T.string

schemaReference :: Binding
schemaReference = def "SchemaReference" $
  T.union [
    "absoluteReference">: gql "AbsoluteCatalogSchemaReference",
    "relativeReference">: gql "RelativeCatalogSchemaReference",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

searchCondition :: Binding
searchCondition = def "SearchCondition" $
  gql "BooleanValueExpression"

searchedCase :: Binding
searchedCase = def "SearchedCase" $
  T.record [
    "whenClauses">: nonemptyList $ gql "SearchedWhenClause",
    "elseClause">: T.maybe $ gql "ElseClause"]

searchedWhenClause :: Binding
searchedWhenClause = def "SearchedWhenClause" $
  T.record [
    "searchCondition">: gql "SearchCondition",
    "result">: gql "Result"]

selectGraphMatch :: Binding
selectGraphMatch = def "SelectGraphMatch" $
  T.record [
    "graphExpression">: gql "GraphExpression",
    "matchStatement">: gql "MatchStatement"]

selectGraphMatchList :: Binding
selectGraphMatchList = def "SelectGraphMatchList" $
  nonemptyList $ gql "SelectGraphMatch"

selectItem :: Binding
selectItem = def "SelectItem" $
  T.record [
    "expression">: gql "AggregatingValueExpression",
    "alias">: T.maybe $ gql "SelectItemAlias"]

selectItemAlias :: Binding
selectItemAlias = def "SelectItemAlias" $
  T.string

selectItemList :: Binding
selectItemList = def "SelectItemList" $
  nonemptyList $ gql "SelectItem"

selectItems :: Binding
selectItems = def "SelectItems" $
  T.union [
    "asterisk">: T.unit,
    "itemList">: gql "SelectItemList"]

selectQuerySpecification :: Binding
selectQuerySpecification = def "SelectQuerySpecification" $
  T.union [
    "nested">: gql "NestedQuerySpecification",
    "graphAndNested">: gql "GraphAndNestedQuerySpecification"]

selectStatement :: Binding
selectStatement = def "SelectStatement" $
  T.record [
    "quantifier">: T.maybe $ gql "SetQuantifier",
    "items">: gql "SelectItems",
    "body">: T.maybe $ gql "SelectStatementBodyAndClauses"]

selectStatementBody :: Binding
selectStatementBody = def "SelectStatementBody" $
  T.union [
    "graphMatchList">: gql "SelectGraphMatchList",
    "querySpecification">: gql "SelectQuerySpecification"]

selectStatementBodyAndClauses :: Binding
selectStatementBodyAndClauses = def "SelectStatementBodyAndClauses" $
  T.record [
    "body">: gql "SelectStatementBody",
    "where">: T.maybe $ gql "WhereClause",
    "groupBy">: T.maybe $ gql "GroupByClause",
    "having">: T.maybe $ gql "HavingClause",
    "orderBy">: T.maybe $ gql "OrderByClause",
    "offset">: T.maybe $ gql "OffsetClause",
    "limit">: T.maybe $ gql "LimitClause"]

sessionActivity :: Binding
sessionActivity = def "SessionActivity" $
  T.union [
    "reset">: nonemptyList $ gql "SessionResetCommand",
    "setAndResetCommands">: gql "SessionSetAndResetCommands"]

sessionCloseCommand :: Binding
sessionCloseCommand = def "SessionCloseCommand" $
  T.unit

sessionParameterSpecification :: Binding
sessionParameterSpecification = def "SessionParameterSpecification" $
  gql "ParameterName"

sessionResetArguments :: Binding
sessionResetArguments = def "SessionResetArguments" $
  T.union [
    "parametersOrCharacteristics">: gql "AllParametersOrCharacteristics",
    "schema">: T.unit,
    "graph">: T.unit,
    "timeZone">: T.unit,
    "parameterSessionSpecification">: gql "ParameterSessionSpecification"]

sessionResetCommand :: Binding
sessionResetCommand = def "SessionResetCommand" $
  T.maybe $ gql "SessionResetArguments"

sessionSetAndResetCommands :: Binding
sessionSetAndResetCommands = def "SessionSetAndResetCommands" $
  T.record [
    "set">: nonemptyList $ gql "SessionSetCommand",
    "reset">: T.list $ gql "SessionResetCommand"]

sessionSetBindingTableParameterClause :: Binding
sessionSetBindingTableParameterClause = def "SessionSetBindingTableParameterClause" $
  T.record [
    "binding">: T.boolean,
    "param">: gql "SessionSetParameterName",
    "init">: gql "OptTypedBindingTableInitializer"]

sessionSetCommand :: Binding
sessionSetCommand = def "SessionSetCommand" $
  T.union [
    "schema">: gql "SessionSetSchemaClause",
    "graph">: gql "SessionSetGraphClause",
    "timeZone">: gql "SessionSetTimeZoneClause",
    "parameter">: gql "SessionSetParameterClause"]

sessionSetGraphClause :: Binding
sessionSetGraphClause = def "SessionSetGraphClause" $
  gql "GraphExpression"

sessionSetGraphParameterClause :: Binding
sessionSetGraphParameterClause = def "SessionSetGraphParameterClause" $
  T.record [
    "graph">: gql "SessionSetParameterName",
    "initializer">: gql "OptTypedGraphInitializer"]

sessionSetParameterClause :: Binding
sessionSetParameterClause = def "SessionSetParameterClause" $
  T.union [
    "graph">: gql "SessionSetGraphParameterClause",
    "bindings">: gql "SessionSetBindingTableParameterClause",
    "value">: gql "SessionSetValueParameterClause"]

sessionSetParameterName :: Binding
sessionSetParameterName = def "SessionSetParameterName" $
  T.record [
    "ifNotExists">: T.boolean,
    "parameter">: gql "SessionParameterSpecification"]

sessionSetSchemaClause :: Binding
sessionSetSchemaClause = def "SessionSetSchemaClause" $
  gql "SchemaReference"

sessionSetTimeZoneClause :: Binding
sessionSetTimeZoneClause = def "SessionSetTimeZoneClause" $
  gql "SetTimeZoneValue"

sessionSetValueParameterClause :: Binding
sessionSetValueParameterClause = def "SessionSetValueParameterClause" $
  T.record [
    "value">: gql "SessionSetParameterName",
    "initializer">: gql "OptTypedValueInitializer"]

setAllPropertiesItem :: Binding
setAllPropertiesItem = def "SetAllPropertiesItem" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "properties">: T.maybe $ gql "PropertyKeyValuePairList"]

setItem :: Binding
setItem = def "SetItem" $
  T.union [
    "property">: gql "SetPropertyItem",
    "allProperties">: gql "SetAllPropertiesItem",
    "label">: gql "SetLabelItem"]

setItemList :: Binding
setItemList = def "SetItemList" $
  nonemptyList $ gql "SetItem"

setLabelItem :: Binding
setLabelItem = def "SetLabelItem" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "isOrColon">: gql "IsOrColon",
    "label">: gql "LabelName"]

setOperator :: Binding
setOperator = def "SetOperator" $
  T.record [
    "operatorType">: gql "SetOperatorType",
    "quantifier">: T.maybe $ gql "SetQuantifier"]

setOperatorType :: Binding
setOperatorType = def "SetOperatorType" $
  T.enum ["union", "except", "intersect"]

setPropertyItem :: Binding
setPropertyItem = def "SetPropertyItem" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "propertyName">: gql "PropertyName",
    "value">: gql "ValueExpression"]

setQuantifier :: Binding
setQuantifier = def "SetQuantifier" $
  T.enum ["distinct", "all"]

setStatement :: Binding
setStatement = def "SetStatement" $
  gql "SetItemList"

setTimeZoneValue :: Binding
setTimeZoneValue = def "SetTimeZoneValue" $
  gql "TimeZoneString"

shortestPathSearch :: Binding
shortestPathSearch = def "ShortestPathSearch" $
  T.union [
    "allShortest">: gql "AllShortestPathSearch",
    "anyShortest">: gql "AnyShortestPathSearch",
    "countedShortest">: gql "CountedShortestPathSearch",
    "countedShortestGroup">: gql "CountedShortestGroupSearch"]

side :: Binding
side = def "Side" $
  T.enum ["left", "right"]

sign :: Binding
sign = def "Sign" $
  T.enum ["plus", "minus"]

signedBinaryExactNumericType :: Binding
signedBinaryExactNumericType = def "SignedBinaryExactNumericType" $
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

signedExpr :: Binding
signedExpr = def "SignedExpr" $
  T.record [
    "sign">: gql "Sign",
    "valueExpression">: gql "ValueExpression"]

signedNumericValueExpression :: Binding
signedNumericValueExpression = def "SignedNumericValueExpression" $
  T.record [
    "sign">: gql "Sign",
    "expression">: gql "NumericValueExpression"]

signedVerboseBinaryExactNumericType :: Binding
signedVerboseBinaryExactNumericType = def "SignedVerboseBinaryExactNumericType" $
  T.record [
    "signed">: T.boolean,
    "verboseType">: gql "VerboseBinaryExactNumericType"]

simpleCase :: Binding
simpleCase = def "SimpleCase" $
  T.record [
    "caseOperand">: gql "CaseOperand",
    "whenClauses">: nonemptyList $ gql "SimpleWhenClause",
    "elseClause">: T.maybe $ gql "ElseClause"]

simpleCatalogModifyingStatement :: Binding
simpleCatalogModifyingStatement = def "SimpleCatalogModifyingStatement" $
  T.union [
    "primitive">: gql "PrimitiveCatalogModifyingStatement",
    "callProcedure">: gql "CallCatalogModifyingProcedureStatement"]

simpleDataAccessingStatement :: Binding
simpleDataAccessingStatement = def "SimpleDataAccessingStatement" $
  T.union [
    "query">: gql "SimpleQueryStatement",
    "modifying">: gql "SimpleDataModifyingStatement"]

simpleDataModifyingStatement :: Binding
simpleDataModifyingStatement = def "SimpleDataModifyingStatement" $
  T.union [
    "primitive">: gql "PrimitiveDataModifyingStatement",
    "callProcedure">: gql "CallDataModifyingProcedureStatement"]

simpleDirectoryPath :: Binding
simpleDirectoryPath = def "SimpleDirectoryPath" $
  nonemptyList $ gql "DirectoryName"

simpleLinearDataAccessingStatement :: Binding
simpleLinearDataAccessingStatement = def "SimpleLinearDataAccessingStatement" $
  nonemptyList $ gql "SimpleDataAccessingStatement"

simpleLinearQueryStatement :: Binding
simpleLinearQueryStatement = def "SimpleLinearQueryStatement" $
  nonemptyList $ gql "SimpleQueryStatement"

simpleMatchStatement :: Binding
simpleMatchStatement = def "SimpleMatchStatement" $
  gql "GraphPatternBindingTable"

simpleQueryStatement :: Binding
simpleQueryStatement = def "SimpleQueryStatement" $
  T.union [
    "primitive">: gql "PrimitiveQueryStatement",
    "call">: gql "CallQueryStatement"]

simpleWhenClause :: Binding
simpleWhenClause = def "SimpleWhenClause" $
  T.record [
    "whenOperands">: gql "WhenOperandList",
    "result">: gql "Result"]

simplifiedConcatenation :: Binding
simplifiedConcatenation = def "SimplifiedConcatenation" $
  T.record [
    "initialTerm">: gql "SimplifiedTerm",
    "nextFactor">: gql "SimplifiedFactorLow"]

simplifiedConjunction :: Binding
simplifiedConjunction = def "SimplifiedConjunction" $
  T.record [
    "left">: gql "SimplifiedFactorLow",
    "right">: gql "SimplifiedFactorHigh"]

simplifiedContents :: Binding
simplifiedContents = def "SimplifiedContents" $
  T.union [
    "term">: gql "SimplifiedTerm",
    "pathUnion">: gql "SimplifiedPathUnion",
    "multisetAlternation">: gql "SimplifiedMultisetAlternation"]

simplifiedDefaultingAnyDirection :: Binding
simplifiedDefaultingAnyDirection = def "SimplifiedDefaultingAnyDirection" $
  gql "SimplifiedContents"

simplifiedDefaultingLeft :: Binding
simplifiedDefaultingLeft = def "SimplifiedDefaultingLeft" $
  gql "SimplifiedContents"

simplifiedDefaultingLeftOrRight :: Binding
simplifiedDefaultingLeftOrRight = def "SimplifiedDefaultingLeftOrRight" $
  gql "SimplifiedContents"

simplifiedDefaultingLeftOrUndirected :: Binding
simplifiedDefaultingLeftOrUndirected = def "SimplifiedDefaultingLeftOrUndirected" $
  gql "SimplifiedContents"

simplifiedDefaultingRight :: Binding
simplifiedDefaultingRight = def "SimplifiedDefaultingRight" $
  gql "SimplifiedContents"

simplifiedDefaultingUndirected :: Binding
simplifiedDefaultingUndirected = def "SimplifiedDefaultingUndirected" $
  gql "SimplifiedContents"

simplifiedDefaultingUndirectedOrRight :: Binding
simplifiedDefaultingUndirectedOrRight = def "SimplifiedDefaultingUndirectedOrRight" $
  gql "SimplifiedContents"

simplifiedDirectionOverride :: Binding
simplifiedDirectionOverride = def "SimplifiedDirectionOverride" $
  T.union [
    "overrideLeft">: gql "SimplifiedOverrideLeft",
    "overrideUndirected">: gql "SimplifiedOverrideUndirected",
    "overrideRight">: gql "SimplifiedOverrideRight",
    "overrideLeftOrUndirected">: gql "SimplifiedOverrideLeftOrUndirected",
    "overrideUndirectedOrRight">: gql "SimplifiedOverrideUndirectedOrRight",
    "overrideLeftOrRight">: gql "SimplifiedOverrideLeftOrRight",
    "overrideAnyDirection">: gql "SimplifiedOverrideAnyDirection"]

simplifiedFactorHigh :: Binding
simplifiedFactorHigh = def "SimplifiedFactorHigh" $
  T.union [
    "tertiary">: gql "SimplifiedTertiary",
    "quantified">: gql "SimplifiedQuantified",
    "questioned">: gql "SimplifiedQuestioned"]

simplifiedFactorLow :: Binding
simplifiedFactorLow = def "SimplifiedFactorLow" $
  T.union [
    "factorHigh">: gql "SimplifiedFactorHigh",
    "conjunction">: gql "SimplifiedConjunction"]

simplifiedMultisetAlternation :: Binding
simplifiedMultisetAlternation = def "SimplifiedMultisetAlternation" $
  nonemptyList $ gql "SimplifiedTerm"

simplifiedNegation :: Binding
simplifiedNegation = def "SimplifiedNegation" $
  gql "SimplifiedPrimary"

simplifiedOverrideAnyDirection :: Binding
simplifiedOverrideAnyDirection = def "SimplifiedOverrideAnyDirection" $
  gql "SimplifiedSecondary"

simplifiedOverrideLeft :: Binding
simplifiedOverrideLeft = def "SimplifiedOverrideLeft" $
  gql "SimplifiedSecondary"

simplifiedOverrideLeftOrRight :: Binding
simplifiedOverrideLeftOrRight = def "SimplifiedOverrideLeftOrRight" $
  gql "SimplifiedSecondary"

simplifiedOverrideLeftOrUndirected :: Binding
simplifiedOverrideLeftOrUndirected = def "SimplifiedOverrideLeftOrUndirected" $
  gql "SimplifiedSecondary"

simplifiedOverrideRight :: Binding
simplifiedOverrideRight = def "SimplifiedOverrideRight" $
  gql "SimplifiedSecondary"

simplifiedOverrideUndirected :: Binding
simplifiedOverrideUndirected = def "SimplifiedOverrideUndirected" $
  gql "SimplifiedSecondary"

simplifiedOverrideUndirectedOrRight :: Binding
simplifiedOverrideUndirectedOrRight = def "SimplifiedOverrideUndirectedOrRight" $
  gql "SimplifiedSecondary"

simplifiedPathPatternExpression :: Binding
simplifiedPathPatternExpression = def "SimplifiedPathPatternExpression" $
  T.union [
    "left">: gql "SimplifiedDefaultingLeft",
    "undirected">: gql "SimplifiedDefaultingUndirected",
    "right">: gql "SimplifiedDefaultingRight",
    "leftOrUndirected">: gql "SimplifiedDefaultingLeftOrUndirected",
    "undirectedOrRight">: gql "SimplifiedDefaultingUndirectedOrRight",
    "leftOrRight">: gql "SimplifiedDefaultingLeftOrRight",
    "anyDirection">: gql "SimplifiedDefaultingAnyDirection"]

simplifiedPathUnion :: Binding
simplifiedPathUnion = def "SimplifiedPathUnion" $
  nonemptyList $ gql "SimplifiedTerm"

simplifiedPrimary :: Binding
simplifiedPrimary = def "SimplifiedPrimary" $
  T.union [
    "labelName">: gql "LabelName",
    "parenthesizedContents">: gql "SimplifiedContents"]

simplifiedQuantified :: Binding
simplifiedQuantified = def "SimplifiedQuantified" $
  T.record [
    "tertiary">: gql "SimplifiedTertiary",
    "quantifier">: gql "GraphPatternQuantifier"]

simplifiedQuestioned :: Binding
simplifiedQuestioned = def "SimplifiedQuestioned" $
  gql "SimplifiedTertiary"

simplifiedSecondary :: Binding
simplifiedSecondary = def "SimplifiedSecondary" $
  T.union [
    "primary">: gql "SimplifiedPrimary",
    "negation">: gql "SimplifiedNegation"]

simplifiedTerm :: Binding
simplifiedTerm = def "SimplifiedTerm" $
  T.union [
    "factorLow">: gql "SimplifiedFactorLow",
    "concatenation">: gql "SimplifiedConcatenation"]

simplifiedTertiary :: Binding
simplifiedTertiary = def "SimplifiedTertiary" $
  T.union [
    "directionOverride">: gql "SimplifiedDirectionOverride",
    "secondary">: gql "SimplifiedSecondary"]

smallIntType :: Binding
smallIntType = def "SmallIntType" $
  T.record ["notNull">: T.boolean]

smallIntegerType :: Binding
smallIntegerType = def "SmallIntegerType" $
  T.record ["notNull">: T.boolean]

sortKey :: Binding
sortKey = def "SortKey" $
  gql "AggregatingValueExpression"

sortSpecification :: Binding
sortSpecification = def "SortSpecification" $
  T.record [
    "sortKey">: gql "SortKey",
    "ordering">: T.maybe $ gql "OrderingSpecification",
    "nullOrdering">: T.maybe $ gql "NullOrdering"]

sortSpecificationList :: Binding
sortSpecificationList = def "SortSpecificationList" $
  nonemptyList $ gql "SortSpecification"

sourceDestinationPredicate :: Binding
sourceDestinationPredicate = def "SourceDestinationPredicate" $
  T.union [
    "sourcePredicate">: gql "SourcePredicate",
    "destinationPredicate">: gql "DestinationPredicate"]

sourceNodeTypeAlias :: Binding
sourceNodeTypeAlias = def "SourceNodeTypeAlias" $
  T.string

sourceNodeTypeReference :: Binding
sourceNodeTypeReference = def "SourceNodeTypeReference" $
  T.union [
    "alias">: gql "SourceNodeTypeAlias",
    "filler">: T.maybe $ gql "NodeTypeFiller"]

sourcePredicate :: Binding
sourcePredicate = def "SourcePredicate" $
  T.record [
    "not">: T.boolean,
    "sourceOf">: gql "EdgeReference"]

specifiedRecordType :: Binding
specifiedRecordType = def "SpecifiedRecordType" $
  T.record [
    "record">: T.boolean,
    "fieldTypes">: gql "FieldTypesSpecification",
    "notNull">: T.boolean]

squareRoot :: Binding
squareRoot = def "SquareRoot" $
  gql "NumericValueExpression"

startAndMaybeProcedureAndMaybeEnd :: Binding
startAndMaybeProcedureAndMaybeEnd = def "StartAndMaybeProcedureAndMaybeEnd" $
  T.record [
    "start">: gql "StartTransactionCommand",
    "procedureAndEnd">: T.maybe $ gql "ProcedureAndMaybeEnd"]

startTransactionCommand :: Binding
startTransactionCommand = def "StartTransactionCommand" $
  T.maybe $ gql "TransactionCharacteristics"

statement :: Binding
statement = def "Statement" $
  T.union [
    "linearCatalogModifying">: gql "LinearCatalogModifyingStatement",
    "linearDataModifying">: gql "LinearDataModifyingStatement",
    "compositeQuery">: gql "CompositeQueryStatement"]

statementBlock :: Binding
statementBlock = def "StatementBlock" $
  T.record [
    "statement">: gql "Statement",
    "nextStatements">: T.list $ gql "NextStatement"]

stringLength :: Binding
stringLength = def "StringLength" $
  gql "NumericValueExpression"

stringType :: Binding
stringType = def "StringType" $
  T.record [
    "minLength">: T.maybe $ gql "MinLength",
    "maxLength">: T.maybe $ gql "MaxLength",
    "notNull">: T.boolean]

subCharacterOrByteString :: Binding
subCharacterOrByteString = def "SubCharacterOrByteString" $
  T.record [
    "side">: gql "Side",
    "valueExpression">: gql "ValueExpression",
    "stringLength">: gql "StringLength"]

subpathVariable :: Binding
subpathVariable = def "SubpathVariable" $
  T.string

subpathVariableDeclaration :: Binding
subpathVariableDeclaration = def "SubpathVariableDeclaration" $
  gql "SubpathVariable"

temporalDurationQualifier :: Binding
temporalDurationQualifier = def "TemporalDurationQualifier" $
  T.enum ["yearToMonth", "dayToSecond"]

temporalDurationType :: Binding
temporalDurationType = def "TemporalDurationType" $
  T.record [
    "qualifier">: gql "TemporalDurationQualifier",
    "notNull">: T.boolean]

temporalInstantType :: Binding
temporalInstantType = def "TemporalInstantType" $
  T.union [
    "datetimeType">: gql "DatetimeType",
    "localDatetimeTypeChoice">: gql "LocalDatetimeTypeChoice",
    "dateType">: gql "DateType",
    "timeType">: gql "TimeType",
    "localTimeTypeChoice">: gql "LocalTimeTypeChoice"]

temporalLiteral :: Binding
temporalLiteral = def "TemporalLiteral" $
  T.union [
    "date">: gql "DateLiteral",
    "time">: gql "TimeLiteral",
    "datetime">: gql "DatetimeLiteral"]

temporalType :: Binding
temporalType = def "TemporalType" $
  T.union [
    "instantType">: gql "TemporalInstantType",
    "durationType">: gql "TemporalDurationType"]

timeFunction :: Binding
timeFunction = def "TimeFunction" $
  T.union [
    "currentTime">: T.unit,
    "zonedTimeWithParams">: T.maybe $ gql "TimeFunctionParameters"]

timeFunctionParameters :: Binding
timeFunctionParameters = def "TimeFunctionParameters" $
  T.union [
    "timeString">: gql "TimeString",
    "recordConstructor">: gql "RecordConstructor"]

timeLiteral :: Binding
timeLiteral = def "TimeLiteral" $
  gql "TimeString"

timeString :: Binding
timeString = def "TimeString" $
  gql "CharacterStringLiteral"

timeType :: Binding
timeType = def "TimeType" $
  T.union [
    "zonedTime">: gql "ZonedTimeType",
    "timeWithTimeZone">: gql "TimeWithTimeZoneType"]

timeWithTimeZoneType :: Binding
timeWithTimeZoneType = def "TimeWithTimeZoneType" $
  T.record [
    "notNull">: T.boolean]

timeWithoutTimeZoneType :: Binding
timeWithoutTimeZoneType = def "TimeWithoutTimeZoneType" $
  T.record [
    "notNull">: T.boolean]

timeZoneString :: Binding
timeZoneString = def "TimeZoneString" $
  gql "CharacterStringLiteral"

timestampWithTimeZoneType :: Binding
timestampWithTimeZoneType = def "TimestampWithTimeZoneType" $
  T.record [
    "notNull">: T.boolean]

timestampWithoutTimeZoneType :: Binding
timestampWithoutTimeZoneType = def "TimestampWithoutTimeZoneType" $
  T.record [
    "notNull">: T.boolean]

transactionAccessMode :: Binding
transactionAccessMode = def "TransactionAccessMode" $
  T.enum [
    "readOnly",
    "readWrite"]

transactionActivity :: Binding
transactionActivity = def "TransactionActivity" $
  T.union [
    "start">: gql "StartAndMaybeProcedureAndMaybeEnd",
    "procedure">: gql "ProcedureAndMaybeEnd",
    "end">: gql "EndTransactionCommand"]

transactionCharacteristics :: Binding
transactionCharacteristics = def "TransactionCharacteristics" $
  nonemptyList $ gql "TransactionMode"

transactionMode :: Binding
transactionMode = def "TransactionMode" $
  gql "TransactionAccessMode"

trigonometricFunction :: Binding
trigonometricFunction = def "TrigonometricFunction" $
  T.record [
    "name">: gql "TrigonometricFunctionName",
    "value">: gql "NumericValueExpression"]

trigonometricFunctionName :: Binding
trigonometricFunctionName = def "TrigonometricFunctionName" $
  T.enum ["sin", "cos", "tan", "cot", "sinh", "cosh", "tanh", "asin", "acos", "atan", "degrees", "radians"]

trimCharacterOrByteString :: Binding
trimCharacterOrByteString = def "TrimCharacterOrByteString" $
  gql "ValueExpression"

trimCharacterOrByteStringSource :: Binding
trimCharacterOrByteStringSource = def "TrimCharacterOrByteStringSource" $
  gql "ValueExpression"

trimListFunction :: Binding
trimListFunction = def "TrimListFunction" $
  T.record [
    "listValue">: gql "ListValueExpression",
    "numericValue">: gql "NumericValueExpression"]

trimMultiCharacterCharacterString :: Binding
trimMultiCharacterCharacterString = def "TrimMultiCharacterCharacterString" $
  T.record [
    "trimType">: gql "TrimType",
    "valueExpression">: gql "ValueExpression",
    "optionalValueExpression">: T.maybe $ gql "ValueExpression"]

trimOperands :: Binding
trimOperands = def "TrimOperands" $
  T.record [
    "specification">: T.maybe $ gql "TrimSpecification",
    "characterOrByteString">: T.maybe $ gql "TrimCharacterOrByteString",
    "source">: gql "TrimCharacterOrByteStringSource"]

trimSingleCharacterOrByteString :: Binding
trimSingleCharacterOrByteString = def "TrimSingleCharacterOrByteString" $
  gql "TrimOperands"

trimSpecification :: Binding
trimSpecification = def "TrimSpecification" $
  T.enum ["leading", "trailing", "both"]

trimType :: Binding
trimType = def "TrimType" $
  T.enum ["btrim", "ltrim", "rtrim"]

truthValue :: Binding
truthValue = def "TruthValue" $
  gql "BooleanLiteral"

typed :: Binding
typed = def "Typed" $
  T.unit

typedBindingTableReferenceValueType :: Binding
typedBindingTableReferenceValueType = def "TypedBindingTableReferenceValueType" $
  T.record [
    "typed">: T.maybe $ gql "Typed",
    "valueType">: gql "BindingTableReferenceValueType"]

typedGraphReferenceValueType :: Binding
typedGraphReferenceValueType = def "TypedGraphReferenceValueType" $
  T.record [
    "typed">: T.maybe $ gql "Typed",
    "valueType">: gql "GraphReferenceValueType"]

typedGraphTypeReference :: Binding
typedGraphTypeReference = def "TypedGraphTypeReference" $
  T.record [
    "typed">: T.maybe $ gql "Typed",
    "reference">: gql "GraphTypeReference"]

typedNestedGraphTypeSpecification :: Binding
typedNestedGraphTypeSpecification = def "TypedNestedGraphTypeSpecification" $
  T.record [
    "typed">: T.maybe $ gql "Typed",
    "graph">: T.boolean,
    "specification">: gql "NestedGraphTypeSpecification"]

typedValueType :: Binding
typedValueType = def "TypedValueType" $
  T.record [
    "typed">: T.maybe $ gql "Typed",
    "valueType">: gql "ValueType"]

uBigIntType :: Binding
uBigIntType = def "UBigIntType" $
  T.record ["notNull">: T.boolean]

uSmallIntType :: Binding
uSmallIntType = def "USmallIntType" $
  T.record ["notNull">: T.boolean]

uint128Type :: Binding
uint128Type = def "Uint128Type" $
  T.record ["notNull">: T.boolean]

uint16Type :: Binding
uint16Type = def "Uint16Type" $
  T.record ["notNull">: T.boolean]

uint256Type :: Binding
uint256Type = def "Uint256Type" $
  T.record ["notNull">: T.boolean]

uint32Type :: Binding
uint32Type = def "Uint32Type" $
  T.record ["notNull">: T.boolean]

uint64Type :: Binding
uint64Type = def "Uint64Type" $
  T.record ["notNull">: T.boolean]

uint8Type :: Binding
uint8Type = def "Uint8Type" $
  T.record ["notNull">: T.boolean]

uintWithPrecision :: Binding
uintWithPrecision = def "UintWithPrecision" $
  T.record [
    "precision">: T.maybe $ gql "Precision",
    "notNull">: T.boolean]

unsignedBinaryExactNumericType :: Binding
unsignedBinaryExactNumericType = def "UnsignedBinaryExactNumericType" $
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

unsignedDecimalInteger :: Binding
unsignedDecimalInteger = def "UnsignedDecimalInteger" $
  T.string

unsignedInteger :: Binding
unsignedInteger = def "UnsignedInteger" $
  T.union [
    "decimal">: T.string,
    "hexadecimal">: T.string,
    "octal">: T.string,
    "binary">: T.string]

unsignedLiteral :: Binding
unsignedLiteral = def "UnsignedLiteral" $
  T.union [
    "numeric">: gql "UnsignedNumericLiteral",
    "general">: gql "GeneralLiteral"]

unsignedNumericLiteral :: Binding
unsignedNumericLiteral = def "UnsignedNumericLiteral" $
  T.union [
    "exact">: gql "ExactNumericLiteral",
    "approximate">: gql "ApproximateNumericLiteral"]

unsignedValueSpecification :: Binding
unsignedValueSpecification = def "UnsignedValueSpecification" $
  T.union [
    "unsignedLiteral">: gql "UnsignedLiteral",
    "generalValueSpecification">: gql "GeneralValueSpecification"]

upperBound :: Binding
upperBound = def "UpperBound" $
  gql "UnsignedInteger"

useGraphClause :: Binding
useGraphClause = def "UseGraphClause" $
  gql "GraphExpression"

valueExpression :: Binding
valueExpression = def "ValueExpression" $
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

valueFunction :: Binding
valueFunction = def "ValueFunction" $
  T.union [
    "numeric">: gql "NumericValueFunction",
    "datetimeSubtraction">: gql "DatetimeSubtraction",
    "datetime">: gql "DatetimeValueFunction",
    "duration">: gql "DurationValueFunction",
    "characterOrByteString">: gql "CharacterOrByteStringFunction",
    "list">: gql "ListValueFunction"]

valueInitializer :: Binding
valueInitializer = def "ValueInitializer" T.unit

valueQueryExpression :: Binding
valueQueryExpression = def "ValueQueryExpression" $
  gql "NestedQuerySpecification"

valueType :: Binding
valueType = def "ValueType" $
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

valueTypePredicate :: Binding
valueTypePredicate = def "ValueTypePredicate" $
  T.record [
    "valueExpression">: gql "PrimaryValueExpression",
    "valueTypePart">: gql "ValueTypePredicatePart2"]

valueTypePredicatePart2 :: Binding
valueTypePredicatePart2 = def "ValueTypePredicatePart2" $
  T.record [
    "not">: T.boolean,
    "typed">: gql "Typed",
    "valueType">: gql "ValueType"]

valueVariableDefinition :: Binding
valueVariableDefinition = def "ValueVariableDefinition" $
  T.record [
    "variable">: gql "BindingVariable",
    "initializer">: gql "OptTypedValueInitializer"]

varbinaryType :: Binding
varbinaryType = def "VarbinaryType" $
  T.record [
    "maxLength">: T.maybe $ gql "MaxLength",
    "notNull">: T.boolean]

varcharType :: Binding
varcharType = def "VarcharType" $
  T.record [
    "maxLength">: T.maybe $ gql "MaxLength",
    "notNull">: T.boolean]

variableScopeClause :: Binding
variableScopeClause = def "VariableScopeClause" $
  T.maybe $ gql "BindingVariableReferenceList"

verboseBinaryExactNumericType :: Binding
verboseBinaryExactNumericType = def "VerboseBinaryExactNumericType" $
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

whenOperand :: Binding
whenOperand = def "WhenOperand" $
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

whenOperandList :: Binding
whenOperandList = def "WhenOperandList" $
  nonemptyList $ gql "WhenOperand"

whereClause :: Binding
whereClause = def "WhereClause" $
  gql "SearchCondition"

yieldClause :: Binding
yieldClause = def "YieldClause" $
  gql "YieldItemList"

yieldItem :: Binding
yieldItem = def "YieldItem" $
  T.record [
    "name">: gql "YieldItemName",
    "alias">: T.maybe $ gql "YieldItemAlias"]

yieldItemAlias :: Binding
yieldItemAlias = def "YieldItemAlias" $
  gql "BindingVariable"

yieldItemList :: Binding
yieldItemList = def "YieldItemList" $
  nonemptyList $ gql "YieldItem"

yieldItemName :: Binding
yieldItemName = def "YieldItemName" $
  gql "FieldName"

zonedDatetimeType :: Binding
zonedDatetimeType = def "ZonedDatetimeType" $
  T.record [
    "notNull">: T.boolean]

zonedTimeType :: Binding
zonedTimeType = def "ZonedTimeType" $
  T.record [
    "notNull">: T.boolean]
