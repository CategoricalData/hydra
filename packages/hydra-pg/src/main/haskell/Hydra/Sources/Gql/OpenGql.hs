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
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A GQL model based on the OpenGQL ANTLR grammar, version 15b256b (2024-09-05), available at:"
    ++ " https://github.com/opengql/grammar/blob/main/GQL.g4"))}
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

abbreviatedEdgePattern :: TypeDefinition
abbreviatedEdgePattern = def "AbbreviatedEdgePattern" $
  T.enum ["leftArrow", "tilde", "rightArrow", "leftArrowTilde", "tildeRightArrow", "leftMinusRight", "minusSign"]

absoluteCatalogSchemaReference :: TypeDefinition
absoluteCatalogSchemaReference = def "AbsoluteCatalogSchemaReference" $
  T.union [
    "root">: T.unit,
    "directoryAndSchema">: gql "AbsoluteDirectoryAndSchema"]

absoluteDirectoryAndSchema :: TypeDefinition
absoluteDirectoryAndSchema = def "AbsoluteDirectoryAndSchema" $
  T.record [
    "directoryPath">: gql "AbsoluteDirectoryPath",
    "schemaName">: gql "SchemaName"]

absoluteDirectoryPath :: TypeDefinition
absoluteDirectoryPath = def "AbsoluteDirectoryPath" $
  T.optional $ gql "SimpleDirectoryPath"

absoluteValueExpression :: TypeDefinition
absoluteValueExpression = def "AbsoluteValueExpression" $
  gql "ValueExpression"

addSubNumericValueExpression :: TypeDefinition
addSubNumericValueExpression = def "AddSubNumericValueExpression" $
  T.record [
    "left">: gql "NumericValueExpression",
    "operator">: gql "AddSubtractOperator",
    "right">: gql "NumericValueExpression"]

addSubtractExpr :: TypeDefinition
addSubtractExpr = def "AddSubtractExpr" $
  T.record [
    "left">: gql "ValueExpression",
    "operator">: gql "AddSubtractOperator",
    "right">: gql "ValueExpression"]

addSubtractOperator :: TypeDefinition
addSubtractOperator = def "AddSubtractOperator" $
  T.enum ["add", "subtract"]

aggregateFunction :: TypeDefinition
aggregateFunction = def "AggregateFunction" $
  T.union [
    "countAll">: T.unit,
    "generalSetFunction">: gql "GeneralSetFunction",
    "binarySetFunction">: gql "BinarySetFunction"]

aggregatingValueExpression :: TypeDefinition
aggregatingValueExpression = def "AggregatingValueExpression" $
  gql "ValueExpression"

allDifferentPredicate :: TypeDefinition
allDifferentPredicate = def "AllDifferentPredicate" $
  T.record [
    "references">: nonemptyList $ gql "ElementVariableReference"]

allParametersOrCharacteristics :: TypeDefinition
allParametersOrCharacteristics = def "AllParametersOrCharacteristics" $
  T.record [
    "all">: T.boolean,
    "type">: gql "ParametersOrCharacteristics"]

allPathSearch :: TypeDefinition
allPathSearch = def "AllPathSearch" $
  T.record [
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

allShortestPathSearch :: TypeDefinition
allShortestPathSearch = def "AllShortestPathSearch" $
  T.record [
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

ambientLinearDataModifyingStatement :: TypeDefinition
ambientLinearDataModifyingStatement = def "AmbientLinearDataModifyingStatement" $
  T.union [
    "simple">: gql "AmbientLinearDataModifyingStatementBody",
    "nested">: gql "NestedDataModifyingProcedureSpecification"]

ambientLinearDataModifyingStatementBody :: TypeDefinition
ambientLinearDataModifyingStatementBody = def "AmbientLinearDataModifyingStatementBody" $
  T.record [
    "simpleAccess">: gql "SimpleLinearDataAccessingStatement",
    "primitiveResult">: T.optional $ gql "PrimitiveResultStatement"]

ambientLinearQueryStatement :: TypeDefinition
ambientLinearQueryStatement = def "AmbientLinearQueryStatement" $
  T.union [
    "simple">: gql "AmbientLinearQueryStatementSimpleAndPrimitiveResult",
    "nested">: gql "NestedQuerySpecification"]

ambientLinearQueryStatementSimpleAndPrimitiveResult :: TypeDefinition
ambientLinearQueryStatementSimpleAndPrimitiveResult = def "AmbientLinearQueryStatementSimpleAndPrimitiveResult" $
  T.record [
    "simple">: T.optional $ gql "SimpleLinearQueryStatement",
    "primitiveResult">: gql "PrimitiveResultStatement"]

anyPathSearch :: TypeDefinition
anyPathSearch = def "AnyPathSearch" $
  T.record [
    "numberOfPaths">: T.optional $ gql "NumberOfPaths",
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

anyRecordType :: TypeDefinition
anyRecordType = def "AnyRecordType" $
  T.record [
    "any">: T.boolean,
    "notNull">: T.boolean]

anyShortestPathSearch :: TypeDefinition
anyShortestPathSearch = def "AnyShortestPathSearch" $
  T.record [
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

approximateNumericLiteral :: TypeDefinition
approximateNumericLiteral = def "ApproximateNumericLiteral" $
  T.union [
    "scientificWithSuffix">: T.string,
    "scientificWithoutSuffix">: T.string,
    "commonWithSuffix">: T.string,
    "integerWithSuffix">: T.string]

approximateNumericType :: TypeDefinition
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

arcTypePointingLeft :: TypeDefinition
arcTypePointingLeft = def "ArcTypePointingLeft" $
  gql "EdgeTypeFiller"

arcTypePointingRight :: TypeDefinition
arcTypePointingRight = def "ArcTypePointingRight" $
  gql "EdgeTypeFiller"

arcTypeUndirected :: TypeDefinition
arcTypeUndirected = def "ArcTypeUndirected" $
  gql "EdgeTypeFiller"

atSchemaClause :: TypeDefinition
atSchemaClause = def "AtSchemaClause" $
  gql "SchemaReference"

bigIntType :: TypeDefinition
bigIntType = def "BigIntType" $
  T.record ["notNull">: T.boolean]

bigIntegerType :: TypeDefinition
bigIntegerType = def "BigIntegerType" $
  T.record ["notNull">: T.boolean]

binaryExactNumericType :: TypeDefinition
binaryExactNumericType = def "BinaryExactNumericType" $
  T.union [
    "signed">: gql "SignedBinaryExactNumericType",
    "unsigned">: gql "UnsignedBinaryExactNumericType"]

binarySetFunction :: TypeDefinition
binarySetFunction = def "BinarySetFunction" $
  T.record [
    "functionType">: gql "BinarySetFunctionType",
    "dependentValue">: gql "DependentValueExpression",
    "independentValue">: gql "IndependentValueExpression"]

binarySetFunctionType :: TypeDefinition
binarySetFunctionType = def "BinarySetFunctionType" $
  T.enum ["percentileCont", "percentileDisc"]

binaryType :: TypeDefinition
binaryType = def "BinaryType" $
  T.record [
    "fixedLength">: T.optional $ gql "FixedLength",
    "notNull">: T.boolean]

bindingEqualsValue :: TypeDefinition
bindingEqualsValue = def "BindingEqualsValue" $
  T.record [
    "binding">: gql "BindingVariable",
    "value">: gql "ValueExpression"]

bindingTableExpression :: TypeDefinition
bindingTableExpression = def "BindingTableExpression" $
  T.union [
    "nested">: gql "NestedBindingTableQuerySpecification",
    "object">: gql "ObjectExpressionPrimary",
    "table">: gql "BindingTableReference",
    "name">: gql "ObjectNameOrBindingVariable"]

bindingTableInitializer :: TypeDefinition
bindingTableInitializer = def "BindingTableInitializer" T.unit

bindingTableName :: TypeDefinition
bindingTableName = def "BindingTableName" $
  T.union [
    "regularIdentifier">: T.string,
    "delimitedBindingTableName">: gql "DelimitedBindingTableName"]

bindingTableReference :: TypeDefinition
bindingTableReference = def "BindingTableReference" $
  T.union [
    "parentAndTableName">: gql "ParentAndTableName",
    "delimitedBindingTableName">: gql "DelimitedBindingTableName",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

bindingTableReferenceValueType :: TypeDefinition
bindingTableReferenceValueType = def "BindingTableReferenceValueType" $
  T.record [
    "bindingTableType">: gql "BindingTableType",
    "notNull">: T.boolean]

bindingTableType :: TypeDefinition
bindingTableType = def "BindingTableType" $
  T.record [
    "binding">: T.boolean,
    "fieldTypes">: gql "FieldTypesSpecification"]

bindingTableVariableDefinition :: TypeDefinition
bindingTableVariableDefinition = def "BindingTableVariableDefinition" $
  T.record [
    "binding">: T.boolean,
    "variable">: gql "BindingVariable",
    "initializer">: gql "OptTypedBindingTableInitializer"]

bindingVariable :: TypeDefinition
bindingVariable = def "BindingVariable" $
  T.string

bindingVariableDefinition :: TypeDefinition
bindingVariableDefinition = def "BindingVariableDefinition" $
  T.union [
    "graph">: gql "GraphVariableDefinition",
    "table">: gql "BindingTableVariableDefinition",
    "value">: gql "ValueVariableDefinition"]

bindingVariableDefinitionBlock :: TypeDefinition
bindingVariableDefinitionBlock = def "BindingVariableDefinitionBlock" $
  nonemptyList $ gql "BindingVariableDefinition"

bindingVariableReference :: TypeDefinition
bindingVariableReference = def "BindingVariableReference" $
  gql "BindingVariable"

bindingVariableReferenceList :: TypeDefinition
bindingVariableReferenceList = def "BindingVariableReferenceList" $
  nonemptyList $ gql "BindingVariableReference"

booleanLiteral :: TypeDefinition
booleanLiteral = def "BooleanLiteral" $
  T.enum ["true", "false", "unknown"]

booleanType :: TypeDefinition
booleanType = def "BooleanType" $
  T.record [
    "notNull">: T.boolean]

booleanValueExpression :: TypeDefinition
booleanValueExpression = def "BooleanValueExpression" $
  gql "ValueExpression"

byteLengthExpression :: TypeDefinition
byteLengthExpression = def "ByteLengthExpression" $
  gql "ByteStringValueExpression"

byteStringLiteral :: TypeDefinition
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

byteStringType :: TypeDefinition
byteStringType = def "ByteStringType" $
  T.union [
    "bytesType">: gql "BytesType",
    "binaryType">: gql "BinaryType",
    "varbinaryType">: gql "VarbinaryType"]

byteStringValueExpression :: TypeDefinition
byteStringValueExpression = def "ByteStringValueExpression" $
  gql "ValueExpression"

bytesType :: TypeDefinition
bytesType = def "BytesType" $
  T.record [
    "minLength">: T.optional $ gql "MinLength",
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

callCatalogModifyingProcedureStatement :: TypeDefinition
callCatalogModifyingProcedureStatement = def "CallCatalogModifyingProcedureStatement" $
  gql "CallProcedureStatement"

callDataModifyingProcedureStatement :: TypeDefinition
callDataModifyingProcedureStatement = def "CallDataModifyingProcedureStatement" $
  gql "CallProcedureStatement"

callProcedureStatement :: TypeDefinition
callProcedureStatement = def "CallProcedureStatement" $
  T.record [
    "optional">: T.boolean,
    "call">: gql "ProcedureCall"]

callQueryStatement :: TypeDefinition
callQueryStatement = def "CallQueryStatement" $
  gql "CallProcedureStatement"

cardinalityArgumentExpression :: TypeDefinition
cardinalityArgumentExpression = def "CardinalityArgumentExpression" $
  gql "ValueExpression"

cardinalityExpression :: TypeDefinition
cardinalityExpression = def "CardinalityExpression" $
  T.union [
    "cardinality">: gql "CardinalityArgumentExpression",
    "size">: gql "ListValueExpression"]

caseAbbreviation :: TypeDefinition
caseAbbreviation = def "CaseAbbreviation" $
  T.union [
    "nullIf">: gql "NullIfAbbreviation",
    "coalesce">: nonemptyList $ gql "ValueExpression"]

caseExpression :: TypeDefinition
caseExpression = def "CaseExpression" $
  T.union [
    "abbreviation">: gql "CaseAbbreviation",
    "specification">: gql "CaseSpecification"]

caseOperand :: TypeDefinition
caseOperand = def "CaseOperand" $
  T.union [
    "valueExpression">: gql "NonParenthesizedPrimaryValueExpression",
    "elementReference">: gql "ElementVariableReference"]

caseSpecification :: TypeDefinition
caseSpecification = def "CaseSpecification" $
  T.union [
    "simple">: gql "SimpleCase",
    "searched">: gql "SearchedCase"]

case_ :: TypeDefinition
case_ = def "Case" $
  T.enum ["upper", "lower"]

castOperand :: TypeDefinition
castOperand = def "CastOperand" $
  T.union [
    "valueExpression">: gql "ValueExpression",
    "nullLiteral">: T.unit]

castSpecification :: TypeDefinition
castSpecification = def "CastSpecification" $
  T.record [
    "operand">: gql "CastOperand",
    "target">: gql "CastTarget"]

castTarget :: TypeDefinition
castTarget = def "CastTarget" $
  gql "ValueType"

catalogGraphParentAndName :: TypeDefinition
catalogGraphParentAndName = def "CatalogGraphParentAndName" $
  T.record [
    "parentReference">: T.optional $ gql "CatalogObjectParentReference",
    "graphName">: gql "GraphName"]

catalogGraphTypeParentAndName :: TypeDefinition
catalogGraphTypeParentAndName = def "CatalogGraphTypeParentAndName" $
  T.record [
    "parentReference">: T.optional $ gql "CatalogObjectParentReference",
    "graphTypeName">: gql "GraphTypeName"]

catalogObjectParentReference :: TypeDefinition
catalogObjectParentReference = def "CatalogObjectParentReference" $
  T.union [
    "schemaAndObjects">: gql "SchemaAndObjects",
    "objectsOnly">: nonemptyList $ gql "ObjectName"]

catalogProcedureParentAndName :: TypeDefinition
catalogProcedureParentAndName = def "CatalogProcedureParentAndName" $
  T.record [
    "parentReference">: T.optional $ gql "CatalogObjectParentReference",
    "procedureName">: gql "ProcedureName"]

catalogSchemaParentAndName :: TypeDefinition
catalogSchemaParentAndName = def "CatalogSchemaParentAndName" $
  gql "AbsoluteDirectoryAndSchema"

ceilingFunction :: TypeDefinition
ceilingFunction = def "CeilingFunction" $
  gql "NumericValueExpression"

charLengthExpression :: TypeDefinition
charLengthExpression = def "CharLengthExpression" $
  gql "CharacterStringValueExpression"

charType :: TypeDefinition
charType = def "CharType" $
  T.record [
    "fixedLength">: T.optional $ gql "FixedLength",
    "notNull">: T.boolean]

characterOrByteStringFunction :: TypeDefinition
characterOrByteStringFunction = def "CharacterOrByteStringFunction" $
  T.union [
    "sub">: gql "SubCharacterOrByteString",
    "trimSingle">: gql "TrimSingleCharacterOrByteString",
    "fold">: gql "FoldCharacterString",
    "trimMultiCharacter">: gql "TrimMultiCharacterCharacterString",
    "normalize">: gql "NormalizeCharacterString"]

characterStringLiteral :: TypeDefinition
characterStringLiteral = def "CharacterStringLiteral" $
  T.string

characterStringType :: TypeDefinition
characterStringType = def "CharacterStringType" $
  T.union [
    "stringType">: gql "StringType",
    "charType">: gql "CharType",
    "varcharType">: gql "VarcharType"]

characterStringValueExpression :: TypeDefinition
characterStringValueExpression = def "CharacterStringValueExpression" $
  gql "ValueExpression"

closedDynamicUnionTypeAlt1 :: TypeDefinition
closedDynamicUnionTypeAlt1 = def "ClosedDynamicUnionTypeAlt1" $
  T.record [
    "anyValue">: T.optional T.boolean,
    "valueTypes">: nonemptyList $ gql "ValueType"]

closedDynamicUnionTypeAlt2 :: TypeDefinition
closedDynamicUnionTypeAlt2 = def "ClosedDynamicUnionTypeAlt2" $
  T.record [
    "valueTypes">: nonemptyList $ gql "ValueType"]

closedEdgeReferenceValueType :: TypeDefinition
closedEdgeReferenceValueType = def "ClosedEdgeReferenceValueType" $
  T.record [
    "edgeTypeSpec">: gql "EdgeTypeSpecification",
    "notNull">: T.boolean]

closedGraphReferenceValueType :: TypeDefinition
closedGraphReferenceValueType = def "ClosedGraphReferenceValueType" $
  T.record [
    "property">: T.boolean,
    "nestedSpec">: gql "NestedGraphTypeSpecification",
    "notNull">: T.boolean]

closedNodeReferenceValueType :: TypeDefinition
closedNodeReferenceValueType = def "ClosedNodeReferenceValueType" $
  T.record [
    "nodeTypeSpec">: gql "NodeTypeSpecification",
    "notNull">: T.boolean]

commitCommand :: TypeDefinition
commitCommand = def "CommitCommand" $
  T.unit

commonLogarithm :: TypeDefinition
commonLogarithm = def "CommonLogarithm" $
  gql "NumericValueExpression"

compOp :: TypeDefinition
compOp = def "CompOp" $
  T.enum [
    "equals",
    "notEquals",
    "lessThan",
    "greaterThan",
    "lessThanOrEquals",
    "greaterThanOrEquals"]

comparisonExpr :: TypeDefinition
comparisonExpr = def "ComparisonExpr" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "comparison">: gql "ComparisonPredicatePart2"]

comparisonPredicatePart2 :: TypeDefinition
comparisonPredicatePart2 = def "ComparisonPredicatePart2" $
  T.record [
    "compOp">: gql "CompOp",
    "valueExpression">: gql "ValueExpression"]

compositeQueryExpression :: TypeDefinition
compositeQueryExpression = def "CompositeQueryExpression" $
  T.union [
    "simple">: gql "CompositeQueryExpressionConjunction",
    "primary">: gql "CompositeQueryPrimary"]

compositeQueryExpressionConjunction :: TypeDefinition
compositeQueryExpressionConjunction = def "CompositeQueryExpressionConjunction" $
  T.record [
    "left">: gql "CompositeQueryExpression",
    "conjunction">: gql "QueryConjunction",
    "right">: gql "CompositeQueryPrimary"]

compositeQueryPrimary :: TypeDefinition
compositeQueryPrimary = def "CompositeQueryPrimary" $
  gql "LinearQueryStatement"

compositeQueryStatement :: TypeDefinition
compositeQueryStatement = def "CompositeQueryStatement" $
  gql "CompositeQueryExpression"

concatenationExpr :: TypeDefinition
concatenationExpr = def "ConcatenationExpr" $
  T.record [
    "left">: gql "ValueExpression",
    "right">: gql "ValueExpression"]

conjunctionLabelExpression :: TypeDefinition
conjunctionLabelExpression = def "ConjunctionLabelExpression" $
  T.record [
    "left">: gql "LabelExpression",
    "right">: gql "LabelExpression"]

conjunctiveExpr :: TypeDefinition
conjunctiveExpr = def "ConjunctiveExpr" $
  T.record [
    "left">: gql "ValueExpression",
    "right">: gql "ValueExpression"]

connectorPointingRight :: TypeDefinition
connectorPointingRight = def "ConnectorPointingRight" $
  T.enum ["to", "rightArrow"]

connectorUndirected :: TypeDefinition
connectorUndirected = def "ConnectorUndirected" $
  T.enum ["to", "tilde"]

copyOfGraphType :: TypeDefinition
copyOfGraphType = def "CopyOfGraphType" $
  gql "GraphTypeReference"

countedShortestGroupSearch :: TypeDefinition
countedShortestGroupSearch = def "CountedShortestGroupSearch" $
  T.record [
    "numberOfGroups">: T.optional $ gql "NumberOfGroups",
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths",
    "groups">: T.boolean]

countedShortestPathSearch :: TypeDefinition
countedShortestPathSearch = def "CountedShortestPathSearch" $
  T.record [
    "numberOfPaths">: gql "NumberOfPaths",
    "mode">: T.optional $ gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

createGraphOption :: TypeDefinition
createGraphOption = def "CreateGraphOption" $
  T.union [
    "graphIfNotExists">: T.boolean,
    "orReplace">: T.unit]

createGraphStatement :: TypeDefinition
createGraphStatement = def "CreateGraphStatement" $
  T.record [
    "createOption">: gql "CreateGraphOption",
    "parentAndName">: gql "CatalogGraphParentAndName",
    "type">: gql "GraphTypeOption",
    "source">: T.optional $ gql "GraphSource"]

createGraphTypeOption :: TypeDefinition
createGraphTypeOption = def "CreateGraphTypeOption" $
  T.union [
    "typeIfNotExists">: T.boolean,
    "orReplace">: T.unit]

createGraphTypeStatement :: TypeDefinition
createGraphTypeStatement = def "CreateGraphTypeStatement" $
  T.record [
    "createOption">: gql "CreateGraphTypeOption",
    "parentAndName">: gql "CatalogGraphTypeParentAndName",
    "source">: gql "GraphTypeSource"]

createSchemaStatement :: TypeDefinition
createSchemaStatement = def "CreateSchemaStatement" $
  T.record [
    "ifNotExists">: T.boolean,
    "parentAndName">: gql "CatalogSchemaParentAndName"]

currentGraph :: TypeDefinition
currentGraph = def "CurrentGraph" $
  T.enum ["graph", "propertyGraph"]

dateFunction :: TypeDefinition
dateFunction = def "DateFunction" $
  T.union [
    "currentDate">: T.unit,
    "dateWithParams">: T.optional $ gql "DateFunctionParameters"]

dateFunctionParameters :: TypeDefinition
dateFunctionParameters = def "DateFunctionParameters" $
  T.union [
    "dateString">: gql "DateString",
    "recordConstructor">: gql "RecordConstructor"]

dateLiteral :: TypeDefinition
dateLiteral = def "DateLiteral" $
  gql "DateString"

dateString :: TypeDefinition
dateString = def "DateString" $
  gql "CharacterStringLiteral"

dateType :: TypeDefinition
dateType = def "DateType" $
  T.record [
    "notNull">: T.boolean]

datetimeFunction :: TypeDefinition
datetimeFunction = def "DatetimeFunction" $
  T.union [
    "currentTimestamp">: T.unit,
    "zonedDatetimeWithParams">: T.optional $ gql "DatetimeFunctionParameters"]

datetimeFunctionParameters :: TypeDefinition
datetimeFunctionParameters = def "DatetimeFunctionParameters" $
  T.union [
    "datetimeString">: gql "DatetimeString",
    "recordConstructor">: gql "RecordConstructor"]

datetimeLiteral :: TypeDefinition
datetimeLiteral = def "DatetimeLiteral" $
  gql "DatetimeString"

datetimeString :: TypeDefinition
datetimeString = def "DatetimeString" $
  gql "CharacterStringLiteral"

datetimeSubtraction :: TypeDefinition
datetimeSubtraction = def "DatetimeSubtraction" $
  T.record [
    "parameters">: gql "DatetimeSubtractionParameters",
    "temporalDurationQualifier">: T.optional $ gql "TemporalDurationQualifier"]

datetimeSubtractionParameters :: TypeDefinition
datetimeSubtractionParameters = def "DatetimeSubtractionParameters" $
  T.record [
    "expression1">: gql "DatetimeValueExpression1",
    "expression2">: gql "DatetimeValueExpression2"]

datetimeType :: TypeDefinition
datetimeType = def "DatetimeType" $
  T.union [
    "zonedDatetime">: gql "ZonedDatetimeType",
    "timestampWithTimeZone">: gql "TimestampWithTimeZoneType"]

datetimeValueExpression :: TypeDefinition
datetimeValueExpression = def "DatetimeValueExpression" $
  gql "ValueExpression"

datetimeValueExpression1 :: TypeDefinition
datetimeValueExpression1 = def "DatetimeValueExpression1" $
  gql "DatetimeValueExpression"

datetimeValueExpression2 :: TypeDefinition
datetimeValueExpression2 = def "DatetimeValueExpression2" $
  gql "DatetimeValueExpression"

datetimeValueFunction :: TypeDefinition
datetimeValueFunction = def "DatetimeValueFunction" $
  T.union [
    "dateFunction">: gql "DateFunction",
    "timeFunction">: gql "TimeFunction",
    "datetimeFunction">: gql "DatetimeFunction",
    "localtimeFunction">: gql "LocaltimeFunction",
    "localdatetimeFunction">: gql "LocaldatetimeFunction"]

decimalExactNumericType :: TypeDefinition
decimalExactNumericType = def "DecimalExactNumericType" $
  T.optional $ gql "PrecisionAndScale"

def :: String -> Type -> TypeDefinition
def = datatype ns

deleteItem :: TypeDefinition
deleteItem = def "DeleteItem" $
  gql "ValueExpression"

deleteItemList :: TypeDefinition
deleteItemList = def "DeleteItemList" $
  nonemptyList $ gql "DeleteItem"

deleteStatement :: TypeDefinition
deleteStatement = def "DeleteStatement" $
  T.record [
    "detach">: T.optional $ gql "DetachOption",
    "items">: gql "DeleteItemList"]

delimitedBindingTableName :: TypeDefinition
delimitedBindingTableName = def "DelimitedBindingTableName" $
  T.string

delimitedGraphName :: TypeDefinition
delimitedGraphName = def "DelimitedGraphName" $
  T.string

dependentValueExpression :: TypeDefinition
dependentValueExpression = def "DependentValueExpression" $
  T.record [
    "setQuantifier">: T.optional $ gql "SetQuantifier",
    "numericValue">: gql "NumericValueExpression"]

destinationNodeTypeAlias :: TypeDefinition
destinationNodeTypeAlias = def "DestinationNodeTypeAlias" $
  T.string

destinationNodeTypeReference :: TypeDefinition
destinationNodeTypeReference = def "DestinationNodeTypeReference" $
  T.union [
    "alias">: gql "DestinationNodeTypeAlias",
    "filler">: T.optional $ gql "NodeTypeFiller"]

destinationPredicate :: TypeDefinition
destinationPredicate = def "DestinationPredicate" $
  T.record [
    "nodeReference">: gql "NodeReference",
    "not">: T.boolean,
    "destinationOf">: gql "EdgeReference"]

detachOption :: TypeDefinition
detachOption = def "DetachOption" $
  T.enum [
    "detach",
    "noDetach"]

differentEdgesMatchMode :: TypeDefinition
differentEdgesMatchMode = def "DifferentEdgesMatchMode" $
  gql "EdgeBindingsOrEdges"

directedPredicate :: TypeDefinition
directedPredicate = def "DirectedPredicate" $
  T.record [
    "elementVariableReference">: gql "ElementVariableReference",
    "directedPart">: gql "DirectedPredicatePart2"]

directedPredicatePart2 :: TypeDefinition
directedPredicatePart2 = def "DirectedPredicatePart2" $
  T.record [
    "not">: T.boolean]

directoryName :: TypeDefinition
directoryName = def "DirectoryName" $
  T.string

disjunctionLabelExpression :: TypeDefinition
disjunctionLabelExpression = def "DisjunctionLabelExpression" $
  T.record [
    "left">: gql "LabelExpression",
    "right">: gql "LabelExpression"]

disjunctiveExpr :: TypeDefinition
disjunctiveExpr = def "DisjunctiveExpr" $
  T.record [
    "left">: gql "ValueExpression",
    "operator">: gql "DisjunctiveOperator",
    "right">: gql "ValueExpression"]

disjunctiveOperator :: TypeDefinition
disjunctiveOperator = def "DisjunctiveOperator" $
  T.enum ["or", "xor"]

doubleTypeWithPrecision :: TypeDefinition
doubleTypeWithPrecision = def "DoubleTypeWithPrecision" $
  T.record [
    "precision">: T.boolean,
    "notNull">: T.boolean]

dropGraphStatement :: TypeDefinition
dropGraphStatement = def "DropGraphStatement" $
  T.record [
    "ifExists">: T.boolean,
    "parentAndName">: gql "CatalogGraphParentAndName"]

dropGraphTypeStatement :: TypeDefinition
dropGraphTypeStatement = def "DropGraphTypeStatement" $
  T.record [
    "ifExists">: T.boolean,
    "parentAndName">: gql "CatalogGraphTypeParentAndName"]

dropSchemaStatement :: TypeDefinition
dropSchemaStatement = def "DropSchemaStatement" $
  T.record [
    "ifExists">: T.boolean,
    "parentAndName">: gql "CatalogSchemaParentAndName"]

durationFunction :: TypeDefinition
durationFunction = def "DurationFunction" $
  gql "DurationFunctionParameters"

durationFunctionParameters :: TypeDefinition
durationFunctionParameters = def "DurationFunctionParameters" $
  T.union [
    "durationString">: gql "DurationString",
    "recordConstructor">: gql "RecordConstructor"]

durationLiteral :: TypeDefinition
durationLiteral = def "DurationLiteral" $
  gql "DurationString"

durationString :: TypeDefinition
durationString = def "DurationString" $
  gql "CharacterStringLiteral"

durationValueExpression :: TypeDefinition
durationValueExpression = def "DurationValueExpression" $
  gql "ValueExpression"

durationValueFunction :: TypeDefinition
durationValueFunction = def "DurationValueFunction" $
  T.union [
    "durationFunction">: gql "DurationFunction",
    "absoluteValue">: gql "AbsoluteValueExpression"]

dynamicParameterSpecification :: TypeDefinition
dynamicParameterSpecification = def "DynamicParameterSpecification" $
  gql "ParameterName"

dynamicPropertyValueType :: TypeDefinition
dynamicPropertyValueType = def "DynamicPropertyValueType" $
  T.record [
    "any">: T.optional T.boolean,
    "property">: T.unit,
    "value">: T.unit,
    "notNull">: T.boolean]

edgeBindingsOrEdges :: TypeDefinition
edgeBindingsOrEdges = def "EdgeBindingsOrEdges" $
  T.union [
    "edgeBindings">: T.boolean,
    "edges">: T.unit]

edgeKeyLabelSetWithContent :: TypeDefinition
edgeKeyLabelSetWithContent = def "EdgeKeyLabelSetWithContent" $
  T.record [
    "keyLabelSet">: gql "EdgeTypeKeyLabelSet",
    "impliedContent">: T.optional $ gql "EdgeTypeImpliedContent"]

edgeKind :: TypeDefinition
edgeKind = def "EdgeKind" $
  T.enum ["directed", "undirected"]

edgeKindAndSynonym :: TypeDefinition
edgeKindAndSynonym = def "EdgeKindAndSynonym" $
  T.record [
    "kind">: T.optional $ gql "EdgeKind",
    "synonym">: gql "EdgeSynonym",
    "typeName">: T.optional $ gql "EdgeTypeName"]

edgeLabelSetWithProperties :: TypeDefinition
edgeLabelSetWithProperties = def "EdgeLabelSetWithProperties" $
  T.record [
    "labelSet">: gql "EdgeTypeLabelSet",
    "propertyTypes">: gql "EdgeTypePropertyTypes"]

edgePattern :: TypeDefinition
edgePattern = def "EdgePattern" $
  T.union [
    "fullEdge">: gql "FullEdgePattern",
    "abbreviatedEdge">: gql "AbbreviatedEdgePattern"]

edgeReference :: TypeDefinition
edgeReference = def "EdgeReference" $
  gql "ElementVariableReference"

edgeReferenceValueExpression :: TypeDefinition
edgeReferenceValueExpression = def "EdgeReferenceValueExpression" $
  gql "PrimaryValueExpression"

edgeReferenceValueType :: TypeDefinition
edgeReferenceValueType = def "EdgeReferenceValueType" $
  T.union [
    "open">: gql "OpenEdgeReferenceValueType",
    "closed">: gql "ClosedEdgeReferenceValueType"]

edgeSynonym :: TypeDefinition
edgeSynonym = def "EdgeSynonym" $
  T.enum ["edge", "relationship"]

edgeTypeFiller :: TypeDefinition
edgeTypeFiller = def "EdgeTypeFiller" $
  T.union [
    "keyLabelSetWithContent">: gql "EdgeKeyLabelSetWithContent",
    "impliedContent">: gql "EdgeTypeImpliedContent"]

edgeTypeImpliedContent :: TypeDefinition
edgeTypeImpliedContent = def "EdgeTypeImpliedContent" $
  T.union [
    "labelSet">: gql "EdgeTypeLabelSet",
    "propertyTypes">: gql "EdgeTypePropertyTypes",
    "labelSetWithProperties">: gql "EdgeLabelSetWithProperties"]

edgeTypeKeyLabelSet :: TypeDefinition
edgeTypeKeyLabelSet = def "EdgeTypeKeyLabelSet" $
  T.optional $ gql "LabelSetPhrase"

edgeTypeLabelSet :: TypeDefinition
edgeTypeLabelSet = def "EdgeTypeLabelSet" $
  gql "LabelSetPhrase"

edgeTypeName :: TypeDefinition
edgeTypeName = def "EdgeTypeName" $
  T.string

edgeTypeNameWithFiller :: TypeDefinition
edgeTypeNameWithFiller = def "EdgeTypeNameWithFiller" $
  T.record [
    "typeName">: gql "EdgeTypeName",
    "filler">: T.optional $ gql "EdgeTypeFiller"]

edgeTypePattern :: TypeDefinition
edgeTypePattern = def "EdgeTypePattern" $
  T.record [
    "kindAndSynonym">: T.optional $ gql "EdgeKindAndSynonym",
    "patternType">: gql "EdgeTypePatternType"]

edgeTypePatternDirected :: TypeDefinition
edgeTypePatternDirected = def "EdgeTypePatternDirected" $
  T.union [
    "pointingRight">: gql "EdgeTypePatternPointingRight",
    "pointingLeft">: gql "EdgeTypePatternPointingLeft"]

edgeTypePatternPointingLeft :: TypeDefinition
edgeTypePatternPointingLeft = def "EdgeTypePatternPointingLeft" $
  T.record [
    "destination">: gql "DestinationNodeTypeReference",
    "arc">: gql "ArcTypePointingLeft",
    "source">: gql "SourceNodeTypeReference"]

edgeTypePatternPointingRight :: TypeDefinition
edgeTypePatternPointingRight = def "EdgeTypePatternPointingRight" $
  T.record [
    "source">: gql "SourceNodeTypeReference",
    "arc">: gql "ArcTypePointingRight",
    "destination">: gql "DestinationNodeTypeReference"]

edgeTypePatternType :: TypeDefinition
edgeTypePatternType = def "EdgeTypePatternType" $
  T.union [
    "directed">: gql "EdgeTypePatternDirected",
    "undirected">: gql "EdgeTypePatternUndirected"]

edgeTypePatternUndirected :: TypeDefinition
edgeTypePatternUndirected = def "EdgeTypePatternUndirected" $
  T.record [
    "source">: gql "SourceNodeTypeReference",
    "arc">: gql "ArcTypeUndirected",
    "destination">: gql "DestinationNodeTypeReference"]

edgeTypePhrase :: TypeDefinition
edgeTypePhrase = def "EdgeTypePhrase" $
  T.record [
    "kind">: gql "EdgeKind",
    "synonym">: gql "EdgeSynonym",
    "typeNameAndFiller">: gql "EdgeTypePhraseFiller",
    "endpointPair">: gql "EndpointPairPhrase"]

edgeTypePhraseFiller :: TypeDefinition
edgeTypePhraseFiller = def "EdgeTypePhraseFiller" $
  T.union [
    "typeNameWithFiller">: gql "EdgeTypeNameWithFiller",
    "fillerOnly">: gql "EdgeTypeFiller"]

edgeTypePropertyTypes :: TypeDefinition
edgeTypePropertyTypes = def "EdgeTypePropertyTypes" $
  gql "PropertyTypesSpecification"

edgeTypeSpecification :: TypeDefinition
edgeTypeSpecification = def "EdgeTypeSpecification" $
  T.union [
    "pattern">: gql "EdgeTypePattern",
    "phrase">: gql "EdgeTypePhrase"]

edgesSynonym :: TypeDefinition
edgesSynonym = def "EdgesSynonym" $
  T.enum ["edges", "relationships"]

elementBindingsOrElements :: TypeDefinition
elementBindingsOrElements = def "ElementBindingsOrElements" $
  T.union [
    "elementBindings">: T.boolean,
    "elements">: T.unit]

elementIdFunction :: TypeDefinition
elementIdFunction = def "ElementIdFunction" $
  gql "ElementVariableReference"

elementPattern :: TypeDefinition
elementPattern = def "ElementPattern" $
  T.union [
    "node">: gql "NodePattern",
    "edge">: gql "EdgePattern"]

elementPatternFiller :: TypeDefinition
elementPatternFiller = def "ElementPatternFiller" $
  T.record [
    "variableDeclaration">: T.optional $ gql "ElementVariableDeclaration",
    "isLabelExpression">: T.optional $ gql "IsLabelExpression",
    "predicate">: T.optional $ gql "ElementPatternPredicate"]

elementPatternPredicate :: TypeDefinition
elementPatternPredicate = def "ElementPatternPredicate" $
  T.union [
    "whereClause">: gql "ElementPatternWhereClause",
    "propertySpecification">: gql "ElementPropertySpecification"]

elementPatternWhereClause :: TypeDefinition
elementPatternWhereClause = def "ElementPatternWhereClause" $
  gql "SearchCondition"

elementPropertySpecification :: TypeDefinition
elementPropertySpecification = def "ElementPropertySpecification" $
  gql "PropertyKeyValuePairList"

elementTypeList :: TypeDefinition
elementTypeList = def "ElementTypeList" $
  nonemptyList $ gql "ElementTypeSpecification"

elementTypeSpecification :: TypeDefinition
elementTypeSpecification = def "ElementTypeSpecification" $
  T.union [
    "nodeType">: gql "NodeTypeSpecification",
    "edgeType">: gql "EdgeTypeSpecification"]

elementVariable :: TypeDefinition
elementVariable = def "ElementVariable" $
  gql "BindingVariable"

elementVariableDeclaration :: TypeDefinition
elementVariableDeclaration = def "ElementVariableDeclaration" $
  T.record [
    "temp">: T.optional T.boolean,
    "variable">: gql "ElementVariable"]

elementVariableReference :: TypeDefinition
elementVariableReference = def "ElementVariableReference" $
  gql "BindingVariableReference"

elementsFunction :: TypeDefinition
elementsFunction = def "ElementsFunction" $
  gql "PathValueExpression"

elseClause :: TypeDefinition
elseClause = def "ElseClause" $
  gql "Result"

emptyType :: TypeDefinition
emptyType = def "EmptyType" $
  T.unit

endTransactionCommand :: TypeDefinition
endTransactionCommand = def "EndTransactionCommand" $
  T.union [
    "rollback">: gql "RollbackCommand",
    "commit">: gql "CommitCommand"]

endpointPair :: TypeDefinition
endpointPair = def "EndpointPair" $
  T.union [
    "directedPair">: gql "EndpointPairDirected",
    "undirectedPair">: gql "EndpointPairUndirected"]

endpointPairDirected :: TypeDefinition
endpointPairDirected = def "EndpointPairDirected" $
  T.union [
    "pointingRight">: gql "EndpointPairPointingRight",
    "pointingLeft">: gql "EndpointPairPointingLeft"]

endpointPairPhrase :: TypeDefinition
endpointPairPhrase = def "EndpointPairPhrase" $
  gql "EndpointPair"

endpointPairPointingLeft :: TypeDefinition
endpointPairPointingLeft = def "EndpointPairPointingLeft" $
  T.record [
    "destinationAlias">: gql "DestinationNodeTypeAlias",
    "sourceAlias">: gql "SourceNodeTypeAlias"]

endpointPairPointingRight :: TypeDefinition
endpointPairPointingRight = def "EndpointPairPointingRight" $
  T.record [
    "sourceAlias">: gql "SourceNodeTypeAlias",
    "connector">: gql "ConnectorPointingRight",
    "destinationAlias">: gql "DestinationNodeTypeAlias"]

endpointPairUndirected :: TypeDefinition
endpointPairUndirected = def "EndpointPairUndirected" $
  T.record [
    "sourceAlias">: gql "SourceNodeTypeAlias",
    "connector">: gql "ConnectorUndirected",
    "destinationAlias">: gql "DestinationNodeTypeAlias"]

exactNumericLiteral :: TypeDefinition
exactNumericLiteral = def "ExactNumericLiteral" $
  T.union [
    "scientificWithSuffix">: T.string,
    "commonWithSuffix">: T.string,
    "commonWithoutSuffix">: T.string,
    "integerWithSuffix">: T.string,
    "unsignedInteger">: gql "UnsignedInteger"]

exactNumericType :: TypeDefinition
exactNumericType = def "ExactNumericType" $
  T.union [
    "binary">: gql "BinaryExactNumericType",
    "decimal">: gql "DecimalExactNumericType"]

existsPredicate :: TypeDefinition
existsPredicate = def "ExistsPredicate" $
  T.union [
    "graphPatternBrace">: gql "GraphPattern",
    "graphPatternParen">: gql "GraphPattern",
    "matchBlockBrace">: gql "MatchStatementBlock",
    "matchBlockParen">: gql "MatchStatementBlock",
    "nestedQuery">: gql "NestedQuerySpecification"]

exponentialFunction :: TypeDefinition
exponentialFunction = def "ExponentialFunction" $
  gql "NumericValueExpression"

field :: TypeDefinition
field = def "Field" $
  T.record [
    "name">: gql "FieldName",
    "value">: gql "ValueExpression"]

fieldList :: TypeDefinition
fieldList = def "FieldList" $
  nonemptyList $ gql "Field"

fieldName_ :: TypeDefinition
fieldName_ = def "FieldName" $
  T.string

fieldType :: TypeDefinition
fieldType = def "FieldType" $
  T.record [
    "fieldName">: gql "FieldName",
    "typed">: T.optional $ gql "Typed",
    "valueType">: gql "ValueType"]

fieldTypeList :: TypeDefinition
fieldTypeList = def "FieldTypeList" $
  nonemptyList $ gql "FieldType"

fieldTypesSpecification :: TypeDefinition
fieldTypesSpecification = def "FieldTypesSpecification" $
  T.optional $ gql "FieldTypeList"

fieldsSpecification :: TypeDefinition
fieldsSpecification = def "FieldsSpecification" $
  T.optional $ gql "FieldList"

filterStatement :: TypeDefinition
filterStatement = def "FilterStatement" $
  T.union [
    "whereClause">: gql "WhereClause",
    "searchCondition">: gql "SearchCondition"]

fixedLength :: TypeDefinition
fixedLength = def "FixedLength" $
  gql "UnsignedInteger"

fixedQuantifier :: TypeDefinition
fixedQuantifier = def "FixedQuantifier" $
  gql "UnsignedInteger"

float128Type :: TypeDefinition
float128Type = def "Float128Type" $
  T.record [
    "notNull">: T.boolean]

float16Type :: TypeDefinition
float16Type = def "Float16Type" $
  T.record [
    "notNull">: T.boolean]

float256Type :: TypeDefinition
float256Type = def "Float256Type" $
  T.record [
    "notNull">: T.boolean]

float32Type :: TypeDefinition
float32Type = def "Float32Type" $
  T.record [
    "notNull">: T.boolean]

float64Type :: TypeDefinition
float64Type = def "Float64Type" $
  T.record [
    "notNull">: T.boolean]

floatTypeWithPrecision :: TypeDefinition
floatTypeWithPrecision = def "FloatTypeWithPrecision" $
  T.record [
    "precisionAndScale">: T.optional $ gql "PrecisionAndScale",
    "notNull">: T.boolean]

floorFunction :: TypeDefinition
floorFunction = def "FloorFunction" $
  gql "NumericValueExpression"

focusedLinearDataModifyingStatement :: TypeDefinition
focusedLinearDataModifyingStatement = def "FocusedLinearDataModifyingStatement" $
  T.union [
    "simple">: gql "FocusedLinearDataModifyingStatementBody",
    "nested">: gql "FocusedNestedDataModifyingProcedureSpecification"]

focusedLinearDataModifyingStatementBody :: TypeDefinition
focusedLinearDataModifyingStatementBody = def "FocusedLinearDataModifyingStatementBody" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "simpleAccess">: gql "SimpleLinearDataAccessingStatement",
    "primitiveResult">: T.optional $ gql "PrimitiveResultStatement"]

focusedLinearQueryAndPrimitiveResultStatementPart :: TypeDefinition
focusedLinearQueryAndPrimitiveResultStatementPart = def "FocusedLinearQueryAndPrimitiveResultStatementPart" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "simple">: gql "SimpleLinearQueryStatement",
    "primitiveResult">: gql "PrimitiveResultStatement"]

focusedLinearQueryStatement :: TypeDefinition
focusedLinearQueryStatement = def "FocusedLinearQueryStatement" $
  T.union [
    "parts">: gql "FocusedLinearQueryStatementPartsAndResult",
    "primitive">: gql "FocusedPrimitiveResultStatement",
    "nested">: gql "FocusedNestedQuerySpecification",
    "select">: gql "SelectStatement"]

focusedLinearQueryStatementPart :: TypeDefinition
focusedLinearQueryStatementPart = def "FocusedLinearQueryStatementPart" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "simple">: gql "SimpleLinearQueryStatement"]

focusedLinearQueryStatementPartsAndResult :: TypeDefinition
focusedLinearQueryStatementPartsAndResult = def "FocusedLinearQueryStatementPartsAndResult" $
  T.record [
    "parts">: T.list $ gql "FocusedLinearQueryStatementPart",
    "result">: gql "FocusedLinearQueryAndPrimitiveResultStatementPart"]

focusedNestedDataModifyingProcedureSpecification :: TypeDefinition
focusedNestedDataModifyingProcedureSpecification = def "FocusedNestedDataModifyingProcedureSpecification" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "nestedSpec">: gql "NestedDataModifyingProcedureSpecification"]

focusedNestedQuerySpecification :: TypeDefinition
focusedNestedQuerySpecification = def "FocusedNestedQuerySpecification" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "nested">: gql "NestedQuerySpecification"]

focusedPrimitiveResultStatement :: TypeDefinition
focusedPrimitiveResultStatement = def "FocusedPrimitiveResultStatement" $
  T.record [
    "useGraph">: gql "UseGraphClause",
    "primitiveResult">: gql "PrimitiveResultStatement"]

foldCharacterString :: TypeDefinition
foldCharacterString = def "FoldCharacterString" $
  T.record [
    "case">: gql "Case",
    "valueExpression">: gql "ValueExpression"]

forItem :: TypeDefinition
forItem = def "ForItem" $
  T.record [
    "alias">: gql "ForItemAlias",
    "source">: gql "ForItemSource"]

forItemAlias :: TypeDefinition
forItemAlias = def "ForItemAlias" $
  gql "BindingVariable"

forItemSource :: TypeDefinition
forItemSource = def "ForItemSource" $
  gql "ValueExpression"

forOrdinalityOrOffset :: TypeDefinition
forOrdinalityOrOffset = def "ForOrdinalityOrOffset" $
  T.record [
    "type">: gql "OrdinalityOrOffsetType",
    "variable">: gql "BindingVariable"]

forStatement :: TypeDefinition
forStatement = def "ForStatement" $
  T.record [
    "item">: gql "ForItem",
    "ordinalityOrOffset">: T.optional $ gql "ForOrdinalityOrOffset"]

fullEdgeAnyDirection :: TypeDefinition
fullEdgeAnyDirection = def "FullEdgeAnyDirection" $
  gql "ElementPatternFiller"

fullEdgeLeftOrRight :: TypeDefinition
fullEdgeLeftOrRight = def "FullEdgeLeftOrRight" $
  gql "ElementPatternFiller"

fullEdgeLeftOrUndirected :: TypeDefinition
fullEdgeLeftOrUndirected = def "FullEdgeLeftOrUndirected" $
  gql "ElementPatternFiller"

fullEdgePattern :: TypeDefinition
fullEdgePattern = def "FullEdgePattern" $
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
  gql "ElementPatternFiller"

fullEdgePointingRight :: TypeDefinition
fullEdgePointingRight = def "FullEdgePointingRight" $
  gql "ElementPatternFiller"

fullEdgeUndirected :: TypeDefinition
fullEdgeUndirected = def "FullEdgeUndirected" $
  gql "ElementPatternFiller"

fullEdgeUndirectedOrRight :: TypeDefinition
fullEdgeUndirectedOrRight = def "FullEdgeUndirectedOrRight" $
  gql "ElementPatternFiller"

generalLiteral :: TypeDefinition
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

generalLogarithmArgument :: TypeDefinition
generalLogarithmArgument = def "GeneralLogarithmArgument" $
  gql "NumericValueExpression"

generalLogarithmBase :: TypeDefinition
generalLogarithmBase = def "GeneralLogarithmBase" $
  gql "NumericValueExpression"

generalLogarithmFunction :: TypeDefinition
generalLogarithmFunction = def "GeneralLogarithmFunction" $
  T.record [
    "base">: gql "GeneralLogarithmBase",
    "argument">: gql "GeneralLogarithmArgument"]

generalQuantifier :: TypeDefinition
generalQuantifier = def "GeneralQuantifier" $
  T.record [
    "lowerBound">: T.optional $ gql "LowerBound",
    "upperBound">: T.optional $ gql "UpperBound"]

generalSetFunction :: TypeDefinition
generalSetFunction = def "GeneralSetFunction" $
  T.record [
    "functionType">: gql "GeneralSetFunctionType",
    "setQuantifier">: T.optional $ gql "SetQuantifier",
    "valueExpression">: gql "ValueExpression"]

generalSetFunctionType :: TypeDefinition
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

generalValueSpecification :: TypeDefinition
generalValueSpecification = def "GeneralValueSpecification" $
  T.union [
    "dynamicParameterSpecification">: gql "DynamicParameterSpecification",
    "sessionUser">: T.unit]

gql :: String -> Type
gql = typeref ns


gqlProgram :: TypeDefinition
gqlProgram = def "GqlProgram" $
  T.record [
    "activity">: T.optional $ gql "ProgramActivity",
    "close">: T.optional $ gql "SessionCloseCommand"]

graphAndNestedQuerySpecification :: TypeDefinition
graphAndNestedQuerySpecification = def "GraphAndNestedQuerySpecification" $
  T.record [
    "graphExpression">: gql "GraphExpression",
    "nested">: gql "NestedQuerySpecification"]

graphExpression :: TypeDefinition
graphExpression = def "GraphExpression" $
  T.union [
    "object">: gql "ObjectExpressionPrimary",
    "reference">: gql "GraphReference",
    "name">: gql "ObjectNameOrBindingVariable",
    "current">: gql "CurrentGraph"]

graphInitializer :: TypeDefinition
graphInitializer = def "GraphInitializer" T.unit

graphName :: TypeDefinition
graphName = def "GraphName" $
  T.string

graphPattern :: TypeDefinition
graphPattern = def "GraphPattern" $
  T.record [
    "matchMode">: T.optional $ gql "MatchMode",
    "pathPatterns">: gql "PathPatternList",
    "keepClause">: T.optional $ gql "KeepClause",
    "whereClause">: T.optional $ gql "GraphPatternWhereClause"]

graphPatternBindingTable :: TypeDefinition
graphPatternBindingTable = def "GraphPatternBindingTable" $
  T.record [
    "pattern">: gql "GraphPattern",
    "yieldClause">: T.optional $ gql "GraphPatternYieldClause"]

graphPatternQuantifier :: TypeDefinition
graphPatternQuantifier = def "GraphPatternQuantifier" $
  T.union [
    "asterisk">: T.unit,
    "plusSign">: T.unit,
    "fixed">: gql "FixedQuantifier",
    "general">: gql "GeneralQuantifier"]

graphPatternWhereClause :: TypeDefinition
graphPatternWhereClause = def "GraphPatternWhereClause" $
  gql "SearchCondition"

graphPatternYieldClause :: TypeDefinition
graphPatternYieldClause = def "GraphPatternYieldClause" $
  gql "GraphPatternYieldItemList"

graphPatternYieldItem :: TypeDefinition
graphPatternYieldItem = def "GraphPatternYieldItem" $
  gql "BindingVariableReference"

graphPatternYieldItemList :: TypeDefinition
graphPatternYieldItemList = def "GraphPatternYieldItemList" $
  T.union [
    "items">: nonemptyList $ gql "GraphPatternYieldItem",
    "noBindings">: T.unit]

graphReference :: TypeDefinition
graphReference = def "GraphReference" $
  T.union [
    "parentAndGraphName">: gql "ParentAndGraphName",
    "delimitedGraphName">: gql "DelimitedGraphName",
    "homeGraph">: gql "HomeGraph",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

graphReferenceValueType :: TypeDefinition
graphReferenceValueType = def "GraphReferenceValueType" $
  T.union [
    "open">: gql "OpenGraphReferenceValueType",
    "closed">: gql "ClosedGraphReferenceValueType"]

graphSource :: TypeDefinition
graphSource = def "GraphSource" $
  gql "GraphExpression"

graphTypeLikeGraph :: TypeDefinition
graphTypeLikeGraph = def "GraphTypeLikeGraph" $
  gql "GraphExpression"

graphTypeName :: TypeDefinition
graphTypeName = def "GraphTypeName" $
  T.string

graphTypeOption :: TypeDefinition
graphTypeOption = def "GraphTypeOption" $
  T.union [
    "openGraphType">: gql "OpenGraphType",
    "ofGraphType">: gql "OfGraphType"]

graphTypeReference :: TypeDefinition
graphTypeReference = def "GraphTypeReference" $
  T.union [
    "parentAndTypeName">: gql "CatalogGraphTypeParentAndName",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

graphTypeSource :: TypeDefinition
graphTypeSource = def "GraphTypeSource" $
  T.union [
    "copyOf">: gql "CopyOfGraphType",
    "likeGraph">: gql "GraphTypeLikeGraph",
    "nestedSpecification">: gql "NestedGraphTypeSpecification"]

graphTypeSpecificationBody :: TypeDefinition
graphTypeSpecificationBody = def "GraphTypeSpecificationBody" $
  gql "ElementTypeList"

graphVariableDefinition :: TypeDefinition
graphVariableDefinition = def "GraphVariableDefinition" $
  T.record [
    "variable">: gql "BindingVariable",
    "initializer">: gql "OptTypedGraphInitializer"]

groupByClause :: TypeDefinition
groupByClause = def "GroupByClause" $
  gql "GroupingElementList"

groupingElement :: TypeDefinition
groupingElement = def "GroupingElement" $
  gql "BindingVariableReference"

groupingElementList :: TypeDefinition
groupingElementList = def "GroupingElementList" $
  T.union [
    "elements">: nonemptyList $ gql "GroupingElement",
    "emptySet">: T.unit]

havingClause :: TypeDefinition
havingClause = def "HavingClause" $
  gql "SearchCondition"

homeGraph :: TypeDefinition
homeGraph = def "HomeGraph" $
  T.enum ["homePropertyGraph", "homeGraph"]

identifier :: TypeDefinition
identifier = def "Identifier" $
  T.string

immaterialValueType :: TypeDefinition
immaterialValueType = def "ImmaterialValueType" $
  T.union [
    "nullType">: gql "NullType",
    "emptyType">: gql "EmptyType"]

implies :: TypeDefinition
implies = def "Implies" $
  T.enum ["rightDoubleArrow", "implies"]

independentValueExpression :: TypeDefinition
independentValueExpression = def "IndependentValueExpression" $
  gql "NumericValueExpression"

inlineProcedureCall :: TypeDefinition
inlineProcedureCall = def "InlineProcedureCall" $
  T.record [
    "scope">: T.optional $ gql "VariableScopeClause",
    "nested">: gql "NestedProcedureSpecification"]

insertEdgeAndNode :: TypeDefinition
insertEdgeAndNode = def "InsertEdgeAndNode" $
  T.record [
    "edge">: gql "InsertEdgePattern",
    "node">: gql "InsertNodePattern"]

insertEdgePattern :: TypeDefinition
insertEdgePattern = def "InsertEdgePattern" $
  T.union [
    "pointingLeft">: gql "InsertEdgePointingLeft",
    "pointingRight">: gql "InsertEdgePointingRight",
    "undirected">: gql "InsertEdgeUndirected"]

insertEdgePointingLeft :: TypeDefinition
insertEdgePointingLeft = def "InsertEdgePointingLeft" $
  T.optional $ gql "InsertElementPatternFiller"

insertEdgePointingRight :: TypeDefinition
insertEdgePointingRight = def "InsertEdgePointingRight" $
  T.optional $ gql "InsertElementPatternFiller"

insertEdgeUndirected :: TypeDefinition
insertEdgeUndirected = def "InsertEdgeUndirected" $
  T.optional $ gql "InsertElementPatternFiller"

insertElementPatternFiller :: TypeDefinition
insertElementPatternFiller = def "InsertElementPatternFiller" $
  T.record [
    "variableDeclaration">: T.optional $ gql "ElementVariableDeclaration",
    "labelAndProperties">: T.optional $ gql "LabelAndPropertySetSpecification"]

insertGraphPattern :: TypeDefinition
insertGraphPattern = def "InsertGraphPattern" $
  gql "InsertPathPatternList"

insertNodePattern :: TypeDefinition
insertNodePattern = def "InsertNodePattern" $
  T.optional $ gql "InsertElementPatternFiller"

insertPathPattern :: TypeDefinition
insertPathPattern = def "InsertPathPattern" $
  T.record [
    "startNode">: gql "InsertNodePattern",
    "edgesAndNodes">: T.list $ gql "InsertEdgeAndNode"]

insertPathPatternList :: TypeDefinition
insertPathPatternList = def "InsertPathPatternList" $
  nonemptyList $ gql "InsertPathPattern"

insertStatement :: TypeDefinition
insertStatement = def "InsertStatement" $
  gql "InsertGraphPattern"

int128Type :: TypeDefinition
int128Type = def "Int128Type" $
  T.record ["notNull">: T.boolean]

int16Type :: TypeDefinition
int16Type = def "Int16Type" $
  T.record ["notNull">: T.boolean]

int256Type :: TypeDefinition
int256Type = def "Int256Type" $
  T.record ["notNull">: T.boolean]

int32Type :: TypeDefinition
int32Type = def "Int32Type" $
  T.record ["notNull">: T.boolean]

int64Type :: TypeDefinition
int64Type = def "Int64Type" $
  T.record ["notNull">: T.boolean]

int8Type :: TypeDefinition
int8Type = def "Int8Type" $
  T.record ["notNull">: T.boolean]

intWithPrecision :: TypeDefinition
intWithPrecision = def "IntWithPrecision" $
  T.record [
    "precision">: T.optional $ gql "Precision",
    "notNull">: T.boolean]

integer128Type :: TypeDefinition
integer128Type = def "Integer128Type" $
  T.record ["notNull">: T.boolean]

integer16Type :: TypeDefinition
integer16Type = def "Integer16Type" $
  T.record ["notNull">: T.boolean]

integer256Type :: TypeDefinition
integer256Type = def "Integer256Type" $
  T.record ["notNull">: T.boolean]

integer32Type :: TypeDefinition
integer32Type = def "Integer32Type" $
  T.record ["notNull">: T.boolean]

integer64Type :: TypeDefinition
integer64Type = def "Integer64Type" $
  T.record ["notNull">: T.boolean]

integer8Type :: TypeDefinition
integer8Type = def "Integer8Type" $
  T.record ["notNull">: T.boolean]

integerWithPrecision :: TypeDefinition
integerWithPrecision = def "IntegerWithPrecision" $
  T.record [
    "precision">: T.optional $ gql "Precision",
    "notNull">: T.boolean]

isLabelExpression :: TypeDefinition
isLabelExpression = def "IsLabelExpression" $
  T.record [
    "isOrColon">: gql "IsOrColon",
    "label">: gql "LabelExpression"]

isLabeledOrColon :: TypeDefinition
isLabeledOrColon = def "IsLabeledOrColon" $
  T.union [
    "not">: T.boolean,
    "colon">: T.unit]

isNotExpr :: TypeDefinition
isNotExpr = def "IsNotExpr" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "not">: T.boolean,
    "truthValue">: gql "TruthValue"]

isOrColon :: TypeDefinition
isOrColon = def "IsOrColon" $
  T.enum ["is", "colon"]

isOrColonWithLabels :: TypeDefinition
isOrColonWithLabels = def "IsOrColonWithLabels" $
  T.record [
    "isOrColon">: gql "IsOrColon",
    "labels">: gql "LabelSetSpecification"]

keepClause :: TypeDefinition
keepClause = def "KeepClause" $
  gql "PathPatternPrefix"

labelAndPropertySetSpecification :: TypeDefinition
labelAndPropertySetSpecification = def "LabelAndPropertySetSpecification" $
  T.record [
    "isOrColon">: T.optional $ gql "IsOrColon",
    "labelSet">: T.optional $ gql "LabelSetSpecification",
    "propertySpecification">: T.optional $ gql "ElementPropertySpecification"]

labelExpression :: TypeDefinition
labelExpression = def "LabelExpression" $
  T.union [
    "negation">: gql "LabelExpression",
    "conjunction">: gql "ConjunctionLabelExpression",
    "disjunction">: gql "DisjunctionLabelExpression",
    "name">: gql "LabelName",
    "wildcard">: T.unit,
    "parenthesized">: gql "LabelExpression"]

labelName :: TypeDefinition
labelName = def "LabelName" $
  T.string

labelSetPhrase :: TypeDefinition
labelSetPhrase = def "LabelSetPhrase" $
  T.union [
    "singleLabel">: gql "LabelName",
    "multipleLabels">: gql "LabelSetSpecification",
    "isOrColonWithLabels">: gql "IsOrColonWithLabels"]

labelSetSpecification :: TypeDefinition
labelSetSpecification = def "LabelSetSpecification" $
  nonemptyList $ gql "LabelName"

labeledPredicate :: TypeDefinition
labeledPredicate = def "LabeledPredicate" $
  T.record [
    "elementVariableReference">: gql "ElementVariableReference",
    "labeledPart">: gql "LabeledPredicatePart2"]

labeledPredicatePart2 :: TypeDefinition
labeledPredicatePart2 = def "LabeledPredicatePart2" $
  T.record [
    "isLabeledOrColon">: gql "IsLabeledOrColon",
    "labelExpression">: gql "LabelExpression"]

lengthExpression :: TypeDefinition
lengthExpression = def "LengthExpression" $
  T.union [
    "char">: gql "CharLengthExpression",
    "byte">: gql "ByteLengthExpression",
    "path">: gql "PathLengthExpression"]

letStatement :: TypeDefinition
letStatement = def "LetStatement" $
  gql "LetVariableDefinitionList"

letValueExpression :: TypeDefinition
letValueExpression = def "LetValueExpression" $
  T.record [
    "letVariables">: gql "LetVariableDefinitionList",
    "valueExpression">: gql "ValueExpression"]

letVariableDefinition :: TypeDefinition
letVariableDefinition = def "LetVariableDefinition" $
  T.union [
    "valueVariable">: gql "ValueVariableDefinition",
    "bindingEqualsValue">: gql "BindingEqualsValue"]

letVariableDefinitionList :: TypeDefinition
letVariableDefinitionList = def "LetVariableDefinitionList" $
  nonemptyList $ gql "LetVariableDefinition"

limitClause :: TypeDefinition
limitClause = def "LimitClause" $
  gql "NonNegativeIntegerSpecification"

linearCatalogModifyingStatement :: TypeDefinition
linearCatalogModifyingStatement = def "LinearCatalogModifyingStatement" $
  nonemptyList $ gql "SimpleCatalogModifyingStatement"

linearDataModifyingStatement :: TypeDefinition
linearDataModifyingStatement = def "LinearDataModifyingStatement" $
  T.union [
    "focused">: gql "FocusedLinearDataModifyingStatement",
    "ambient">: gql "AmbientLinearDataModifyingStatement"]

linearQueryStatement :: TypeDefinition
linearQueryStatement = def "LinearQueryStatement" $
  T.union [
    "focused">: gql "FocusedLinearQueryStatement",
    "ambient">: gql "AmbientLinearQueryStatement"]

listElement :: TypeDefinition
listElement = def "ListElement" $
  gql "ValueExpression"

listElementList :: TypeDefinition
listElementList = def "ListElementList" $
  nonemptyList $ gql "ListElement"

listLiteral :: TypeDefinition
listLiteral = def "ListLiteral" $
  gql "ListValueConstructorByEnumeration"

listValueConstructor :: TypeDefinition
listValueConstructor = def "ListValueConstructor" $
  gql "ListValueConstructorByEnumeration"

listValueConstructorByEnumeration :: TypeDefinition
listValueConstructorByEnumeration = def "ListValueConstructorByEnumeration" $
  T.record [
    "listValueTypeName">: T.optional $ gql "ListValueTypeName",
    "elements">: T.optional $ gql "ListElementList"]

listValueExpression :: TypeDefinition
listValueExpression = def "ListValueExpression" $
  gql "ValueExpression"

listValueFunction :: TypeDefinition
listValueFunction = def "ListValueFunction" $
  T.union [
    "trim">: gql "TrimListFunction",
    "elements">: gql "ElementsFunction"]

listValueTypeAlt1 :: TypeDefinition
listValueTypeAlt1 = def "ListValueTypeAlt1" $
  T.record [
    "typeName">: gql "ListValueTypeName",
    "valueType">: gql "ValueType",
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

listValueTypeAlt2 :: TypeDefinition
listValueTypeAlt2 = def "ListValueTypeAlt2" $
  T.record [
    "valueType">: gql "ValueType",
    "typeName">: gql "ListValueTypeName",
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

listValueTypeAlt3 :: TypeDefinition
listValueTypeAlt3 = def "ListValueTypeAlt3" $
  T.record [
    "typeName">: gql "ListValueTypeName",
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

listValueTypeName :: TypeDefinition
listValueTypeName = def "ListValueTypeName" $
  T.record [
    "group">: T.boolean,
    "synonym">: gql "ListValueTypeNameSynonym"]

listValueTypeNameSynonym :: TypeDefinition
listValueTypeNameSynonym = def "ListValueTypeNameSynonym" $
  T.enum ["list", "array"]

localDatetimeType :: TypeDefinition
localDatetimeType = def "LocalDatetimeType" $
  T.record [
    "notNull">: T.boolean]

localDatetimeTypeChoice :: TypeDefinition
localDatetimeTypeChoice = def "LocalDatetimeTypeChoice" $
  T.union [
    "localDatetime">: gql "LocalDatetimeType",
    "timestampWithoutTimeZone">: gql "TimestampWithoutTimeZoneType"]

localNodeTypeAlias :: TypeDefinition
localNodeTypeAlias = def "LocalNodeTypeAlias" $
  T.string

localTimeType :: TypeDefinition
localTimeType = def "LocalTimeType" $
  T.record [
    "notNull">: T.boolean]

localTimeTypeChoice :: TypeDefinition
localTimeTypeChoice = def "LocalTimeTypeChoice" $
  T.union [
    "localTime">: gql "LocalTimeType",
    "timeWithoutTimeZone">: gql "TimeWithoutTimeZoneType"]

localdatetimeFunction :: TypeDefinition
localdatetimeFunction = def "LocaldatetimeFunction" $
  T.union [
    "localTimestamp">: T.unit,
    "localDatetimeWithParams">: T.optional $ gql "DatetimeFunctionParameters"]

localtimeFunction :: TypeDefinition
localtimeFunction = def "LocaltimeFunction" $
  T.optional $ gql "TimeFunctionParameters"

lowerBound :: TypeDefinition
lowerBound = def "LowerBound" $
  gql "UnsignedInteger"

matchMode :: TypeDefinition
matchMode = def "MatchMode" $
  T.union [
    "repeatableElements">: gql "RepeatableElementsMatchMode",
    "differentEdges">: gql "DifferentEdgesMatchMode"]

matchStatement :: TypeDefinition
matchStatement = def "MatchStatement" $
  T.union [
    "simple">: gql "SimpleMatchStatement",
    "optional">: gql "OptionalMatchStatement"]

matchStatementBlock :: TypeDefinition
matchStatementBlock = def "MatchStatementBlock" $
  nonemptyList $ gql "MatchStatement"

maxLength :: TypeDefinition
maxLength = def "MaxLength" $
  gql "UnsignedInteger"

minLength :: TypeDefinition
minLength = def "MinLength" $
  gql "UnsignedInteger"

modulusExpression :: TypeDefinition
modulusExpression = def "ModulusExpression" $
  T.record [
    "dividend">: gql "NumericValueExpressionDividend",
    "divisor">: gql "NumericValueExpressionDivisor"]

mulDivNumericValueExpression :: TypeDefinition
mulDivNumericValueExpression = def "MulDivNumericValueExpression" $
  T.record [
    "left">: gql "NumericValueExpression",
    "operator">: gql "MultDivOperator",
    "right">: gql "NumericValueExpression"]

multDivExpr :: TypeDefinition
multDivExpr = def "MultDivExpr" $
  T.record [
    "left">: gql "ValueExpression",
    "operator">: gql "MultDivOperator",
    "right">: gql "ValueExpression"]

multDivOperator :: TypeDefinition
multDivOperator = def "MultDivOperator" $
  T.enum ["multiply", "divide"]

namedProcedureCall :: TypeDefinition
namedProcedureCall = def "NamedProcedureCall" $
  T.record [
    "reference">: gql "ProcedureReference",
    "arguments">: T.optional $ gql "ProcedureArgumentList",
    "yield">: T.optional $ gql "YieldClause"]

naturalLogarithm :: TypeDefinition
naturalLogarithm = def "NaturalLogarithm" $
  gql "NumericValueExpression"

nestedBindingTableQuerySpecification :: TypeDefinition
nestedBindingTableQuerySpecification = def "NestedBindingTableQuerySpecification" T.unit

nestedDataModifyingProcedureSpecification :: TypeDefinition
nestedDataModifyingProcedureSpecification = def "NestedDataModifyingProcedureSpecification" $
  gql "ProcedureBody"

nestedGraphTypeSpecification :: TypeDefinition
nestedGraphTypeSpecification = def "NestedGraphTypeSpecification" $
  gql "GraphTypeSpecificationBody"

nestedProcedureSpecification :: TypeDefinition
nestedProcedureSpecification = def "NestedProcedureSpecification" $
  gql "ProcedureSpecification"

nestedQuerySpecification :: TypeDefinition
nestedQuerySpecification = def "NestedQuerySpecification" $
  gql "ProcedureBody"

nextStatement :: TypeDefinition
nextStatement = def "NextStatement" $
  T.record [
    "yieldClause">: T.optional $ gql "YieldClause",
    "statement">: gql "Statement"]

nodeKeyLabelSetWithContent :: TypeDefinition
nodeKeyLabelSetWithContent = def "NodeKeyLabelSetWithContent" $
  T.record [
    "keyLabelSet">: gql "NodeTypeKeyLabelSet",
    "impliedContent">: T.optional $ gql "NodeTypeImpliedContent"]

nodeLabelSetWithProperties :: TypeDefinition
nodeLabelSetWithProperties = def "NodeLabelSetWithProperties" $
  T.record [
    "labelSet">: gql "NodeTypeLabelSet",
    "propertyTypes">: gql "NodeTypePropertyTypes"]

nodePattern :: TypeDefinition
nodePattern = def "NodePattern" $
  gql "ElementPatternFiller"

nodeReference :: TypeDefinition
nodeReference = def "NodeReference" $
  gql "ElementVariableReference"

nodeReferenceValueExpression :: TypeDefinition
nodeReferenceValueExpression = def "NodeReferenceValueExpression" $
  gql "PrimaryValueExpression"

nodeReferenceValueType :: TypeDefinition
nodeReferenceValueType = def "NodeReferenceValueType" $
  T.union [
    "open">: gql "OpenNodeReferenceValueType",
    "closed">: gql "ClosedNodeReferenceValueType"]

nodeSynonym :: TypeDefinition
nodeSynonym = def "NodeSynonym" $
  T.enum ["node", "vertex"]

nodeSynonymAndTypeName :: TypeDefinition
nodeSynonymAndTypeName = def "NodeSynonymAndTypeName" $
  T.record [
    "nodeSynonym">: gql "NodeSynonym",
    "typeName">: T.optional $ gql "NodeTypeName"]

nodeTypeFiller :: TypeDefinition
nodeTypeFiller = def "NodeTypeFiller" $
  T.union [
    "keyLabelSet">: gql "NodeKeyLabelSetWithContent",
    "impliedContent">: gql "NodeTypeImpliedContent"]

nodeTypeImpliedContent :: TypeDefinition
nodeTypeImpliedContent = def "NodeTypeImpliedContent" $
  T.union [
    "labelSet">: gql "NodeTypeLabelSet",
    "propertyTypes">: gql "NodeTypePropertyTypes",
    "labelSetWithProperties">: gql "NodeLabelSetWithProperties"]

nodeTypeKeyLabelSet :: TypeDefinition
nodeTypeKeyLabelSet = def "NodeTypeKeyLabelSet" $
  T.optional $ gql "LabelSetPhrase"

nodeTypeLabelSet :: TypeDefinition
nodeTypeLabelSet = def "NodeTypeLabelSet" $
  gql "LabelSetPhrase"

nodeTypeName :: TypeDefinition
nodeTypeName = def "NodeTypeName" $
  T.string

nodeTypeNameWithFiller :: TypeDefinition
nodeTypeNameWithFiller = def "NodeTypeNameWithFiller" $
  T.record [
    "typeName">: gql "NodeTypeName",
    "filler">: T.optional $ gql "NodeTypeFiller"]

nodeTypePattern :: TypeDefinition
nodeTypePattern = def "NodeTypePattern" $
  T.record [
    "synonymAndTypeName">: T.optional $ gql "NodeSynonymAndTypeName",
    "alias">: T.optional $ gql "LocalNodeTypeAlias",
    "filler">: T.optional $ gql "NodeTypeFiller"]

nodeTypePhrase :: TypeDefinition
nodeTypePhrase = def "NodeTypePhrase" $
  T.record [
    "synonym">: gql "NodeSynonym",
    "typePhraseFiller">: gql "NodeTypePhraseFiller",
    "alias">: T.optional $ gql "LocalNodeTypeAlias"]

nodeTypePhraseFiller :: TypeDefinition
nodeTypePhraseFiller = def "NodeTypePhraseFiller" $
  T.union [
    "typeName">: gql "NodeTypeNameWithFiller",
    "fillerOnly">: gql "NodeTypeFiller"]

nodeTypePropertyTypes :: TypeDefinition
nodeTypePropertyTypes = def "NodeTypePropertyTypes" $
  gql "PropertyTypesSpecification"

nodeTypeSpecification :: TypeDefinition
nodeTypeSpecification = def "NodeTypeSpecification" $
  T.union [
    "pattern">: gql "NodeTypePattern",
    "phrase">: gql "NodeTypePhrase"]

nonNegativeIntegerSpecification :: TypeDefinition
nonNegativeIntegerSpecification = def "NonNegativeIntegerSpecification" $
  T.union [
    "unsignedInteger">: gql "UnsignedInteger",
    "dynamicParameterSpecification">: gql "DynamicParameterSpecification"]

nonParenthesizedPrimaryValueExpression :: TypeDefinition
nonParenthesizedPrimaryValueExpression = def "NonParenthesizedPrimaryValueExpression" $
  T.union [
    "special">: gql "NonParenthesizedPrimaryValueExpressionSpecialCase",
    "bindingVariable">: gql "BindingVariableReference"]

nonParenthesizedPrimaryValueExpressionSpecialCase :: TypeDefinition
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

normalForm :: TypeDefinition
normalForm = def "NormalForm" $
  T.enum ["nfc", "nfd", "nfkc", "nfkd"]

normalizeCharacterString :: TypeDefinition
normalizeCharacterString = def "NormalizeCharacterString" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "normalForm">: T.optional $ gql "NormalForm"]

normalizedPredicateExpr :: TypeDefinition
normalizedPredicateExpr = def "NormalizedPredicateExpr" $
  T.record [
    "valueExpression">: gql "ValueExpression",
    "normalizedPredicate">: gql "NormalizedPredicatePart2"]

normalizedPredicatePart2 :: TypeDefinition
normalizedPredicatePart2 = def "NormalizedPredicatePart2" $
  T.record [
    "not">: T.boolean,
    "normalForm">: T.optional $ gql "NormalForm"]

notExpr :: TypeDefinition
notExpr = def "NotExpr" $
  gql "ValueExpression"

notNull :: TypeDefinition
notNull = def "NotNull" $
  T.unit

nullIfAbbreviation :: TypeDefinition
nullIfAbbreviation = def "NullIfAbbreviation" $
  T.record [
    "first">: gql "ValueExpression",
    "second">: gql "ValueExpression"]

nullLiteral :: TypeDefinition
nullLiteral = def "NullLiteral" $
  T.unit

nullOrdering :: TypeDefinition
nullOrdering = def "NullOrdering" $
  T.enum ["nullsFirst", "nullsLast"]

nullPredicate :: TypeDefinition
nullPredicate = def "NullPredicate" $
  T.record [
    "valueExpression">: gql "PrimaryValueExpression",
    "nullPart">: gql "NullPredicatePart2"]

nullPredicatePart2 :: TypeDefinition
nullPredicatePart2 = def "NullPredicatePart2" $
  T.record [
    "not">: T.boolean]

nullType :: TypeDefinition
nullType = def "NullType" $
  T.unit

numberOfGroups :: TypeDefinition
numberOfGroups = def "NumberOfGroups" $
  gql "NonNegativeIntegerSpecification"

numberOfPaths :: TypeDefinition
numberOfPaths = def "NumberOfPaths" $
  gql "NonNegativeIntegerSpecification"

numericType :: TypeDefinition
numericType = def "NumericType" $
  T.union [
    "exact">: gql "ExactNumericType",
    "approximate">: gql "ApproximateNumericType"]

numericValueExpression :: TypeDefinition
numericValueExpression = def "NumericValueExpression" $
  T.union [
    "signed">: gql "SignedNumericValueExpression",
    "multiplicationOrDivision">: gql "MulDivNumericValueExpression",
    "additionOrSubtraction">: gql "AddSubNumericValueExpression",
    "primary">: gql "PrimaryValueExpression",
    "function">: gql "NumericValueFunction"]

numericValueExpressionBase :: TypeDefinition
numericValueExpressionBase = def "NumericValueExpressionBase" $
  gql "NumericValueExpression"

numericValueExpressionDividend :: TypeDefinition
numericValueExpressionDividend = def "NumericValueExpressionDividend" $
  gql "NumericValueExpression"

numericValueExpressionDivisor :: TypeDefinition
numericValueExpressionDivisor = def "NumericValueExpressionDivisor" $
  gql "NumericValueExpression"

numericValueExpressionExponent :: TypeDefinition
numericValueExpressionExponent = def "NumericValueExpressionExponent" $
  gql "NumericValueExpression"

numericValueFunction :: TypeDefinition
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

objectExpressionPrimary :: TypeDefinition
objectExpressionPrimary = def "ObjectExpressionPrimary" $
  T.union [
    "variable">: gql "PrimaryValueExpression",
    "parenthesized">: gql "ParenthesizedValueExpression",
    "nonParenthesized">: gql "NonParenthesizedPrimaryValueExpressionSpecialCase"]

objectName :: TypeDefinition
objectName = def "ObjectName" $
  T.string

objectNameOrBindingVariable :: TypeDefinition
objectNameOrBindingVariable = def "ObjectNameOrBindingVariable" $
  T.string

ofGraphType :: TypeDefinition
ofGraphType = def "OfGraphType" $
  T.union [
    "likeGraph">: gql "GraphTypeLikeGraph",
    "reference">: gql "TypedGraphTypeReference",
    "nested">: gql "TypedNestedGraphTypeSpecification"]

offsetAndOptionalLimit :: TypeDefinition
offsetAndOptionalLimit = def "OffsetAndOptionalLimit" $
  T.record [
    "offset">: gql "OffsetClause",
    "limit">: T.optional $ gql "LimitClause"]

offsetClause :: TypeDefinition
offsetClause = def "OffsetClause" $
  T.record [
    "synonym">: gql "OffsetSynonym",
    "value">: gql "NonNegativeIntegerSpecification"]

offsetSynonym :: TypeDefinition
offsetSynonym = def "OffsetSynonym" $
  T.enum ["offset", "skipReservedWord"]

openDynamicUnionType :: TypeDefinition
openDynamicUnionType = def "OpenDynamicUnionType" $
  T.record [
    "value">: T.boolean,
    "notNull">: T.boolean]

openEdgeReferenceValueType :: TypeDefinition
openEdgeReferenceValueType = def "OpenEdgeReferenceValueType" $
  T.record [
    "any">: T.boolean,
    "edgeSynonym">: gql "EdgeSynonym",
    "notNull">: T.boolean]

openGraphReferenceValueType :: TypeDefinition
openGraphReferenceValueType = def "OpenGraphReferenceValueType" $
  T.record [
    "any">: T.optional T.boolean,
    "property">: T.boolean,
    "notNull">: T.boolean]

openGraphType :: TypeDefinition
openGraphType = def "OpenGraphType" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "graph">: T.boolean]

openNodeReferenceValueType :: TypeDefinition
openNodeReferenceValueType = def "OpenNodeReferenceValueType" $
  T.record [
    "any">: T.boolean,
    "nodeSynonym">: gql "NodeSynonym",
    "notNull">: T.boolean]

optTypedBindingTableInitializer :: TypeDefinition
optTypedBindingTableInitializer = def "OptTypedBindingTableInitializer" $
  T.record [
    "type">: T.optional $ gql "TypedBindingTableReferenceValueType",
    "initializer">: gql "BindingTableInitializer"]

optTypedGraphInitializer :: TypeDefinition
optTypedGraphInitializer = def "OptTypedGraphInitializer" $
  T.record [
    "type">: T.optional $ gql "TypedGraphReferenceValueType",
    "initializer">: gql "GraphInitializer"]

optTypedValueInitializer :: TypeDefinition
optTypedValueInitializer = def "OptTypedValueInitializer" $
  T.record [
    "type">: T.optional $ gql "TypedValueType",
    "initializer">: gql "ValueInitializer"]

optionalMatchStatement :: TypeDefinition
optionalMatchStatement = def "OptionalMatchStatement" $
  gql "OptionalOperand"

optionalOperand :: TypeDefinition
optionalOperand = def "OptionalOperand" $
  T.union [
    "simple">: gql "SimpleMatchStatement",
    "braceBlock">: gql "MatchStatementBlock",
    "parenBlock">: gql "MatchStatementBlock"]

orderByAndOptionalOffsetAndLimit :: TypeDefinition
orderByAndOptionalOffsetAndLimit = def "OrderByAndOptionalOffsetAndLimit" $
  T.record [
    "orderBy">: gql "OrderByClause",
    "offset">: T.optional $ gql "OffsetClause",
    "limit">: T.optional $ gql "LimitClause"]

orderByAndPageStatement :: TypeDefinition
orderByAndPageStatement = def "OrderByAndPageStatement" $
  T.union [
    "orderByAndOptionalOffsetAndLimit">: gql "OrderByAndOptionalOffsetAndLimit",
    "offsetAndOptionalLimit">: gql "OffsetAndOptionalLimit",
    "limitOnly">: gql "LimitClause"]

orderByClause :: TypeDefinition
orderByClause = def "OrderByClause" $
  gql "SortSpecificationList"

orderingSpecification :: TypeDefinition
orderingSpecification = def "OrderingSpecification" $
  T.enum ["ascending", "descending"]

ordinalityOrOffsetType :: TypeDefinition
ordinalityOrOffsetType = def "OrdinalityOrOffsetType" $
  T.enum ["ordinality", "offset"]

parameterName :: TypeDefinition
parameterName = def "ParameterName" $
  T.string

parameterSessionSpecification :: TypeDefinition
parameterSessionSpecification = def "ParameterSessionSpecification" $
  T.record [
    "parameter">: T.boolean,
    "sessionParameterSpecification">: gql "SessionParameterSpecification"]

parametersOrCharacteristics :: TypeDefinition
parametersOrCharacteristics = def "ParametersOrCharacteristics" $
  T.enum ["parameters", "characteristics"]

parentAndGraphName :: TypeDefinition
parentAndGraphName = def "ParentAndGraphName" $
  T.record [
    "parentReference">: gql "CatalogObjectParentReference",
    "graphName">: gql "GraphName"]

parentAndTableName :: TypeDefinition
parentAndTableName = def "ParentAndTableName" $
  T.record [
    "parentReference">: gql "CatalogObjectParentReference",
    "tableName">: gql "BindingTableName"]

parenthesizedPathPatternExpression :: TypeDefinition
parenthesizedPathPatternExpression = def "ParenthesizedPathPatternExpression" $
  T.record [
    "subpathDeclaration">: T.optional $ gql "SubpathVariableDeclaration",
    "pathMode">: T.optional $ gql "PathModePrefix",
    "expression">: gql "PathPatternExpression",
    "whereClause">: T.optional $ gql "ParenthesizedPathPatternWhereClause"]

parenthesizedPathPatternWhereClause :: TypeDefinition
parenthesizedPathPatternWhereClause = def "ParenthesizedPathPatternWhereClause" $
  gql "SearchCondition"

parenthesizedValueExpression :: TypeDefinition
parenthesizedValueExpression = def "ParenthesizedValueExpression" $
  gql "ValueExpression"

pathElementList :: TypeDefinition
pathElementList = def "PathElementList" $
  T.record [
    "start">: gql "PathElementListStart",
    "steps">: T.list $ gql "PathElementListStep"]

pathElementListStart :: TypeDefinition
pathElementListStart = def "PathElementListStart" $
  gql "NodeReferenceValueExpression"

pathElementListStep :: TypeDefinition
pathElementListStep = def "PathElementListStep" $
  T.record [
    "edgeReference">: gql "EdgeReferenceValueExpression",
    "nodeReference">: gql "NodeReferenceValueExpression"]

pathFactor :: TypeDefinition
pathFactor = def "PathFactor" $
  T.union [
    "primary">: gql "PathPrimary",
    "quantifiedPrimary">: gql "QuantifiedPathPrimary",
    "questionedPrimary">: gql "QuestionedPathPrimary"]

pathLengthExpression :: TypeDefinition
pathLengthExpression = def "PathLengthExpression" $
  gql "PathValueExpression"

pathMode :: TypeDefinition
pathMode = def "PathMode" $
  T.enum ["walk", "trail", "simple", "acyclic"]

pathModePrefix :: TypeDefinition
pathModePrefix = def "PathModePrefix" $
  T.record [
    "mode">: gql "PathMode",
    "orPaths">: T.optional $ gql "PathOrPaths"]

pathOrPaths :: TypeDefinition
pathOrPaths = def "PathOrPaths" $
  T.enum ["path", "paths"]

pathPattern :: TypeDefinition
pathPattern = def "PathPattern" $
  T.record [
    "variableDeclaration">: T.optional $ gql "PathVariableDeclaration",
    "prefix">: T.optional $ gql "PathPatternPrefix",
    "expression">: gql "PathPatternExpression"]

pathPatternExpression :: TypeDefinition
pathPatternExpression = def "PathPatternExpression" $
  T.union [
    "term">: gql "PathTerm",
    "multisetAlternation">: nonemptyList $ gql "PathTerm",
    "patternUnion">: nonemptyList $ gql "PathTerm"]

pathPatternList :: TypeDefinition
pathPatternList = def "PathPatternList" $
  nonemptyList $ gql "PathPattern"

pathPatternPrefix :: TypeDefinition
pathPatternPrefix = def "PathPatternPrefix" $
  T.union [
    "modePrefix">: gql "PathModePrefix",
    "searchPrefix">: gql "PathSearchPrefix"]

pathPrimary :: TypeDefinition
pathPrimary = def "PathPrimary" $
  T.union [
    "elementPattern">: gql "ElementPattern",
    "parenthesizedExpression">: gql "ParenthesizedPathPatternExpression",
    "simplifiedExpression">: gql "SimplifiedPathPatternExpression"]

pathSearchPrefix :: TypeDefinition
pathSearchPrefix = def "PathSearchPrefix" $
  T.union [
    "all">: gql "AllPathSearch",
    "any">: gql "AnyPathSearch",
    "shortest">: gql "ShortestPathSearch"]

pathTerm :: TypeDefinition
pathTerm = def "PathTerm" $
  nonemptyList $ gql "PathFactor"

pathValueConstructor :: TypeDefinition
pathValueConstructor = def "PathValueConstructor" $
  gql "PathValueConstructorByEnumeration"

pathValueConstructorByEnumeration :: TypeDefinition
pathValueConstructorByEnumeration = def "PathValueConstructorByEnumeration" $
  gql "PathElementList"

pathValueExpression :: TypeDefinition
pathValueExpression = def "PathValueExpression" $
  gql "ValueExpression"

pathValueType :: TypeDefinition
pathValueType = def "PathValueType" $
  T.record [
    "notNull">: T.boolean]

pathVariable :: TypeDefinition
pathVariable = def "PathVariable" $
  gql "BindingVariable"

pathVariableDeclaration :: TypeDefinition
pathVariableDeclaration = def "PathVariableDeclaration" $
  gql "PathVariable"

pathVariableReference :: TypeDefinition
pathVariableReference = def "PathVariableReference" $
  gql "BindingVariableReference"

powerFunction :: TypeDefinition
powerFunction = def "PowerFunction" $
  T.record [
    "base">: gql "NumericValueExpressionBase",
    "exponent">: gql "NumericValueExpressionExponent"]

precision :: TypeDefinition
precision = def "Precision" $
  gql "UnsignedDecimalInteger"

precisionAndScale :: TypeDefinition
precisionAndScale = def "PrecisionAndScale" $
  T.record [
    "precision">: gql "Precision",
    "scale">: T.optional $ gql "Scale",
    "notNull">: T.boolean]

predefinedSchemaReference :: TypeDefinition
predefinedSchemaReference = def "PredefinedSchemaReference" $
  T.enum ["homeSchema", "currentSchema", "period"]

predefinedType :: TypeDefinition
predefinedType = def "PredefinedType" $
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
  T.union [
    "createSchema">: gql "CreateSchemaStatement",
    "dropSchema">: gql "DropSchemaStatement",
    "createGraph">: gql "CreateGraphStatement",
    "dropGraph">: gql "DropGraphStatement",
    "createGraphType">: gql "CreateGraphTypeStatement",
    "dropGraphType">: gql "DropGraphTypeStatement"]

primitiveDataModifyingStatement :: TypeDefinition
primitiveDataModifyingStatement = def "PrimitiveDataModifyingStatement" $
  T.union [
    "insert">: gql "InsertStatement",
    "set">: gql "SetStatement",
    "remove">: gql "RemoveStatement",
    "delete">: gql "DeleteStatement"]

primitiveQueryStatement :: TypeDefinition
primitiveQueryStatement = def "PrimitiveQueryStatement" $
  T.union [
    "match">: gql "MatchStatement",
    "let">: gql "LetStatement",
    "for">: gql "ForStatement",
    "filter">: gql "FilterStatement",
    "orderByAndPage">: gql "OrderByAndPageStatement"]

primitiveResultStatement :: TypeDefinition
primitiveResultStatement = def "PrimitiveResultStatement" $
  T.union [
    "returnAndOptionalOrderBy">: gql "ReturnAndOptionalOrderByAndPage",
    "finish">: T.unit]

procedureAndMaybeEnd :: TypeDefinition
procedureAndMaybeEnd = def "ProcedureAndMaybeEnd" $
  T.record [
    "procedure">: gql "ProcedureSpecification",
    "end">: T.optional $ gql "EndTransactionCommand"]

procedureArgument :: TypeDefinition
procedureArgument = def "ProcedureArgument" $
  gql "ValueExpression"

procedureArgumentList :: TypeDefinition
procedureArgumentList = def "ProcedureArgumentList" $
  nonemptyList $ gql "ProcedureArgument"

procedureBody :: TypeDefinition
procedureBody = def "ProcedureBody" $
  T.record [
    "atSchema">: T.optional $ gql "AtSchemaClause",
    "bindings">: T.optional $ gql "BindingVariableDefinitionBlock",
    "statements">: gql "StatementBlock"]

procedureCall :: TypeDefinition
procedureCall = def "ProcedureCall" $
  T.union [
    "inline">: gql "InlineProcedureCall",
    "named">: gql "NamedProcedureCall"]

procedureName :: TypeDefinition
procedureName = def "ProcedureName" $
  T.string

procedureReference :: TypeDefinition
procedureReference = def "ProcedureReference" $
  T.union [
    "parentAndProcedureName">: gql "CatalogProcedureParentAndName",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

procedureSpecification :: TypeDefinition
procedureSpecification = def "ProcedureSpecification" $
  gql "ProcedureBody"

programActivity :: TypeDefinition
programActivity = def "ProgramActivity" $
  T.union [
    "session">: gql "SessionActivity",
    "transaction">: gql "TransactionActivity"]

propertyExistsPredicate :: TypeDefinition
propertyExistsPredicate = def "PropertyExistsPredicate" $
  T.record [
    "elementVariableReference">: gql "ElementVariableReference",
    "propertyName">: gql "PropertyName"]

propertyKeyValuePair :: TypeDefinition
propertyKeyValuePair = def "PropertyKeyValuePair" $
  T.record [
    "name">: gql "PropertyName",
    "value">: gql "ValueExpression"]

propertyKeyValuePairList :: TypeDefinition
propertyKeyValuePairList = def "PropertyKeyValuePairList" $
  nonemptyList $ gql "PropertyKeyValuePair"

propertyName :: TypeDefinition
propertyName = def "PropertyName" $
  T.string

propertyReference :: TypeDefinition
propertyReference = def "PropertyReference" $
  T.record [
    "valueExpression">: gql "PrimaryValueExpression",
    "propertyName">: gql "PropertyName"]

propertyType :: TypeDefinition
propertyType = def "PropertyType" $
  T.record [
    "name">: gql "PropertyName",
    "typed">: T.optional $ gql "Typed",
    "valueType">: gql "PropertyValueType"]

propertyTypeList :: TypeDefinition
propertyTypeList = def "PropertyTypeList" $
  nonemptyList $ gql "PropertyType"

propertyTypesSpecification :: TypeDefinition
propertyTypesSpecification = def "PropertyTypesSpecification" $
  T.optional $ gql "PropertyTypeList"

propertyValueType :: TypeDefinition
propertyValueType = def "PropertyValueType" $
  gql "ValueType"

quantifiedPathPrimary :: TypeDefinition
quantifiedPathPrimary = def "QuantifiedPathPrimary" $
  T.record [
    "primary">: gql "PathPrimary",
    "quantifier">: gql "GraphPatternQuantifier"]

queryConjunction :: TypeDefinition
queryConjunction = def "QueryConjunction" $
  T.union [
    "setOperator">: gql "SetOperator",
    "otherwise">: T.unit]

questionedPathPrimary :: TypeDefinition
questionedPathPrimary = def "QuestionedPathPrimary" $
  gql "PathPrimary"

realType :: TypeDefinition
realType = def "RealType" $
  T.record [
    "notNull">: T.boolean]

recordConstructor :: TypeDefinition
recordConstructor = def "RecordConstructor" $
  gql "FieldsSpecification"

recordLiteral :: TypeDefinition
recordLiteral = def "RecordLiteral" $
  gql "RecordConstructor"

recordType :: TypeDefinition
recordType = def "RecordType" $
  T.union [
    "anyRecord">: gql "AnyRecordType",
    "specifiedRecord">: gql "SpecifiedRecordType"]

referenceParameterSpecification :: TypeDefinition
referenceParameterSpecification = def "ReferenceParameterSpecification" $
  T.unit

referenceValueType :: TypeDefinition
referenceValueType = def "ReferenceValueType" $
  T.union [
    "graph">: gql "GraphReferenceValueType",
    "bindingTable">: gql "BindingTableReferenceValueType",
    "node">: gql "NodeReferenceValueType",
    "edge">: gql "EdgeReferenceValueType"]

regularIdentifier :: TypeDefinition
regularIdentifier = def "RegularIdentifier" $
  T.string

relativeCatalogSchemaReference :: TypeDefinition
relativeCatalogSchemaReference = def "RelativeCatalogSchemaReference" $
  T.union [
    "predefinedReference">: gql "PredefinedSchemaReference",
    "directoryAndSchema">: gql "RelativeDirectoryAndSchema"]

relativeDirectoryAndSchema :: TypeDefinition
relativeDirectoryAndSchema = def "RelativeDirectoryAndSchema" $
  T.record [
    "directoryPath">: gql "RelativeDirectoryPath",
    "schemaName">: gql "SchemaName"]

relativeDirectoryPath :: TypeDefinition
relativeDirectoryPath = def "RelativeDirectoryPath" $
  T.record [
    "parentDirectories">: T.nonNegativeInt32,
    "simplePath">: T.optional $ gql "SimpleDirectoryPath"]

removeItem :: TypeDefinition
removeItem = def "RemoveItem" $
  T.union [
    "property">: gql "RemovePropertyItem",
    "label">: gql "RemoveLabelItem"]

removeItemList :: TypeDefinition
removeItemList = def "RemoveItemList" $
  nonemptyList $ gql "RemoveItem"

removeLabelItem :: TypeDefinition
removeLabelItem = def "RemoveLabelItem" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "isOrColon">: gql "IsOrColon",
    "label">: gql "LabelName"]

removePropertyItem :: TypeDefinition
removePropertyItem = def "RemovePropertyItem" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "propertyName">: gql "PropertyName"]

removeStatement :: TypeDefinition
removeStatement = def "RemoveStatement" $
  gql "RemoveItemList"

repeatableElementsMatchMode :: TypeDefinition
repeatableElementsMatchMode = def "RepeatableElementsMatchMode" $
  gql "ElementBindingsOrElements"

result :: TypeDefinition
result = def "Result" $
  T.union [
    "simple">: gql "ResultExpression",
    "nullLiteral">: T.unit]

resultExpression :: TypeDefinition
resultExpression = def "ResultExpression" $
  gql "ValueExpression"

returnAndOptionalOrderByAndPage :: TypeDefinition
returnAndOptionalOrderByAndPage = def "ReturnAndOptionalOrderByAndPage" $
  T.record [
    "return">: gql "ReturnStatement",
    "orderByAndPage">: T.optional $ gql "OrderByAndPageStatement"]

returnItem :: TypeDefinition
returnItem = def "ReturnItem" $
  T.record [
    "expression">: gql "AggregatingValueExpression",
    "alias">: T.optional $ gql "ReturnItemAlias"]

returnItemAlias :: TypeDefinition
returnItemAlias = def "ReturnItemAlias" $
  T.string

returnItemList :: TypeDefinition
returnItemList = def "ReturnItemList" $
  nonemptyList $ gql "ReturnItem"

returnItems :: TypeDefinition
returnItems = def "ReturnItems" $
  T.union [
    "asterisk">: T.unit,
    "itemList">: gql "ReturnItemList"]

returnItemsAndGroupBy :: TypeDefinition
returnItemsAndGroupBy = def "ReturnItemsAndGroupBy" $
  T.record [
    "quantifier">: T.optional $ gql "SetQuantifier",
    "items">: gql "ReturnItems",
    "groupBy">: T.optional $ gql "GroupByClause"]

returnStatement :: TypeDefinition
returnStatement = def "ReturnStatement" $
  gql "ReturnStatementBody"

returnStatementBody :: TypeDefinition
returnStatementBody = def "ReturnStatementBody" $
  T.union [
    "items">: gql "ReturnItemsAndGroupBy",
    "noBindings">: T.unit]

rollbackCommand :: TypeDefinition
rollbackCommand = def "RollbackCommand" $
  T.unit

samePredicate :: TypeDefinition
samePredicate = def "SamePredicate" $
  T.record [
    "references">: nonemptyList $ gql "ElementVariableReference"]

scale :: TypeDefinition
scale = def "Scale" $
  gql "UnsignedDecimalInteger"

schemaAndObjects :: TypeDefinition
schemaAndObjects = def "SchemaAndObjects" $
  T.record [
    "schemaReference">: gql "SchemaReference",
    "objects">: T.list $ gql "ObjectName"]

schemaName :: TypeDefinition
schemaName = def "SchemaName" $
  T.string

schemaReference :: TypeDefinition
schemaReference = def "SchemaReference" $
  T.union [
    "absoluteReference">: gql "AbsoluteCatalogSchemaReference",
    "relativeReference">: gql "RelativeCatalogSchemaReference",
    "parameterSpecification">: gql "ReferenceParameterSpecification"]

searchCondition :: TypeDefinition
searchCondition = def "SearchCondition" $
  gql "BooleanValueExpression"

searchedCase :: TypeDefinition
searchedCase = def "SearchedCase" $
  T.record [
    "whenClauses">: nonemptyList $ gql "SearchedWhenClause",
    "elseClause">: T.optional $ gql "ElseClause"]

searchedWhenClause :: TypeDefinition
searchedWhenClause = def "SearchedWhenClause" $
  T.record [
    "searchCondition">: gql "SearchCondition",
    "result">: gql "Result"]

selectGraphMatch :: TypeDefinition
selectGraphMatch = def "SelectGraphMatch" $
  T.record [
    "graphExpression">: gql "GraphExpression",
    "matchStatement">: gql "MatchStatement"]

selectGraphMatchList :: TypeDefinition
selectGraphMatchList = def "SelectGraphMatchList" $
  nonemptyList $ gql "SelectGraphMatch"

selectItem :: TypeDefinition
selectItem = def "SelectItem" $
  T.record [
    "expression">: gql "AggregatingValueExpression",
    "alias">: T.optional $ gql "SelectItemAlias"]

selectItemAlias :: TypeDefinition
selectItemAlias = def "SelectItemAlias" $
  T.string

selectItemList :: TypeDefinition
selectItemList = def "SelectItemList" $
  nonemptyList $ gql "SelectItem"

selectItems :: TypeDefinition
selectItems = def "SelectItems" $
  T.union [
    "asterisk">: T.unit,
    "itemList">: gql "SelectItemList"]

selectQuerySpecification :: TypeDefinition
selectQuerySpecification = def "SelectQuerySpecification" $
  T.union [
    "nested">: gql "NestedQuerySpecification",
    "graphAndNested">: gql "GraphAndNestedQuerySpecification"]

selectStatement :: TypeDefinition
selectStatement = def "SelectStatement" $
  T.record [
    "quantifier">: T.optional $ gql "SetQuantifier",
    "items">: gql "SelectItems",
    "body">: T.optional $ gql "SelectStatementBodyAndClauses"]

selectStatementBody :: TypeDefinition
selectStatementBody = def "SelectStatementBody" $
  T.union [
    "graphMatchList">: gql "SelectGraphMatchList",
    "querySpecification">: gql "SelectQuerySpecification"]

selectStatementBodyAndClauses :: TypeDefinition
selectStatementBodyAndClauses = def "SelectStatementBodyAndClauses" $
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
  T.union [
    "reset">: nonemptyList $ gql "SessionResetCommand",
    "setAndResetCommands">: gql "SessionSetAndResetCommands"]

sessionCloseCommand :: TypeDefinition
sessionCloseCommand = def "SessionCloseCommand" $
  T.unit

sessionParameterSpecification :: TypeDefinition
sessionParameterSpecification = def "SessionParameterSpecification" $
  gql "ParameterName"

sessionResetArguments :: TypeDefinition
sessionResetArguments = def "SessionResetArguments" $
  T.union [
    "parametersOrCharacteristics">: gql "AllParametersOrCharacteristics",
    "schema">: T.unit,
    "graph">: T.unit,
    "timeZone">: T.unit,
    "parameterSessionSpecification">: gql "ParameterSessionSpecification"]

sessionResetCommand :: TypeDefinition
sessionResetCommand = def "SessionResetCommand" $
  T.optional $ gql "SessionResetArguments"

sessionSetAndResetCommands :: TypeDefinition
sessionSetAndResetCommands = def "SessionSetAndResetCommands" $
  T.record [
    "set">: nonemptyList $ gql "SessionSetCommand",
    "reset">: T.list $ gql "SessionResetCommand"]

sessionSetBindingTableParameterClause :: TypeDefinition
sessionSetBindingTableParameterClause = def "SessionSetBindingTableParameterClause" $
  T.record [
    "binding">: T.boolean,
    "param">: gql "SessionSetParameterName",
    "init">: gql "OptTypedBindingTableInitializer"]

sessionSetCommand :: TypeDefinition
sessionSetCommand = def "SessionSetCommand" $
  T.union [
    "schema">: gql "SessionSetSchemaClause",
    "graph">: gql "SessionSetGraphClause",
    "timeZone">: gql "SessionSetTimeZoneClause",
    "parameter">: gql "SessionSetParameterClause"]

sessionSetGraphClause :: TypeDefinition
sessionSetGraphClause = def "SessionSetGraphClause" $
  gql "GraphExpression"

sessionSetGraphParameterClause :: TypeDefinition
sessionSetGraphParameterClause = def "SessionSetGraphParameterClause" $
  T.record [
    "graph">: gql "SessionSetParameterName",
    "initializer">: gql "OptTypedGraphInitializer"]

sessionSetParameterClause :: TypeDefinition
sessionSetParameterClause = def "SessionSetParameterClause" $
  T.union [
    "graph">: gql "SessionSetGraphParameterClause",
    "bindings">: gql "SessionSetBindingTableParameterClause",
    "value">: gql "SessionSetValueParameterClause"]

sessionSetParameterName :: TypeDefinition
sessionSetParameterName = def "SessionSetParameterName" $
  T.record [
    "ifNotExists">: T.boolean,
    "parameter">: gql "SessionParameterSpecification"]

sessionSetSchemaClause :: TypeDefinition
sessionSetSchemaClause = def "SessionSetSchemaClause" $
  gql "SchemaReference"

sessionSetTimeZoneClause :: TypeDefinition
sessionSetTimeZoneClause = def "SessionSetTimeZoneClause" $
  gql "SetTimeZoneValue"

sessionSetValueParameterClause :: TypeDefinition
sessionSetValueParameterClause = def "SessionSetValueParameterClause" $
  T.record [
    "value">: gql "SessionSetParameterName",
    "initializer">: gql "OptTypedValueInitializer"]

setAllPropertiesItem :: TypeDefinition
setAllPropertiesItem = def "SetAllPropertiesItem" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "properties">: T.optional $ gql "PropertyKeyValuePairList"]

setItem :: TypeDefinition
setItem = def "SetItem" $
  T.union [
    "property">: gql "SetPropertyItem",
    "allProperties">: gql "SetAllPropertiesItem",
    "label">: gql "SetLabelItem"]

setItemList :: TypeDefinition
setItemList = def "SetItemList" $
  nonemptyList $ gql "SetItem"

setLabelItem :: TypeDefinition
setLabelItem = def "SetLabelItem" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "isOrColon">: gql "IsOrColon",
    "label">: gql "LabelName"]

setOperator :: TypeDefinition
setOperator = def "SetOperator" $
  T.record [
    "operatorType">: gql "SetOperatorType",
    "quantifier">: T.optional $ gql "SetQuantifier"]

setOperatorType :: TypeDefinition
setOperatorType = def "SetOperatorType" $
  T.enum ["union", "except", "intersect"]

setPropertyItem :: TypeDefinition
setPropertyItem = def "SetPropertyItem" $
  T.record [
    "variable">: gql "BindingVariableReference",
    "propertyName">: gql "PropertyName",
    "value">: gql "ValueExpression"]

setQuantifier :: TypeDefinition
setQuantifier = def "SetQuantifier" $
  T.enum ["distinct", "all"]

setStatement :: TypeDefinition
setStatement = def "SetStatement" $
  gql "SetItemList"

setTimeZoneValue :: TypeDefinition
setTimeZoneValue = def "SetTimeZoneValue" $
  gql "TimeZoneString"

shortestPathSearch :: TypeDefinition
shortestPathSearch = def "ShortestPathSearch" $
  T.union [
    "allShortest">: gql "AllShortestPathSearch",
    "anyShortest">: gql "AnyShortestPathSearch",
    "countedShortest">: gql "CountedShortestPathSearch",
    "countedShortestGroup">: gql "CountedShortestGroupSearch"]

side :: TypeDefinition
side = def "Side" $
  T.enum ["left", "right"]

sign :: TypeDefinition
sign = def "Sign" $
  T.enum ["plus", "minus"]

signedBinaryExactNumericType :: TypeDefinition
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

signedExpr :: TypeDefinition
signedExpr = def "SignedExpr" $
  T.record [
    "sign">: gql "Sign",
    "valueExpression">: gql "ValueExpression"]

signedNumericValueExpression :: TypeDefinition
signedNumericValueExpression = def "SignedNumericValueExpression" $
  T.record [
    "sign">: gql "Sign",
    "expression">: gql "NumericValueExpression"]

signedVerboseBinaryExactNumericType :: TypeDefinition
signedVerboseBinaryExactNumericType = def "SignedVerboseBinaryExactNumericType" $
  T.record [
    "signed">: T.boolean,
    "verboseType">: gql "VerboseBinaryExactNumericType"]

simpleCase :: TypeDefinition
simpleCase = def "SimpleCase" $
  T.record [
    "caseOperand">: gql "CaseOperand",
    "whenClauses">: nonemptyList $ gql "SimpleWhenClause",
    "elseClause">: T.optional $ gql "ElseClause"]

simpleCatalogModifyingStatement :: TypeDefinition
simpleCatalogModifyingStatement = def "SimpleCatalogModifyingStatement" $
  T.union [
    "primitive">: gql "PrimitiveCatalogModifyingStatement",
    "callProcedure">: gql "CallCatalogModifyingProcedureStatement"]

simpleDataAccessingStatement :: TypeDefinition
simpleDataAccessingStatement = def "SimpleDataAccessingStatement" $
  T.union [
    "query">: gql "SimpleQueryStatement",
    "modifying">: gql "SimpleDataModifyingStatement"]

simpleDataModifyingStatement :: TypeDefinition
simpleDataModifyingStatement = def "SimpleDataModifyingStatement" $
  T.union [
    "primitive">: gql "PrimitiveDataModifyingStatement",
    "callProcedure">: gql "CallDataModifyingProcedureStatement"]

simpleDirectoryPath :: TypeDefinition
simpleDirectoryPath = def "SimpleDirectoryPath" $
  nonemptyList $ gql "DirectoryName"

simpleLinearDataAccessingStatement :: TypeDefinition
simpleLinearDataAccessingStatement = def "SimpleLinearDataAccessingStatement" $
  nonemptyList $ gql "SimpleDataAccessingStatement"

simpleLinearQueryStatement :: TypeDefinition
simpleLinearQueryStatement = def "SimpleLinearQueryStatement" $
  nonemptyList $ gql "SimpleQueryStatement"

simpleMatchStatement :: TypeDefinition
simpleMatchStatement = def "SimpleMatchStatement" $
  gql "GraphPatternBindingTable"

simpleQueryStatement :: TypeDefinition
simpleQueryStatement = def "SimpleQueryStatement" $
  T.union [
    "primitive">: gql "PrimitiveQueryStatement",
    "call">: gql "CallQueryStatement"]

simpleWhenClause :: TypeDefinition
simpleWhenClause = def "SimpleWhenClause" $
  T.record [
    "whenOperands">: gql "WhenOperandList",
    "result">: gql "Result"]

simplifiedConcatenation :: TypeDefinition
simplifiedConcatenation = def "SimplifiedConcatenation" $
  T.record [
    "initialTerm">: gql "SimplifiedTerm",
    "nextFactor">: gql "SimplifiedFactorLow"]

simplifiedConjunction :: TypeDefinition
simplifiedConjunction = def "SimplifiedConjunction" $
  T.record [
    "left">: gql "SimplifiedFactorLow",
    "right">: gql "SimplifiedFactorHigh"]

simplifiedContents :: TypeDefinition
simplifiedContents = def "SimplifiedContents" $
  T.union [
    "term">: gql "SimplifiedTerm",
    "pathUnion">: gql "SimplifiedPathUnion",
    "multisetAlternation">: gql "SimplifiedMultisetAlternation"]

simplifiedDefaultingAnyDirection :: TypeDefinition
simplifiedDefaultingAnyDirection = def "SimplifiedDefaultingAnyDirection" $
  gql "SimplifiedContents"

simplifiedDefaultingLeft :: TypeDefinition
simplifiedDefaultingLeft = def "SimplifiedDefaultingLeft" $
  gql "SimplifiedContents"

simplifiedDefaultingLeftOrRight :: TypeDefinition
simplifiedDefaultingLeftOrRight = def "SimplifiedDefaultingLeftOrRight" $
  gql "SimplifiedContents"

simplifiedDefaultingLeftOrUndirected :: TypeDefinition
simplifiedDefaultingLeftOrUndirected = def "SimplifiedDefaultingLeftOrUndirected" $
  gql "SimplifiedContents"

simplifiedDefaultingRight :: TypeDefinition
simplifiedDefaultingRight = def "SimplifiedDefaultingRight" $
  gql "SimplifiedContents"

simplifiedDefaultingUndirected :: TypeDefinition
simplifiedDefaultingUndirected = def "SimplifiedDefaultingUndirected" $
  gql "SimplifiedContents"

simplifiedDefaultingUndirectedOrRight :: TypeDefinition
simplifiedDefaultingUndirectedOrRight = def "SimplifiedDefaultingUndirectedOrRight" $
  gql "SimplifiedContents"

simplifiedDirectionOverride :: TypeDefinition
simplifiedDirectionOverride = def "SimplifiedDirectionOverride" $
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
  T.union [
    "tertiary">: gql "SimplifiedTertiary",
    "quantified">: gql "SimplifiedQuantified",
    "questioned">: gql "SimplifiedQuestioned"]

simplifiedFactorLow :: TypeDefinition
simplifiedFactorLow = def "SimplifiedFactorLow" $
  T.union [
    "factorHigh">: gql "SimplifiedFactorHigh",
    "conjunction">: gql "SimplifiedConjunction"]

simplifiedMultisetAlternation :: TypeDefinition
simplifiedMultisetAlternation = def "SimplifiedMultisetAlternation" $
  nonemptyList $ gql "SimplifiedTerm"

simplifiedNegation :: TypeDefinition
simplifiedNegation = def "SimplifiedNegation" $
  gql "SimplifiedPrimary"

simplifiedOverrideAnyDirection :: TypeDefinition
simplifiedOverrideAnyDirection = def "SimplifiedOverrideAnyDirection" $
  gql "SimplifiedSecondary"

simplifiedOverrideLeft :: TypeDefinition
simplifiedOverrideLeft = def "SimplifiedOverrideLeft" $
  gql "SimplifiedSecondary"

simplifiedOverrideLeftOrRight :: TypeDefinition
simplifiedOverrideLeftOrRight = def "SimplifiedOverrideLeftOrRight" $
  gql "SimplifiedSecondary"

simplifiedOverrideLeftOrUndirected :: TypeDefinition
simplifiedOverrideLeftOrUndirected = def "SimplifiedOverrideLeftOrUndirected" $
  gql "SimplifiedSecondary"

simplifiedOverrideRight :: TypeDefinition
simplifiedOverrideRight = def "SimplifiedOverrideRight" $
  gql "SimplifiedSecondary"

simplifiedOverrideUndirected :: TypeDefinition
simplifiedOverrideUndirected = def "SimplifiedOverrideUndirected" $
  gql "SimplifiedSecondary"

simplifiedOverrideUndirectedOrRight :: TypeDefinition
simplifiedOverrideUndirectedOrRight = def "SimplifiedOverrideUndirectedOrRight" $
  gql "SimplifiedSecondary"

simplifiedPathPatternExpression :: TypeDefinition
simplifiedPathPatternExpression = def "SimplifiedPathPatternExpression" $
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
  nonemptyList $ gql "SimplifiedTerm"

simplifiedPrimary :: TypeDefinition
simplifiedPrimary = def "SimplifiedPrimary" $
  T.union [
    "labelName">: gql "LabelName",
    "parenthesizedContents">: gql "SimplifiedContents"]

simplifiedQuantified :: TypeDefinition
simplifiedQuantified = def "SimplifiedQuantified" $
  T.record [
    "tertiary">: gql "SimplifiedTertiary",
    "quantifier">: gql "GraphPatternQuantifier"]

simplifiedQuestioned :: TypeDefinition
simplifiedQuestioned = def "SimplifiedQuestioned" $
  gql "SimplifiedTertiary"

simplifiedSecondary :: TypeDefinition
simplifiedSecondary = def "SimplifiedSecondary" $
  T.union [
    "primary">: gql "SimplifiedPrimary",
    "negation">: gql "SimplifiedNegation"]

simplifiedTerm :: TypeDefinition
simplifiedTerm = def "SimplifiedTerm" $
  T.union [
    "factorLow">: gql "SimplifiedFactorLow",
    "concatenation">: gql "SimplifiedConcatenation"]

simplifiedTertiary :: TypeDefinition
simplifiedTertiary = def "SimplifiedTertiary" $
  T.union [
    "directionOverride">: gql "SimplifiedDirectionOverride",
    "secondary">: gql "SimplifiedSecondary"]

smallIntType :: TypeDefinition
smallIntType = def "SmallIntType" $
  T.record ["notNull">: T.boolean]

smallIntegerType :: TypeDefinition
smallIntegerType = def "SmallIntegerType" $
  T.record ["notNull">: T.boolean]

sortKey :: TypeDefinition
sortKey = def "SortKey" $
  gql "AggregatingValueExpression"

sortSpecification :: TypeDefinition
sortSpecification = def "SortSpecification" $
  T.record [
    "sortKey">: gql "SortKey",
    "ordering">: T.optional $ gql "OrderingSpecification",
    "nullOrdering">: T.optional $ gql "NullOrdering"]

sortSpecificationList :: TypeDefinition
sortSpecificationList = def "SortSpecificationList" $
  nonemptyList $ gql "SortSpecification"

sourceDestinationPredicate :: TypeDefinition
sourceDestinationPredicate = def "SourceDestinationPredicate" $
  T.union [
    "sourcePredicate">: gql "SourcePredicate",
    "destinationPredicate">: gql "DestinationPredicate"]

sourceNodeTypeAlias :: TypeDefinition
sourceNodeTypeAlias = def "SourceNodeTypeAlias" $
  T.string

sourceNodeTypeReference :: TypeDefinition
sourceNodeTypeReference = def "SourceNodeTypeReference" $
  T.union [
    "alias">: gql "SourceNodeTypeAlias",
    "filler">: T.optional $ gql "NodeTypeFiller"]

sourcePredicate :: TypeDefinition
sourcePredicate = def "SourcePredicate" $
  T.record [
    "not">: T.boolean,
    "sourceOf">: gql "EdgeReference"]

specifiedRecordType :: TypeDefinition
specifiedRecordType = def "SpecifiedRecordType" $
  T.record [
    "record">: T.boolean,
    "fieldTypes">: gql "FieldTypesSpecification",
    "notNull">: T.boolean]

squareRoot :: TypeDefinition
squareRoot = def "SquareRoot" $
  gql "NumericValueExpression"

startAndMaybeProcedureAndMaybeEnd :: TypeDefinition
startAndMaybeProcedureAndMaybeEnd = def "StartAndMaybeProcedureAndMaybeEnd" $
  T.record [
    "start">: gql "StartTransactionCommand",
    "procedureAndEnd">: T.optional $ gql "ProcedureAndMaybeEnd"]

startTransactionCommand :: TypeDefinition
startTransactionCommand = def "StartTransactionCommand" $
  T.optional $ gql "TransactionCharacteristics"

statement :: TypeDefinition
statement = def "Statement" $
  T.union [
    "linearCatalogModifying">: gql "LinearCatalogModifyingStatement",
    "linearDataModifying">: gql "LinearDataModifyingStatement",
    "compositeQuery">: gql "CompositeQueryStatement"]

statementBlock :: TypeDefinition
statementBlock = def "StatementBlock" $
  T.record [
    "statement">: gql "Statement",
    "nextStatements">: T.list $ gql "NextStatement"]

stringLength :: TypeDefinition
stringLength = def "StringLength" $
  gql "NumericValueExpression"

stringType :: TypeDefinition
stringType = def "StringType" $
  T.record [
    "minLength">: T.optional $ gql "MinLength",
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

subCharacterOrByteString :: TypeDefinition
subCharacterOrByteString = def "SubCharacterOrByteString" $
  T.record [
    "side">: gql "Side",
    "valueExpression">: gql "ValueExpression",
    "stringLength">: gql "StringLength"]

subpathVariable :: TypeDefinition
subpathVariable = def "SubpathVariable" $
  T.string

subpathVariableDeclaration :: TypeDefinition
subpathVariableDeclaration = def "SubpathVariableDeclaration" $
  gql "SubpathVariable"

temporalDurationQualifier :: TypeDefinition
temporalDurationQualifier = def "TemporalDurationQualifier" $
  T.enum ["yearToMonth", "dayToSecond"]

temporalDurationType :: TypeDefinition
temporalDurationType = def "TemporalDurationType" $
  T.record [
    "qualifier">: gql "TemporalDurationQualifier",
    "notNull">: T.boolean]

temporalInstantType :: TypeDefinition
temporalInstantType = def "TemporalInstantType" $
  T.union [
    "datetimeType">: gql "DatetimeType",
    "localDatetimeTypeChoice">: gql "LocalDatetimeTypeChoice",
    "dateType">: gql "DateType",
    "timeType">: gql "TimeType",
    "localTimeTypeChoice">: gql "LocalTimeTypeChoice"]

temporalLiteral :: TypeDefinition
temporalLiteral = def "TemporalLiteral" $
  T.union [
    "date">: gql "DateLiteral",
    "time">: gql "TimeLiteral",
    "datetime">: gql "DatetimeLiteral"]

temporalType :: TypeDefinition
temporalType = def "TemporalType" $
  T.union [
    "instantType">: gql "TemporalInstantType",
    "durationType">: gql "TemporalDurationType"]

timeFunction :: TypeDefinition
timeFunction = def "TimeFunction" $
  T.union [
    "currentTime">: T.unit,
    "zonedTimeWithParams">: T.optional $ gql "TimeFunctionParameters"]

timeFunctionParameters :: TypeDefinition
timeFunctionParameters = def "TimeFunctionParameters" $
  T.union [
    "timeString">: gql "TimeString",
    "recordConstructor">: gql "RecordConstructor"]

timeLiteral :: TypeDefinition
timeLiteral = def "TimeLiteral" $
  gql "TimeString"

timeString :: TypeDefinition
timeString = def "TimeString" $
  gql "CharacterStringLiteral"

timeType :: TypeDefinition
timeType = def "TimeType" $
  T.union [
    "zonedTime">: gql "ZonedTimeType",
    "timeWithTimeZone">: gql "TimeWithTimeZoneType"]

timeWithTimeZoneType :: TypeDefinition
timeWithTimeZoneType = def "TimeWithTimeZoneType" $
  T.record [
    "notNull">: T.boolean]

timeWithoutTimeZoneType :: TypeDefinition
timeWithoutTimeZoneType = def "TimeWithoutTimeZoneType" $
  T.record [
    "notNull">: T.boolean]

timeZoneString :: TypeDefinition
timeZoneString = def "TimeZoneString" $
  gql "CharacterStringLiteral"

timestampWithTimeZoneType :: TypeDefinition
timestampWithTimeZoneType = def "TimestampWithTimeZoneType" $
  T.record [
    "notNull">: T.boolean]

timestampWithoutTimeZoneType :: TypeDefinition
timestampWithoutTimeZoneType = def "TimestampWithoutTimeZoneType" $
  T.record [
    "notNull">: T.boolean]

transactionAccessMode :: TypeDefinition
transactionAccessMode = def "TransactionAccessMode" $
  T.enum [
    "readOnly",
    "readWrite"]

transactionActivity :: TypeDefinition
transactionActivity = def "TransactionActivity" $
  T.union [
    "start">: gql "StartAndMaybeProcedureAndMaybeEnd",
    "procedure">: gql "ProcedureAndMaybeEnd",
    "end">: gql "EndTransactionCommand"]

transactionCharacteristics :: TypeDefinition
transactionCharacteristics = def "TransactionCharacteristics" $
  nonemptyList $ gql "TransactionMode"

transactionMode :: TypeDefinition
transactionMode = def "TransactionMode" $
  gql "TransactionAccessMode"

trigonometricFunction :: TypeDefinition
trigonometricFunction = def "TrigonometricFunction" $
  T.record [
    "name">: gql "TrigonometricFunctionName",
    "value">: gql "NumericValueExpression"]

trigonometricFunctionName :: TypeDefinition
trigonometricFunctionName = def "TrigonometricFunctionName" $
  T.enum ["sin", "cos", "tan", "cot", "sinh", "cosh", "tanh", "asin", "acos", "atan", "degrees", "radians"]

trimCharacterOrByteString :: TypeDefinition
trimCharacterOrByteString = def "TrimCharacterOrByteString" $
  gql "ValueExpression"

trimCharacterOrByteStringSource :: TypeDefinition
trimCharacterOrByteStringSource = def "TrimCharacterOrByteStringSource" $
  gql "ValueExpression"

trimListFunction :: TypeDefinition
trimListFunction = def "TrimListFunction" $
  T.record [
    "listValue">: gql "ListValueExpression",
    "numericValue">: gql "NumericValueExpression"]

trimMultiCharacterCharacterString :: TypeDefinition
trimMultiCharacterCharacterString = def "TrimMultiCharacterCharacterString" $
  T.record [
    "trimType">: gql "TrimType",
    "valueExpression">: gql "ValueExpression",
    "optionalValueExpression">: T.optional $ gql "ValueExpression"]

trimOperands :: TypeDefinition
trimOperands = def "TrimOperands" $
  T.record [
    "specification">: T.optional $ gql "TrimSpecification",
    "characterOrByteString">: T.optional $ gql "TrimCharacterOrByteString",
    "source">: gql "TrimCharacterOrByteStringSource"]

trimSingleCharacterOrByteString :: TypeDefinition
trimSingleCharacterOrByteString = def "TrimSingleCharacterOrByteString" $
  gql "TrimOperands"

trimSpecification :: TypeDefinition
trimSpecification = def "TrimSpecification" $
  T.enum ["leading", "trailing", "both"]

trimType :: TypeDefinition
trimType = def "TrimType" $
  T.enum ["btrim", "ltrim", "rtrim"]

truthValue :: TypeDefinition
truthValue = def "TruthValue" $
  gql "BooleanLiteral"

typed :: TypeDefinition
typed = def "Typed" $
  T.unit

typedBindingTableReferenceValueType :: TypeDefinition
typedBindingTableReferenceValueType = def "TypedBindingTableReferenceValueType" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "valueType">: gql "BindingTableReferenceValueType"]

typedGraphReferenceValueType :: TypeDefinition
typedGraphReferenceValueType = def "TypedGraphReferenceValueType" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "valueType">: gql "GraphReferenceValueType"]

typedGraphTypeReference :: TypeDefinition
typedGraphTypeReference = def "TypedGraphTypeReference" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "reference">: gql "GraphTypeReference"]

typedNestedGraphTypeSpecification :: TypeDefinition
typedNestedGraphTypeSpecification = def "TypedNestedGraphTypeSpecification" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "graph">: T.boolean,
    "specification">: gql "NestedGraphTypeSpecification"]

typedValueType :: TypeDefinition
typedValueType = def "TypedValueType" $
  T.record [
    "typed">: T.optional $ gql "Typed",
    "valueType">: gql "ValueType"]

uBigIntType :: TypeDefinition
uBigIntType = def "UBigIntType" $
  T.record ["notNull">: T.boolean]

uSmallIntType :: TypeDefinition
uSmallIntType = def "USmallIntType" $
  T.record ["notNull">: T.boolean]

uint128Type :: TypeDefinition
uint128Type = def "Uint128Type" $
  T.record ["notNull">: T.boolean]

uint16Type :: TypeDefinition
uint16Type = def "Uint16Type" $
  T.record ["notNull">: T.boolean]

uint256Type :: TypeDefinition
uint256Type = def "Uint256Type" $
  T.record ["notNull">: T.boolean]

uint32Type :: TypeDefinition
uint32Type = def "Uint32Type" $
  T.record ["notNull">: T.boolean]

uint64Type :: TypeDefinition
uint64Type = def "Uint64Type" $
  T.record ["notNull">: T.boolean]

uint8Type :: TypeDefinition
uint8Type = def "Uint8Type" $
  T.record ["notNull">: T.boolean]

uintWithPrecision :: TypeDefinition
uintWithPrecision = def "UintWithPrecision" $
  T.record [
    "precision">: T.optional $ gql "Precision",
    "notNull">: T.boolean]

unsignedBinaryExactNumericType :: TypeDefinition
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

unsignedDecimalInteger :: TypeDefinition
unsignedDecimalInteger = def "UnsignedDecimalInteger" $
  T.string

unsignedInteger :: TypeDefinition
unsignedInteger = def "UnsignedInteger" $
  T.union [
    "decimal">: T.string,
    "hexadecimal">: T.string,
    "octal">: T.string,
    "binary">: T.string]

unsignedLiteral :: TypeDefinition
unsignedLiteral = def "UnsignedLiteral" $
  T.union [
    "numeric">: gql "UnsignedNumericLiteral",
    "general">: gql "GeneralLiteral"]

unsignedNumericLiteral :: TypeDefinition
unsignedNumericLiteral = def "UnsignedNumericLiteral" $
  T.union [
    "exact">: gql "ExactNumericLiteral",
    "approximate">: gql "ApproximateNumericLiteral"]

unsignedValueSpecification :: TypeDefinition
unsignedValueSpecification = def "UnsignedValueSpecification" $
  T.union [
    "unsignedLiteral">: gql "UnsignedLiteral",
    "generalValueSpecification">: gql "GeneralValueSpecification"]

upperBound :: TypeDefinition
upperBound = def "UpperBound" $
  gql "UnsignedInteger"

useGraphClause :: TypeDefinition
useGraphClause = def "UseGraphClause" $
  gql "GraphExpression"

valueExpression :: TypeDefinition
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

valueFunction :: TypeDefinition
valueFunction = def "ValueFunction" $
  T.union [
    "numeric">: gql "NumericValueFunction",
    "datetimeSubtraction">: gql "DatetimeSubtraction",
    "datetime">: gql "DatetimeValueFunction",
    "duration">: gql "DurationValueFunction",
    "characterOrByteString">: gql "CharacterOrByteStringFunction",
    "list">: gql "ListValueFunction"]

valueInitializer :: TypeDefinition
valueInitializer = def "ValueInitializer" T.unit

valueQueryExpression :: TypeDefinition
valueQueryExpression = def "ValueQueryExpression" $
  gql "NestedQuerySpecification"

valueType :: TypeDefinition
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

valueTypePredicate :: TypeDefinition
valueTypePredicate = def "ValueTypePredicate" $
  T.record [
    "valueExpression">: gql "PrimaryValueExpression",
    "valueTypePart">: gql "ValueTypePredicatePart2"]

valueTypePredicatePart2 :: TypeDefinition
valueTypePredicatePart2 = def "ValueTypePredicatePart2" $
  T.record [
    "not">: T.boolean,
    "typed">: gql "Typed",
    "valueType">: gql "ValueType"]

valueVariableDefinition :: TypeDefinition
valueVariableDefinition = def "ValueVariableDefinition" $
  T.record [
    "variable">: gql "BindingVariable",
    "initializer">: gql "OptTypedValueInitializer"]

varbinaryType :: TypeDefinition
varbinaryType = def "VarbinaryType" $
  T.record [
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

varcharType :: TypeDefinition
varcharType = def "VarcharType" $
  T.record [
    "maxLength">: T.optional $ gql "MaxLength",
    "notNull">: T.boolean]

variableScopeClause :: TypeDefinition
variableScopeClause = def "VariableScopeClause" $
  T.optional $ gql "BindingVariableReferenceList"

verboseBinaryExactNumericType :: TypeDefinition
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

whenOperand :: TypeDefinition
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

whenOperandList :: TypeDefinition
whenOperandList = def "WhenOperandList" $
  nonemptyList $ gql "WhenOperand"

whereClause :: TypeDefinition
whereClause = def "WhereClause" $
  gql "SearchCondition"

yieldClause :: TypeDefinition
yieldClause = def "YieldClause" $
  gql "YieldItemList"

yieldItem :: TypeDefinition
yieldItem = def "YieldItem" $
  T.record [
    "name">: gql "YieldItemName",
    "alias">: T.optional $ gql "YieldItemAlias"]

yieldItemAlias :: TypeDefinition
yieldItemAlias = def "YieldItemAlias" $
  gql "BindingVariable"

yieldItemList :: TypeDefinition
yieldItemList = def "YieldItemList" $
  nonemptyList $ gql "YieldItem"

yieldItemName :: TypeDefinition
yieldItemName = def "YieldItemName" $
  gql "FieldName"

zonedDatetimeType :: TypeDefinition
zonedDatetimeType = def "ZonedDatetimeType" $
  T.record [
    "notNull">: T.boolean]

zonedTimeType :: TypeDefinition
zonedTimeType = def "ZonedTimeType" $
  T.record [
    "notNull">: T.boolean]
