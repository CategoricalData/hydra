module Hydra.Dsl.Mantle where

import Hydra.Kernel
import Hydra.Dsl.Phantoms
import Hydra.Mantle

import qualified Data.Map as M
import qualified Data.Maybe as Y


accessorEdge :: TTerm AccessorNode -> TTerm AccessorPath -> TTerm AccessorNode -> TTerm AccessorEdge
accessorEdge source path target = record _AccessorEdge [
  _AccessorEdge_source>>: source,
  _AccessorEdge_path>>: path,
  _AccessorEdge_target>>: target]

accessorEdgeSource = unitVariant _AccessorEdge _AccessorEdge_source
accessorEdgePath = unitVariant _AccessorEdge _AccessorEdge_path
accessorEdgeTarget = unitVariant _AccessorEdge _AccessorEdge_target

accessorGraph :: TTerm [AccessorNode] -> TTerm [AccessorEdge] -> TTerm AccessorGraph
accessorGraph nodes edges = record _AccessorGraph [
  _AccessorGraph_nodes>>: nodes,
  _AccessorGraph_edges>>: edges]

accessorGraphNodes = unitVariant _AccessorGraph _AccessorGraph_nodes
accessorGraphEdges = unitVariant _AccessorGraph _AccessorGraph_edges

accessorNode :: TTerm Name -> TTerm String -> TTerm String -> TTerm AccessorNode
accessorNode name label id = record _AccessorNode [
  _AccessorNode_name>>: name,
  _AccessorNode_label>>: label,
  _AccessorNode_id>>: id]

accessorNodeName = unitVariant _AccessorNode _AccessorNode_name
accessorNodeLabel = unitVariant _AccessorNode _AccessorNode_label
accessorNodeId = unitVariant _AccessorNode _AccessorNode_id

accessorPath :: TTerm [TermAccessor] -> TTerm AccessorPath
accessorPath path = wrap _AccessorPath path

caseConventionCamel = unitVariant _CaseConvention _CaseConvention_camel
caseConventionPascal = unitVariant _CaseConvention _CaseConvention_pascal
caseConventionLowerSnake = unitVariant _CaseConvention _CaseConvention_lowerSnake
caseConventionUpperSnake = unitVariant _CaseConvention _CaseConvention_upperSnake

eitherLeft :: TTerm a -> TTerm (Hydra.Mantle.Either a b)
eitherLeft = variant _Either _Either_left

eitherRight :: TTerm b -> TTerm (Hydra.Mantle.Either a b)
eitherRight = variant _Either _Either_right

eliminationVariant :: EliminationVariant -> TTerm EliminationVariant
eliminationVariant v = unitVariant _EliminationVariant $ case v of
  EliminationVariantProduct -> _EliminationVariant_product
  EliminationVariantRecord -> _EliminationVariant_record
  EliminationVariantUnion -> _EliminationVariant_union
  EliminationVariantWrap -> _EliminationVariant_wrap

eliminationVariantProduct :: TTerm EliminationVariant
eliminationVariantProduct = unitVariant _EliminationVariant _EliminationVariant_product

eliminationVariantRecord :: TTerm EliminationVariant
eliminationVariantRecord = unitVariant _EliminationVariant _EliminationVariant_record

eliminationVariantUnion :: TTerm EliminationVariant
eliminationVariantUnion = unitVariant _EliminationVariant _EliminationVariant_union

eliminationVariantWrap :: TTerm EliminationVariant
eliminationVariantWrap = unitVariant _EliminationVariant _EliminationVariant_wrap

functionVariant :: FunctionVariant -> TTerm FunctionVariant
functionVariant v = unitVariant _FunctionVariant $ case v of
  FunctionVariantElimination -> _FunctionVariant_elimination
  FunctionVariantLambda -> _FunctionVariant_lambda
  FunctionVariantPrimitive -> _FunctionVariant_primitive

functionVariantElimination :: TTerm FunctionVariant
functionVariantElimination = unitVariant _FunctionVariant _FunctionVariant_elimination

functionVariantLambda :: TTerm FunctionVariant
functionVariantLambda = unitVariant _FunctionVariant _FunctionVariant_lambda

functionVariantPrimitive :: TTerm FunctionVariant
functionVariantPrimitive = unitVariant _FunctionVariant _FunctionVariant_primitive

literalVariant :: LiteralVariant -> TTerm LiteralVariant
literalVariant v = unitVariant _LiteralVariant $ case v of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

literalVariantBinary :: TTerm LiteralVariant
literalVariantBinary = unitVariant _LiteralVariant _LiteralVariant_binary

literalVariantBoolean :: TTerm LiteralVariant
literalVariantBoolean = unitVariant _LiteralVariant _LiteralVariant_boolean

literalVariantFloat :: TTerm LiteralVariant
literalVariantFloat = unitVariant _LiteralVariant _LiteralVariant_float

literalVariantInteger :: TTerm LiteralVariant
literalVariantInteger = unitVariant _LiteralVariant _LiteralVariant_integer

literalVariantString :: TTerm LiteralVariant
literalVariantString = unitVariant _LiteralVariant _LiteralVariant_string

precisionArbitrary :: TTerm Precision
precisionArbitrary = unitVariant _Precision _Precision_arbitrary

precisionBits :: TTerm Int -> TTerm Precision
precisionBits = variant _Precision _Precision_bits

termAccessorAnnotatedBody :: TTerm TermAccessor
termAccessorAnnotatedBody = unitVariant _TermAccessor _TermAccessor_annotatedBody

termAccessorApplicationFunction :: TTerm TermAccessor
termAccessorApplicationFunction = unitVariant _TermAccessor _TermAccessor_applicationFunction

termAccessorApplicationArgument :: TTerm TermAccessor
termAccessorApplicationArgument = unitVariant _TermAccessor _TermAccessor_applicationArgument

termAccessorLambdaBody :: TTerm TermAccessor
termAccessorLambdaBody = unitVariant _TermAccessor _TermAccessor_lambdaBody

termAccessorUnionCasesDefault :: TTerm TermAccessor
termAccessorUnionCasesDefault = unitVariant _TermAccessor _TermAccessor_unionCasesDefault

termAccessorUnionCasesBranch :: TTerm Name -> TTerm TermAccessor
termAccessorUnionCasesBranch = variant _TermAccessor _TermAccessor_unionCasesBranch

termAccessorLetEnvironment :: TTerm TermAccessor
termAccessorLetEnvironment = unitVariant _TermAccessor _TermAccessor_letBody

termAccessorLetBinding :: TTerm Name -> TTerm TermAccessor
termAccessorLetBinding = variant _TermAccessor _TermAccessor_letBinding

termAccessorListElement :: TTerm Int -> TTerm TermAccessor
termAccessorListElement = variant _TermAccessor _TermAccessor_listElement

termAccessorMapKey :: TTerm Int -> TTerm TermAccessor
termAccessorMapKey = variant _TermAccessor _TermAccessor_mapKey

termAccessorMapValue :: TTerm Int -> TTerm TermAccessor
termAccessorMapValue = variant _TermAccessor _TermAccessor_mapValue

termAccessorOptionalTerm :: TTerm TermAccessor
termAccessorOptionalTerm = unitVariant _TermAccessor _TermAccessor_optionalTerm

termAccessorProductTerm :: TTerm Int -> TTerm TermAccessor
termAccessorProductTerm = variant _TermAccessor _TermAccessor_productTerm

termAccessorRecordField :: TTerm Name -> TTerm TermAccessor
termAccessorRecordField = variant _TermAccessor _TermAccessor_recordField

termAccessorSetElement :: TTerm Int -> TTerm TermAccessor
termAccessorSetElement = variant _TermAccessor _TermAccessor_setElement

termAccessorSumTerm :: TTerm TermAccessor
termAccessorSumTerm = unitVariant _TermAccessor _TermAccessor_sumTerm

termAccessorTypeLambdaBody :: TTerm TermAccessor
termAccessorTypeLambdaBody = unitVariant _TermAccessor _TermAccessor_typeLambdaBody

termAccessorTypeApplicationTerm :: TTerm TermAccessor
termAccessorTypeApplicationTerm = unitVariant _TermAccessor _TermAccessor_typeApplicationTerm

termAccessorInjectionTerm :: TTerm TermAccessor
termAccessorInjectionTerm = unitVariant _TermAccessor _TermAccessor_injectionTerm

termAccessorWrappedTerm :: TTerm TermAccessor
termAccessorWrappedTerm = unitVariant _TermAccessor _TermAccessor_wrappedTerm

termVariant :: TermVariant -> TTerm TermVariant
termVariant v = unitVariant _TermVariant $ case v of
  TermVariantAnnotated -> _TermVariant_annotated
  TermVariantApplication -> _TermVariant_application
  TermVariantFunction -> _TermVariant_function
  TermVariantLet -> _TermVariant_let
  TermVariantList -> _TermVariant_list
  TermVariantLiteral -> _TermVariant_literal
  TermVariantMap -> _TermVariant_map
  TermVariantOptional -> _TermVariant_optional
  TermVariantProduct -> _TermVariant_product
  TermVariantRecord -> _TermVariant_record
  TermVariantSet -> _TermVariant_set
  TermVariantSum -> _TermVariant_sum
  TermVariantTypeLambda -> _TermVariant_typeLambda
  TermVariantTypeApplication -> _TermVariant_typeApplication
  TermVariantUnion -> _TermVariant_union
  TermVariantUnit -> _TermVariant_unit
  TermVariantVariable -> _TermVariant_variable
  TermVariantWrap -> _TermVariant_wrap

termVariantAnnotated :: TTerm TermVariant
termVariantAnnotated = unitVariant _TermVariant _TermVariant_annotated

termVariantApplication :: TTerm TermVariant
termVariantApplication = unitVariant _TermVariant _TermVariant_application

termVariantFunction :: TTerm TermVariant
termVariantFunction = unitVariant _TermVariant _TermVariant_function

termVariantLet :: TTerm TermVariant
termVariantLet = unitVariant _TermVariant _TermVariant_let

termVariantList :: TTerm TermVariant
termVariantList = unitVariant _TermVariant _TermVariant_list

termVariantLiteral :: TTerm TermVariant
termVariantLiteral = unitVariant _TermVariant _TermVariant_literal

termVariantMap :: TTerm TermVariant
termVariantMap = unitVariant _TermVariant _TermVariant_map

termVariantOptional :: TTerm TermVariant
termVariantOptional = unitVariant _TermVariant _TermVariant_optional

termVariantProduct :: TTerm TermVariant
termVariantProduct = unitVariant _TermVariant _TermVariant_product

termVariantRecord :: TTerm TermVariant
termVariantRecord = unitVariant _TermVariant _TermVariant_record

termVariantSet :: TTerm TermVariant
termVariantSet = unitVariant _TermVariant _TermVariant_set

termVariantSum :: TTerm TermVariant
termVariantSum = unitVariant _TermVariant _TermVariant_sum

termVariantTypeLambda :: TTerm TermVariant
termVariantTypeLambda = unitVariant _TermVariant _TermVariant_typeLambda

termVariantTypeApplication :: TTerm TermVariant
termVariantTypeApplication = unitVariant _TermVariant _TermVariant_typeApplication

termVariantUnion :: TTerm TermVariant
termVariantUnion = unitVariant _TermVariant _TermVariant_union

termVariantUnit :: TTerm TermVariant
termVariantUnit = unitVariant _TermVariant _TermVariant_unit

termVariantVariable :: TTerm TermVariant
termVariantVariable = unitVariant _TermVariant _TermVariant_variable

termVariantWrap :: TTerm TermVariant
termVariantWrap = unitVariant _TermVariant _TermVariant_wrap

typeVariant :: TypeVariant -> TTerm TypeVariant
typeVariant v = unitVariant _TypeVariant $ case v of
  TypeVariantAnnotated -> _TypeVariant_annotated
  TypeVariantApplication -> _TypeVariant_application
  TypeVariantFunction -> _TypeVariant_function
  TypeVariantForall -> _TypeVariant_forall
  TypeVariantList -> _TypeVariant_list
  TypeVariantLiteral -> _TypeVariant_literal
  TypeVariantMap -> _TypeVariant_map
  TypeVariantOptional -> _TypeVariant_optional
  TypeVariantProduct -> _TypeVariant_product
  TypeVariantRecord -> _TypeVariant_record
  TypeVariantSet -> _TypeVariant_set
  TypeVariantUnion -> _TypeVariant_union
  TypeVariantUnit -> _TypeVariant_unit
  TypeVariantVariable -> _TypeVariant_variable
  TypeVariantWrap -> _TypeVariant_wrap

typeVariantAnnotated :: TTerm TypeVariant
typeVariantAnnotated = unitVariant _TypeVariant _TypeVariant_annotated

typeVariantApplication :: TTerm TypeVariant
typeVariantApplication = unitVariant _TypeVariant _TypeVariant_application

typeVariantFunction :: TTerm TypeVariant
typeVariantFunction = unitVariant _TypeVariant _TypeVariant_function

typeVariantForall :: TTerm TypeVariant
typeVariantForall = unitVariant _TypeVariant _TypeVariant_forall

typeVariantList :: TTerm TypeVariant
typeVariantList = unitVariant _TypeVariant _TypeVariant_list

typeVariantLiteral :: TTerm TypeVariant
typeVariantLiteral = unitVariant _TypeVariant _TypeVariant_literal

typeVariantMap :: TTerm TypeVariant
typeVariantMap = unitVariant _TypeVariant _TypeVariant_map

typeVariantOptional :: TTerm TypeVariant
typeVariantOptional = unitVariant _TypeVariant _TypeVariant_optional

typeVariantProduct :: TTerm TypeVariant
typeVariantProduct = unitVariant _TypeVariant _TypeVariant_product

typeVariantRecord :: TTerm TypeVariant
typeVariantRecord = unitVariant _TypeVariant _TypeVariant_record

typeVariantSet :: TTerm TypeVariant
typeVariantSet = unitVariant _TypeVariant _TypeVariant_set

typeVariantSum :: TTerm TypeVariant
typeVariantSum = unitVariant _TypeVariant _TypeVariant_sum

typeVariantUnion :: TTerm TypeVariant
typeVariantUnion = unitVariant _TypeVariant _TypeVariant_union

typeVariantUnit :: TTerm TypeVariant
typeVariantUnit = unitVariant _TypeVariant _TypeVariant_unit

typeVariantVariable :: TTerm TypeVariant
typeVariantVariable = unitVariant _TypeVariant _TypeVariant_variable

typeVariantWrap :: TTerm TypeVariant
typeVariantWrap = unitVariant _TypeVariant _TypeVariant_wrap

unAccessorPath :: TTerm AccessorPath -> TTerm [TermAccessor]
unAccessorPath path = unwrap _AccessorPath @@ path
