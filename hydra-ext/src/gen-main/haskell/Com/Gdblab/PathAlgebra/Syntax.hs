-- | A syntax model for the path algebra grammar by Angles et al. See the paper "Path-based Algebraic Foundations of Graph Query Languages" and the ANTLR grammar at https://github.com/pathalgebra/AlgebraParser

module Com.Gdblab.PathAlgebra.Syntax where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

type Number = Integer

_Number = (Core.Name "com.gdblab.pathAlgebra.syntax.Number")

type Text = String

_Text = (Core.Name "com.gdblab.pathAlgebra.syntax.Text")

type Label = String

_Label = (Core.Name "com.gdblab.pathAlgebra.syntax.Label")

type Variable = String

_Variable = (Core.Name "com.gdblab.pathAlgebra.syntax.Variable")

type PathName = String

_PathName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathName")

data PathQuery = 
  PathQuery {
    pathQueryProjection :: Projection,
    pathQueryRestrictorExt :: (Maybe RestrictorExt),
    pathQueryPathPattern :: PathPattern,
    pathQueryGroupBy :: (Maybe GroupBy),
    pathQueryOrderBy :: (Maybe OrderBy)}
  deriving (Eq, Ord, Read, Show)

_PathQuery = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery")

_PathQuery_projection = (Core.Name "projection")

_PathQuery_restrictorExt = (Core.Name "restrictorExt")

_PathQuery_pathPattern = (Core.Name "pathPattern")

_PathQuery_groupBy = (Core.Name "groupBy")

_PathQuery_orderBy = (Core.Name "orderBy")

data Projection = 
  Projection {
    projectionPartProj :: PartProj,
    projectionGroupProj :: GroupProj,
    projectionPathProj :: PathProj}
  deriving (Eq, Ord, Read, Show)

_Projection = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection")

_Projection_partProj = (Core.Name "partProj")

_Projection_groupProj = (Core.Name "groupProj")

_Projection_pathProj = (Core.Name "pathProj")

data PartProj = 
  PartProjAll  |
  PartProjLimited Number
  deriving (Eq, Ord, Read, Show)

_PartProj = (Core.Name "com.gdblab.pathAlgebra.syntax.PartProj")

_PartProj_all = (Core.Name "all")

_PartProj_limited = (Core.Name "limited")

data GroupProj = 
  GroupProjAll  |
  GroupProjLimited Number
  deriving (Eq, Ord, Read, Show)

_GroupProj = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupProj")

_GroupProj_all = (Core.Name "all")

_GroupProj_limited = (Core.Name "limited")

data PathProj = 
  PathProjAll  |
  PathProjLimited Number
  deriving (Eq, Ord, Read, Show)

_PathProj = (Core.Name "com.gdblab.pathAlgebra.syntax.PathProj")

_PathProj_all = (Core.Name "all")

_PathProj_limited = (Core.Name "limited")

data RestrictorExt = 
  RestrictorExtWalk  |
  RestrictorExtTrail  |
  RestrictorExtSimple  |
  RestrictorExtAcyclic  |
  RestrictorExtShortest 
  deriving (Eq, Ord, Read, Show)

_RestrictorExt = (Core.Name "com.gdblab.pathAlgebra.syntax.RestrictorExt")

_RestrictorExt_walk = (Core.Name "walk")

_RestrictorExt_trail = (Core.Name "trail")

_RestrictorExt_simple = (Core.Name "simple")

_RestrictorExt_acyclic = (Core.Name "acyclic")

_RestrictorExt_shortest = (Core.Name "shortest")

newtype OrderBy = 
  OrderBy {
    unOrderBy :: OrderByOption}
  deriving (Eq, Ord, Read, Show)

_OrderBy = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderBy")

newtype GroupBy = 
  GroupBy {
    unGroupBy :: GroupByOption}
  deriving (Eq, Ord, Read, Show)

_GroupBy = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupBy")

data OrderByOption = 
  OrderByOptionPartition  |
  OrderByOptionGroup  |
  OrderByOptionPath  |
  OrderByOptionPartitionGroup  |
  OrderByOptionPartitionPath  |
  OrderByOptionGroupPath  |
  OrderByOptionPartitionGroupPath 
  deriving (Eq, Ord, Read, Show)

_OrderByOption = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption")

_OrderByOption_partition = (Core.Name "partition")

_OrderByOption_group = (Core.Name "group")

_OrderByOption_path = (Core.Name "path")

_OrderByOption_partitionGroup = (Core.Name "partitionGroup")

_OrderByOption_partitionPath = (Core.Name "partitionPath")

_OrderByOption_groupPath = (Core.Name "groupPath")

_OrderByOption_partitionGroupPath = (Core.Name "partitionGroupPath")

data GroupByOption = 
  GroupByOptionSource  |
  GroupByOptionTarget  |
  GroupByOptionLength  |
  GroupByOptionSourceTarget  |
  GroupByOptionSourceLength  |
  GroupByOptionTargetLength  |
  GroupByOptionSourceTargetLength 
  deriving (Eq, Ord, Read, Show)

_GroupByOption = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption")

_GroupByOption_source = (Core.Name "source")

_GroupByOption_target = (Core.Name "target")

_GroupByOption_length = (Core.Name "length")

_GroupByOption_sourceTarget = (Core.Name "sourceTarget")

_GroupByOption_sourceLength = (Core.Name "sourceLength")

_GroupByOption_targetLength = (Core.Name "targetLength")

_GroupByOption_sourceTargetLength = (Core.Name "sourceTargetLength")

data PathPattern = 
  PathPattern {
    pathPatternPathName :: PathName,
    pathPatternStartNode :: NodePattern,
    pathPatternEdge :: EdgePattern,
    pathPatternEndNode :: NodePattern,
    pathPatternCondition :: (Maybe ComplexCondition)}
  deriving (Eq, Ord, Read, Show)

_PathPattern = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern")

_PathPattern_pathName = (Core.Name "pathName")

_PathPattern_startNode = (Core.Name "startNode")

_PathPattern_edge = (Core.Name "edge")

_PathPattern_endNode = (Core.Name "endNode")

_PathPattern_condition = (Core.Name "condition")

data NodePattern = 
  NodePattern {
    nodePatternVariable :: (Maybe Variable)}
  deriving (Eq, Ord, Read, Show)

_NodePattern = (Core.Name "com.gdblab.pathAlgebra.syntax.NodePattern")

_NodePattern_variable = (Core.Name "variable")

data EdgePattern = 
  EdgePattern {
    edgePatternDirection :: EdgeDirection,
    edgePatternRpq :: (Maybe Rpq)}
  deriving (Eq, Ord, Read, Show)

_EdgePattern = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern")

_EdgePattern_direction = (Core.Name "direction")

_EdgePattern_rpq = (Core.Name "rpq")

data EdgeDirection = 
  EdgeDirectionOutgoing  |
  EdgeDirectionIncoming  |
  EdgeDirectionUndirected 
  deriving (Eq, Ord, Read, Show)

_EdgeDirection = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgeDirection")

_EdgeDirection_outgoing = (Core.Name "outgoing")

_EdgeDirection_incoming = (Core.Name "incoming")

_EdgeDirection_undirected = (Core.Name "undirected")

data Rpq = 
  RpqParenthesis Rpq |
  RpqLabel Label |
  RpqNegated Label |
  RpqReverse Label |
  RpqOptional Rpq |
  RpqPlus Plus |
  RpqStar Star |
  RpqConcatenation Concatenation |
  RpqAlternation Alternation
  deriving (Eq, Ord, Read, Show)

_Rpq = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq")

_Rpq_parenthesis = (Core.Name "parenthesis")

_Rpq_label = (Core.Name "label")

_Rpq_negated = (Core.Name "negated")

_Rpq_reverse = (Core.Name "reverse")

_Rpq_optional = (Core.Name "optional")

_Rpq_plus = (Core.Name "plus")

_Rpq_star = (Core.Name "star")

_Rpq_concatenation = (Core.Name "concatenation")

_Rpq_alternation = (Core.Name "alternation")

data Plus = 
  Plus {
    plusExpression :: Rpq,
    plusRestrictor :: (Maybe RpqRestrictor)}
  deriving (Eq, Ord, Read, Show)

_Plus = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus")

_Plus_expression = (Core.Name "expression")

_Plus_restrictor = (Core.Name "restrictor")

data Star = 
  Star {
    starExpression :: Rpq,
    starRestrictor :: (Maybe RpqRestrictor)}
  deriving (Eq, Ord, Read, Show)

_Star = (Core.Name "com.gdblab.pathAlgebra.syntax.Star")

_Star_expression = (Core.Name "expression")

_Star_restrictor = (Core.Name "restrictor")

data Concatenation = 
  Concatenation {
    concatenationLeft :: Rpq,
    concatenationRight :: Rpq}
  deriving (Eq, Ord, Read, Show)

_Concatenation = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation")

_Concatenation_left = (Core.Name "left")

_Concatenation_right = (Core.Name "right")

data Alternation = 
  Alternation {
    alternationLeft :: Rpq,
    alternationRight :: Rpq}
  deriving (Eq, Ord, Read, Show)

_Alternation = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation")

_Alternation_left = (Core.Name "left")

_Alternation_right = (Core.Name "right")

newtype RpqRestrictor = 
  RpqRestrictor {
    unRpqRestrictor :: RestrictorExt}
  deriving (Eq, Ord, Read, Show)

_RpqRestrictor = (Core.Name "com.gdblab.pathAlgebra.syntax.RpqRestrictor")

data ComplexCondition = 
  ComplexConditionSimple Condition |
  ComplexConditionCompound CompoundComplexCondition
  deriving (Eq, Ord, Read, Show)

_ComplexCondition = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexCondition")

_ComplexCondition_simple = (Core.Name "simple")

_ComplexCondition_compound = (Core.Name "compound")

data CompoundComplexCondition = 
  CompoundComplexCondition {
    compoundComplexConditionLhs :: Condition,
    compoundComplexConditionOperator :: BoolOp,
    compoundComplexConditionRhs :: ComplexCondition}
  deriving (Eq, Ord, Read, Show)

_CompoundComplexCondition = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition")

_CompoundComplexCondition_lhs = (Core.Name "lhs")

_CompoundComplexCondition_operator = (Core.Name "operator")

_CompoundComplexCondition_rhs = (Core.Name "rhs")

data Condition = 
  Condition {
    conditionFunction :: Function,
    conditionCompareSym :: CompareSym,
    conditionValue :: Text}
  deriving (Eq, Ord, Read, Show)

_Condition = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition")

_Condition_function = (Core.Name "function")

_Condition_compareSym = (Core.Name "compareSym")

_Condition_value = (Core.Name "value")

data CompareSym = 
  CompareSymEqual  |
  CompareSymNotEqual  |
  CompareSymLessThan  |
  CompareSymGreaterThan  |
  CompareSymLessThanOrEqual  |
  CompareSymGreaterThanOrEqual 
  deriving (Eq, Ord, Read, Show)

_CompareSym = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym")

_CompareSym_equal = (Core.Name "equal")

_CompareSym_notEqual = (Core.Name "notEqual")

_CompareSym_lessThan = (Core.Name "lessThan")

_CompareSym_greaterThan = (Core.Name "greaterThan")

_CompareSym_lessThanOrEqual = (Core.Name "lessThanOrEqual")

_CompareSym_greaterThanOrEqual = (Core.Name "greaterThanOrEqual")

data Function = 
  FunctionSimple SimpleFunction |
  FunctionNested NestedFunction |
  FunctionComplex ComplexFunction
  deriving (Eq, Ord, Read, Show)

_Function = (Core.Name "com.gdblab.pathAlgebra.syntax.Function")

_Function_simple = (Core.Name "simple")

_Function_nested = (Core.Name "nested")

_Function_complex = (Core.Name "complex")

data SimpleFunction = 
  SimpleFunction {
    simpleFunctionName :: Text,
    simpleFunctionArgument :: Text}
  deriving (Eq, Ord, Read, Show)

_SimpleFunction = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction")

_SimpleFunction_name = (Core.Name "name")

_SimpleFunction_argument = (Core.Name "argument")

data NestedFunction = 
  NestedFunction {
    nestedFunctionName :: Text,
    nestedFunctionInnerFunction :: Function}
  deriving (Eq, Ord, Read, Show)

_NestedFunction = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction")

_NestedFunction_name = (Core.Name "name")

_NestedFunction_innerFunction = (Core.Name "innerFunction")

data ComplexFunction = 
  ComplexFunction {
    complexFunctionName :: Text,
    complexFunctionInnerFunction :: Function,
    complexFunctionAdditionalArg :: Text}
  deriving (Eq, Ord, Read, Show)

_ComplexFunction = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction")

_ComplexFunction_name = (Core.Name "name")

_ComplexFunction_innerFunction = (Core.Name "innerFunction")

_ComplexFunction_additionalArg = (Core.Name "additionalArg")

data BoolOp = 
  BoolOpAnd  |
  BoolOpOr 
  deriving (Eq, Ord, Read, Show)

_BoolOp = (Core.Name "com.gdblab.pathAlgebra.syntax.BoolOp")

_BoolOp_and = (Core.Name "and")

_BoolOp_or = (Core.Name "or")
