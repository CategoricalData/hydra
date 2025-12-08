module Hydra.Ext.Sources.Gql.PathAlgebra.Syntax where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "com.gdblab.pathAlgebra.syntax"

define :: String -> Type -> Binding
define = defineType ns

pathAlg :: String -> Type
pathAlg = typeref ns

module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just ("A syntax model for the path algebra grammar by Angles et al."
      ++ " See the paper \"Path-based Algebraic Foundations of Graph Query Languages\""
      ++ " and the ANTLR grammar at https://github.com/pathalgebra/AlgebraParser")
  where
    elements = terminals ++ nonterminals

    terminals = [
      number, text, label, variable, pathName_]

    nonterminals = [
      pathQuery, projection, partProj, groupProj, pathProj, restrictorExt,
      orderBy, groupBy, orderByOption, groupByOption, pathPattern, nodePattern,
      edgePattern, edgeDirection, rpq, plus_, star_, concatenation, alternation,
      rpqRestrictor, complexCondition, compoundComplexCondition, condition,
      compareSym, function_, simpleFunction, nestedFunction, complexFunction, boolOp]

-- Terminals from the grammar
number :: Binding
number = define "Number" T.bigint

text :: Binding
text = define "Text" T.string

label :: Binding
label = define "Label" T.string

variable :: Binding
variable = define "Variable" T.string

pathName_ :: Binding
pathName_ = define "PathName" T.string

-- Nonterminal productions from the grammar
pathQuery :: Binding
pathQuery = define "PathQuery" $ T.record [
  "projection">: pathAlg "Projection",
  "restrictorExt">: T.optional $ pathAlg "RestrictorExt",
  "pathPattern">: pathAlg "PathPattern",
  "groupBy">: T.optional $ pathAlg "GroupBy",
  "orderBy">: T.optional $ pathAlg "OrderBy"]

projection :: Binding
projection = define "Projection" $ T.record [
  "partProj">: pathAlg "PartProj",
  "groupProj">: pathAlg "GroupProj",
  "pathProj">: pathAlg "PathProj"]

partProj :: Binding
partProj = define "PartProj" $ T.union [
  "all">: T.unit,
  "limited">: pathAlg "Number"]

groupProj :: Binding
groupProj = define "GroupProj" $ T.union [
  "all">: T.unit,
  "limited">: pathAlg "Number"]

pathProj :: Binding
pathProj = define "PathProj" $ T.union [
  "all">: T.unit,
  "limited">: pathAlg "Number"]

restrictorExt :: Binding
restrictorExt = define "RestrictorExt" $ T.enum [
  "walk",
  "trail",
  "simple",
  "acyclic",
  "shortest"]

orderBy :: Binding
orderBy = define "OrderBy" $ T.wrap $ pathAlg "OrderByOption"

groupBy :: Binding
groupBy = define "GroupBy" $ T.wrap $ pathAlg "GroupByOption"

orderByOption :: Binding
orderByOption = define "OrderByOption" $ T.enum [
  "partition",
  "group",
  "path",
  "partitionGroup",
  "partitionPath",
  "groupPath",
  "partitionGroupPath"]

groupByOption :: Binding
groupByOption = define "GroupByOption" $ T.enum [
  "source",
  "target",
  "length",
  "sourceTarget",
  "sourceLength",
  "targetLength",
  "sourceTargetLength"]

pathPattern :: Binding
pathPattern = define "PathPattern" $ T.record [
  "pathName">: pathAlg "PathName",
  "startNode">: pathAlg "NodePattern",
  "edge">: pathAlg "EdgePattern",
  "endNode">: pathAlg "NodePattern",
  "condition">: T.optional $ pathAlg "ComplexCondition"]

nodePattern :: Binding
nodePattern = define "NodePattern" $ T.record [
  "variable">: T.optional $ pathAlg "Variable"]

edgePattern :: Binding
edgePattern = define "EdgePattern" $ T.record [
  "direction">: pathAlg "EdgeDirection",
  "rpq">: T.optional $ pathAlg "Rpq"]

edgeDirection :: Binding
edgeDirection = define "EdgeDirection" $ T.enum [
  "outgoing",
  "incoming",
  "undirected"]

rpq :: Binding
rpq = define "Rpq" $ T.union [
  "parenthesis">: pathAlg "Rpq",
  "label">: pathAlg "Label",
  "negated">: pathAlg "Label",
  "reverse">: pathAlg "Label",
  "optional">: pathAlg "Rpq",
  "plus">: pathAlg "Plus",
  "star">: pathAlg "Star",
  "concatenation">: pathAlg "Concatenation",
  "alternation">: pathAlg "Alternation"]

plus_ :: Binding
plus_ = define "Plus" $ T.record [
  "expression">: pathAlg "Rpq",
  "restrictor">: T.optional $ pathAlg "RpqRestrictor"]

star_ :: Binding
star_ = define "Star" $ T.record [
  "expression">: pathAlg "Rpq",
  "restrictor">: T.optional $ pathAlg "RpqRestrictor"]

concatenation :: Binding
concatenation = define "Concatenation" $ T.record [
  "left">: pathAlg "Rpq",
  "right">: pathAlg "Rpq"]

alternation :: Binding
alternation = define "Alternation" $ T.record [
  "left">: pathAlg "Rpq",
  "right">: pathAlg "Rpq"]

rpqRestrictor :: Binding
rpqRestrictor = define "RpqRestrictor" $ T.wrap $ pathAlg "RestrictorExt"

complexCondition :: Binding
complexCondition = define "ComplexCondition" $ T.union [
  "simple">: pathAlg "Condition",
  "compound">: pathAlg "CompoundComplexCondition"]

compoundComplexCondition :: Binding
compoundComplexCondition = define "CompoundComplexCondition" $ T.record [
  "lhs">: pathAlg "Condition",
  "operator">: pathAlg "BoolOp",
  "rhs">: pathAlg "ComplexCondition"]

condition :: Binding
condition = define "Condition" $ T.record [
  "function">: pathAlg "Function",
  "compareSym">: pathAlg "CompareSym",
  "value">: pathAlg "Text"]

compareSym :: Binding
compareSym = define "CompareSym" $ T.enum [
  "equal",
  "notEqual",
  "lessThan",
  "greaterThan",
  "lessThanOrEqual",
  "greaterThanOrEqual"]

function_ :: Binding
function_ = define "Function" $ T.union [
  "simple">: pathAlg "SimpleFunction",
  "nested">: pathAlg "NestedFunction",
  "complex">: pathAlg "ComplexFunction"]

simpleFunction :: Binding
simpleFunction = define "SimpleFunction" $ T.record [
  "name">: pathAlg "Text",
  "argument">: pathAlg "Text"]

nestedFunction :: Binding
nestedFunction = define "NestedFunction" $ T.record [
  "name">: pathAlg "Text",
  "innerFunction">: pathAlg "Function"]

complexFunction :: Binding
complexFunction = define "ComplexFunction" $ T.record [
  "name">: pathAlg "Text",
  "innerFunction">: pathAlg "Function",
  "additionalArg">: pathAlg "Text"]

boolOp :: Binding
boolOp = define "BoolOp" $ T.enum [
  "and",
  "or"]
