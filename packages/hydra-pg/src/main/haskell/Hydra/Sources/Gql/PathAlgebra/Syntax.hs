module Hydra.Sources.Gql.PathAlgebra.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
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
ns = ModuleName "com.gdblab.pathAlgebra.syntax"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A syntax model for the path algebra grammar by Angles et al."
      ++ " See the paper \"Path-based Algebraic Foundations of Graph Query Languages\""
      ++ " and the ANTLR grammar at https://github.com/pathalgebra/AlgebraParser"))}
  where
    definitions = terminals ++ nonterminals

    terminals = [
      number, text, label, variable, pathName_]

    nonterminals = [
      pathQuery, projection, partProj, groupProj, pathProj, restrictorExt,
      orderBy, groupBy, orderByOption, groupByOption, pathPattern, nodePattern,
      edgePattern, edgeDirection, rpq, plus_, star_, concatenation, alternation,
      rpqRestrictor, complexCondition, compoundComplexCondition, condition,
      compareSym, function_, simpleFunction, nestedFunction, complexFunction, boolOp]

alternation :: TypeDefinition
alternation = define "Alternation" $ T.record [
  "left">: pathAlg "Rpq",
  "right">: pathAlg "Rpq"]

boolOp :: TypeDefinition
boolOp = define "BoolOp" $ T.enum [
  "and",
  "or"]

compareSym :: TypeDefinition
compareSym = define "CompareSym" $ T.enum [
  "equal",
  "notEqual",
  "lessThan",
  "greaterThan",
  "lessThanOrEqual",
  "greaterThanOrEqual"]

complexCondition :: TypeDefinition
complexCondition = define "ComplexCondition" $ T.union [
  "simple">: pathAlg "Condition",
  "compound">: pathAlg "CompoundComplexCondition"]

complexFunction :: TypeDefinition
complexFunction = define "ComplexFunction" $ T.record [
  "name">: pathAlg "Text",
  "innerFunction">: pathAlg "Function",
  "additionalArg">: pathAlg "Text"]

compoundComplexCondition :: TypeDefinition
compoundComplexCondition = define "CompoundComplexCondition" $ T.record [
  "lhs">: pathAlg "Condition",
  "operator">: pathAlg "BoolOp",
  "rhs">: pathAlg "ComplexCondition"]

concatenation :: TypeDefinition
concatenation = define "Concatenation" $ T.record [
  "left">: pathAlg "Rpq",
  "right">: pathAlg "Rpq"]

condition :: TypeDefinition
condition = define "Condition" $ T.record [
  "function">: pathAlg "Function",
  "compareSym">: pathAlg "CompareSym",
  "value">: pathAlg "Text"]

edgeDirection :: TypeDefinition
edgeDirection = define "EdgeDirection" $ T.enum [
  "outgoing",
  "incoming",
  "undirected"]

edgePattern :: TypeDefinition
edgePattern = define "EdgePattern" $ T.record [
  "direction">: pathAlg "EdgeDirection",
  "rpq">: T.optional $ pathAlg "Rpq"]

function_ :: TypeDefinition
function_ = define "Function" $ T.union [
  "simple">: pathAlg "SimpleFunction",
  "nested">: pathAlg "NestedFunction",
  "complex">: pathAlg "ComplexFunction"]

groupBy :: TypeDefinition
groupBy = define "GroupBy" $ T.wrap $ pathAlg "GroupByOption"

groupByOption :: TypeDefinition
groupByOption = define "GroupByOption" $ T.enum [
  "source",
  "target",
  "length",
  "sourceTarget",
  "sourceLength",
  "targetLength",
  "sourceTargetLength"]

groupProj :: TypeDefinition
groupProj = define "GroupProj" $ T.union [
  "all">: T.unit,
  "limited">: pathAlg "Number"]

label :: TypeDefinition
label = define "Label" T.string

nestedFunction :: TypeDefinition
nestedFunction = define "NestedFunction" $ T.record [
  "name">: pathAlg "Text",
  "innerFunction">: pathAlg "Function"]

nodePattern :: TypeDefinition
nodePattern = define "NodePattern" $ T.record [
  "variable">: T.optional $ pathAlg "Variable"]

-- Terminals from the grammar
number :: TypeDefinition
number = define "Number" T.bigint

orderBy :: TypeDefinition
orderBy = define "OrderBy" $ T.wrap $ pathAlg "OrderByOption"

orderByOption :: TypeDefinition
orderByOption = define "OrderByOption" $ T.enum [
  "partition",
  "group",
  "path",
  "partitionGroup",
  "partitionPath",
  "groupPath",
  "partitionGroupPath"]

partProj :: TypeDefinition
partProj = define "PartProj" $ T.union [
  "all">: T.unit,
  "limited">: pathAlg "Number"]

pathAlg :: String -> Type
pathAlg = typeref ns

pathName_ :: TypeDefinition
pathName_ = define "PathName" T.string

pathPattern :: TypeDefinition
pathPattern = define "PathPattern" $ T.record [
  "pathName">: pathAlg "PathName",
  "startNode">: pathAlg "NodePattern",
  "edge">: pathAlg "EdgePattern",
  "endNode">: pathAlg "NodePattern",
  "condition">: T.optional $ pathAlg "ComplexCondition"]

pathProj :: TypeDefinition
pathProj = define "PathProj" $ T.union [
  "all">: T.unit,
  "limited">: pathAlg "Number"]

-- Nonterminal productions from the grammar
pathQuery :: TypeDefinition
pathQuery = define "PathQuery" $ T.record [
  "projection">: pathAlg "Projection",
  "restrictorExt">: T.optional $ pathAlg "RestrictorExt",
  "pathPattern">: pathAlg "PathPattern",
  "groupBy">: T.optional $ pathAlg "GroupBy",
  "orderBy">: T.optional $ pathAlg "OrderBy"]

plus_ :: TypeDefinition
plus_ = define "Plus" $ T.record [
  "expression">: pathAlg "Rpq",
  "restrictor">: T.optional $ pathAlg "RpqRestrictor"]

projection :: TypeDefinition
projection = define "Projection" $ T.record [
  "partProj">: pathAlg "PartProj",
  "groupProj">: pathAlg "GroupProj",
  "pathProj">: pathAlg "PathProj"]

restrictorExt :: TypeDefinition
restrictorExt = define "RestrictorExt" $ T.enum [
  "walk",
  "trail",
  "simple",
  "acyclic",
  "shortest"]

rpq :: TypeDefinition
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

rpqRestrictor :: TypeDefinition
rpqRestrictor = define "RpqRestrictor" $ T.wrap $ pathAlg "RestrictorExt"

simpleFunction :: TypeDefinition
simpleFunction = define "SimpleFunction" $ T.record [
  "name">: pathAlg "Text",
  "argument">: pathAlg "Text"]

star_ :: TypeDefinition
star_ = define "Star" $ T.record [
  "expression">: pathAlg "Rpq",
  "restrictor">: T.optional $ pathAlg "RpqRestrictor"]

text :: TypeDefinition
text = define "Text" T.string

variable :: TypeDefinition
variable = define "Variable" T.string
